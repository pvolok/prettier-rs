use swc_ecma_ast::{
  BlockStmtOrExpr, CallExpr, Callee, Expr, ExprOrSpread, Lit, NewExpr, OptCall,
};

use crate::{ast_path::fake_path, ast_printer::AstPrinter, doc::Doc};

use super::{
  array::is_consicely_printed_array,
  function::{print_arrow_expr, ArrowArgs},
};

pub fn print_call_expr(
  cx: &mut AstPrinter,
  call_expr: &CallExpr,
) -> anyhow::Result<Doc> {
  let callee_doc = match &call_expr.callee {
    swc_ecma_ast::Callee::Super(_) => "super".into(),
    swc_ecma_ast::Callee::Import(_) => "import".into(),
    swc_ecma_ast::Callee::Expr(expr) => cx.print_expr(expr)?,
  };

  // TODO
  let is_tpl_lit_single_arg = false;

  fn is_test_call() -> bool {
    false
  }
  if is_tpl_lit_single_arg
    || call_expr.args.len() > 0
      && (is_common_js_or_amd_call(call_expr) || is_test_call())
  {
    let arg_docs = call_expr
      .args
      .iter()
      .map(|arg| {
        let expr_doc = cx.print_expr(&arg.expr)?;
        Ok(if arg.spread.is_some() {
          Doc::new_concat(vec!["...".into(), expr_doc])
        } else {
          expr_doc
        })
      })
      .collect::<anyhow::Result<Vec<Doc>>>()?;
    return Ok(Doc::new_concat(vec![
      callee_doc,
      "(".into(),
      Doc::new_concat(Doc::join(&", ".into(), arg_docs)),
      ")".into(),
    ]));
  }

  let args = print_call_expr_args(cx, &call_expr.args)?;

  let doc = Doc::new_concat(vec![callee_doc, args]);

  Ok(doc)
}

pub fn print_opt_call(
  cx: &mut AstPrinter,
  opt_call: &OptCall,
) -> anyhow::Result<Doc> {
  let callee_doc = cx.print_expr(&opt_call.callee)?;

  let args = print_call_expr_args(cx, &opt_call.args)?;

  let doc = Doc::new_concat(vec![callee_doc, "?.".into(), args]);

  Ok(doc)
}

fn print_call_expr_args(
  cx: &mut AstPrinter,
  args: &[ExprOrSpread],
) -> anyhow::Result<Doc> {
  let mut any_arg_empty_line = false;
  let args_len = args.len();

  let all_args_broken_out = |args: Vec<Doc>| -> Doc {
    Doc::new_group(
      Doc::new_concat(vec![
        "(".into(),
        Doc::new_indent(Doc::new_concat(
          [Doc::line()].into_iter().chain(args).collect(),
        )),
        ",".into(),
        Doc::line(),
        ")".into(),
      ]),
      true,
      None,
      None,
    )
  };

  let arg_docs = args
    .iter()
    .enumerate()
    .map(|(i, expr_or_spread)| {
      let is_last = i == args_len - 1;
      let expr_doc = cx.print_expr(&expr_or_spread.expr)?;
      let doc = if expr_or_spread.spread.is_some() {
        Doc::new_concat(vec![Doc::new_text("...".to_string()), expr_doc])
      } else {
        expr_doc
      };
      let doc = if !is_last {
        let mut parts = vec![doc, ",".into()];

        if cx.is_next_line_empty(args.iter(), i) {
          any_arg_empty_line = true;
          parts.push(Doc::hardline());
          parts.push(Doc::hardline());
        } else {
          parts.push(Doc::line());
        }
        Doc::new_concat(parts)
      } else {
        doc
      };
      Ok(doc)
    })
    .collect::<anyhow::Result<Vec<Doc>>>()?;

  if any_arg_empty_line {
    return Ok(all_args_broken_out(arg_docs));
  }

  // TODO: should expand first arg

  if should_expand_last_arg(args) {
    let head_args = &arg_docs[..args.len() - 1];
    if head_args.iter().any(|arg| arg.will_break()) {
      return Ok(all_args_broken_out(arg_docs));
    }

    let last_arg = match args.last().unwrap().expr.as_ref() {
      Expr::Arrow(arrow_expr) => print_arrow_expr(
        cx,
        fake_path(arrow_expr),
        &ArrowArgs {
          expand_last_arg: true,
          ..Default::default()
        },
      )?,
      expr => cx.print_expr(expr)?,
    };

    // TODO:
    // try {
    //   lastArg = print(getCallArgumentSelector(node, -1), {
    //     expandLastArg: true,
    //   });
    // } catch (caught) {
    //   if (caught instanceof ArgExpansionBailout) {
    //     return allArgsBrokenOut();
    //   }
    //   /* c8 ignore next */
    //   throw caught;
    // }

    if last_arg.will_break() {
      return Ok(Doc::new_concat(vec![
        Doc::break_parent(),
        Doc::conditional_group(vec![
          Doc::new_concat(vec![
            "(".into(),
            Doc::new_concat(head_args.to_vec()),
            Doc::new_group(last_arg, true, None, None),
            ")".into(),
          ]),
          all_args_broken_out(arg_docs),
        ]),
      ]));
    }

    todo!()
  }

  let doc = Doc::new_group(
    Doc::new_concat(vec![
      "(".into(),
      Doc::new_indent(Doc::new_concat(
        [Doc::softline()].into_iter().chain(arg_docs).collect(),
      )),
      Doc::new_if_break(",".into(), "".into(), None),
      Doc::softline(),
      ")".into(),
    ])
    .into(),
    false,
    None,
    None,
  );

  Ok(doc)
}

pub fn print_new_expr(
  cx: &mut AstPrinter,
  new_expr: &NewExpr,
) -> anyhow::Result<Doc> {
  let callee_doc = cx.print_expr(&new_expr.callee)?;

  let args = if let Some(args) = &new_expr.args {
    print_call_expr_args(cx, args)?
  } else {
    print_call_expr_args(cx, &[])?
  };

  let doc = Doc::new_concat(vec!["new ".into(), callee_doc, args]);

  Ok(doc)
}

fn should_expand_last_arg(args: &[ExprOrSpread]) -> bool {
  let last_arg = if let Some(last_arg) = args.last() {
    last_arg
  } else {
    return false;
  };

  // TODO: comments

  if !could_expand_arg(&last_arg.expr, false) {
    return false;
  }
  // If the last two arguments are of the same type,
  // disable last element expansion.
  if args.len() >= 2 {
    // Safety: args.len() >= 2
    let penultimate = args.get(args.len() - 2).unwrap();
    match (penultimate.expr.as_ref(), &last_arg.expr.as_ref()) {
      (Expr::Object(_), Expr::Object(_)) => return false,
      (Expr::Array(_), Expr::Array(_)) => return false,
      (Expr::Fn(_), Expr::Fn(_)) => return false,
      (Expr::Arrow(_), Expr::Arrow(_)) => return false,
      _ => (),
    }
  }

  // useMemo(() => func(), [foo, bar, baz])
  if args.len() == 2
    && args.get(0).unwrap().expr.is_arrow()
    && last_arg.expr.is_array()
  {
    return false;
  }

  if args.len() > 1
    && last_arg
      .expr
      .as_array()
      .map(|arr| is_consicely_printed_array(&arr.elems))
      .unwrap_or(false)
  {
    return false;
  }

  return true;
}

fn could_expand_arg(arg: &Expr, arrow_chain_recursion: bool) -> bool {
  match arg {
    Expr::Object(object_lit) => {
      object_lit.props.len() > 1 || /* has_comment(arg) */ false
    }
    Expr::Array(array_lit) => {
      array_lit.elems.len() > 1 || /* has_comment(arg) */ false
    }
    Expr::Fn(_) => true,
    Expr::Arrow(arrow_expr) => match arrow_expr.body.as_ref() {
      BlockStmtOrExpr::BlockStmt(_) => true,
      BlockStmtOrExpr::Expr(body_expr) => match body_expr.as_ref() {
        Expr::Arrow(_) => arrow_expr
          .body
          .as_expr()
          .map_or(false, |expr| could_expand_arg(&expr, true)),
        Expr::Array(_) => true,
        Expr::Object(_) => true,
        Expr::Cond(_) => !arrow_chain_recursion,
        Expr::Call(_) => !arrow_chain_recursion,
        Expr::OptChain(opt_chain) => {
          opt_chain.base.is_call() && !arrow_chain_recursion
        }
        Expr::JSXElement(_) | Expr::JSXFragment(_) => true,
        _ => false,
      },
    },
    _ => false,
  }
}

fn is_common_js_or_amd_call(call_expr: &CallExpr) -> bool {
  match &call_expr.callee {
    Callee::Expr(expr) => match expr.as_ref() {
      Expr::Ident(ident) => {
        if ident.sym == "require" {
          return call_expr.args.len() == 1
            && call_expr.args.first().map_or(false, |a| {
              a.expr.as_lit().map_or(false, |l| matches!(l, Lit::Str(_)))
            })
            || call_expr.args.len() > 1;
        } else if ident.sym == "define" {
          todo!()
        }
      }
      _ => (),
    },
    _ => (),
  }

  false
}
