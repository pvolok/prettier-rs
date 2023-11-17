use swc_ecma_ast::{
  ArrowExpr, BlockStmtOrExpr, Expr, FnDecl, FnExpr, Function, Ident, Param,
};
use swc_ecma_visit::{
  fields::{CallExprField, NewExprField},
  AstParentKind,
};

use crate::{
  ast_printer::AstPrinter, ast_util::starts_with_no_lookahead_token, doc::Doc,
};

use super::assign::AssignmentLayout;

pub fn print_fn_decl(
  cx: &mut AstPrinter,
  fn_decl: &FnDecl,
) -> anyhow::Result<Doc> {
  print_function(cx, &fn_decl.function, Some(&fn_decl.ident))
}

pub fn print_fn_expr(
  cx: &mut AstPrinter,
  fn_expr: &FnExpr,
) -> anyhow::Result<Doc> {
  print_function(cx, &fn_expr.function, fn_expr.ident.as_ref())
}

pub fn print_arrow_expr(
  cx: &mut AstPrinter,
  arrow_expr: &ArrowExpr,
  assignment_layout: Option<AssignmentLayout>,
) -> anyhow::Result<Doc> {
  let should_print_as_chain = arrow_expr
    .body
    .as_expr()
    .map(|b| b.is_arrow())
    .unwrap_or(false);

  struct LocalVars {
    sig_docs: Vec<Doc>,
    should_break_chain: bool,
    should_print_as_chain: bool,
  }
  let mut vars = LocalVars {
    sig_docs: Vec::new(),
    should_break_chain: false,
    should_print_as_chain,
  };

  fn rec<'a>(
    cx: &mut AstPrinter,
    vars: &mut LocalVars,
    arrow_expr: &'a ArrowExpr,
  ) -> anyhow::Result<(Doc, &'a BlockStmtOrExpr)> {
    let sig_doc = print_arrow_sig(cx, arrow_expr)?;

    if vars.sig_docs.is_empty() {
      vars.sig_docs.push(sig_doc);
    } else {
      // TODO: comments
      vars.sig_docs.push(sig_doc);
    }

    if vars.should_print_as_chain {
      vars.should_break_chain = vars.should_break_chain
        || arrow_expr.params.iter().any(|p| !p.is_ident());
    }

    match arrow_expr.body.as_ref() {
      BlockStmtOrExpr::Expr(expr) => match expr.as_ref() {
        Expr::Arrow(arrow_expr) => return rec(cx, vars, arrow_expr),
        _ => (),
      },
      _ => (),
    }

    let doc = match arrow_expr.body.as_ref() {
      BlockStmtOrExpr::BlockStmt(block_stmt) => {
        cx.print_block_stmt(block_stmt, false)
      }
      BlockStmtOrExpr::Expr(expr) => cx.print_expr(&expr),
    }?;

    Ok((doc, arrow_expr.body.as_ref()))
  }
  let (body_doc, body) = rec(cx, &mut vars, arrow_expr)?;

  let should_put_body_on_same_line = true;

  let is_callee = if let Some(parent) = cx.stack.last() {
    match parent {
      AstParentKind::CallExpr(CallExprField::Callee) => true,
      AstParentKind::NewExpr(NewExprField::Callee) => true,
      _ => false,
    }
  } else {
    false
  };
  let chain_group_id = cx.group_id("arrow-chain");

  let sigs_doc = print_arrow_sigs(
    cx,
    vars.sig_docs,
    vars.should_break_chain,
    assignment_layout,
  )?;

  let mut should_break_sigs = false;
  let mut should_indent_sigs = false;
  if should_print_as_chain && (is_callee || assignment_layout.is_some()) {
    should_indent_sigs = true;
    should_break_sigs = assignment_layout
      == Some(AssignmentLayout::ChainTailArrowChain)
      || (is_callee && !should_put_body_on_same_line);
  }
  println!("is_callee: {}", is_callee);
  println!("assignment_layout: {:?}", assignment_layout);
  let sigs_doc = if should_indent_sigs {
    Doc::new_indent(Doc::new_concat(vec![Doc::softline(), sigs_doc]))
  } else {
    sigs_doc
  };

  let body_doc =
    print_arrow_body(body_doc, body, should_put_body_on_same_line)?;

  println!("should_print_as_chain: {}", should_print_as_chain);
  println!("should_indent_sigs: {}", should_indent_sigs);
  println!("should_break_sigs: {}", should_break_sigs);

  let doc = Doc::new_group(
    Doc::new_concat(vec![
      Doc::new_group(sigs_doc, should_break_sigs, None, Some(chain_group_id)),
      " =>".into(),
      if should_print_as_chain {
        Doc::new_indent_if_break(body_doc, Some(chain_group_id), false)
      } else {
        Doc::new_group(body_doc, false, None, None)
      },
      if should_print_as_chain && is_callee {
        Doc::new_if_break(Doc::softline(), "".into(), Some(chain_group_id))
      } else {
        "".into()
      },
    ]),
    false,
    None,
    None,
  );
  Ok(doc)
}

fn print_arrow_sigs(
  cx: &mut AstPrinter,
  sig_docs: Vec<Doc>,
  should_break: bool,
  assignment_layout: Option<AssignmentLayout>,
) -> anyhow::Result<Doc> {
  if sig_docs.len() == 1 {
    return Ok(sig_docs.get(0).unwrap().clone());
  }

  if assignment_layout.is_some() {
    let doc = Doc::new_group(
      Doc::new_concat(
        sig_docs
          .into_iter()
          .intersperse(Doc::new_concat(vec![" =>".into(), Doc::line()]))
          .collect::<Vec<Doc>>(),
      ),
      should_break,
      None,
      None,
    );
    return Ok(doc);
  }

  let doc = Doc::new_group(
    Doc::new_indent(Doc::new_concat(
      sig_docs
        .into_iter()
        .intersperse(Doc::new_concat(vec![" =>".into(), Doc::line()]))
        .collect::<Vec<Doc>>(),
    )),
    should_break,
    None,
    None,
  );
  Ok(doc)
}

fn print_arrow_sig(
  cx: &mut AstPrinter,
  arrow_expr: &ArrowExpr,
) -> anyhow::Result<Doc> {
  let mut parts = Vec::new();

  if arrow_expr.is_async {
    parts.push("async ".into());
  }

  let params = arrow_expr
    .params
    .iter()
    .map(|pat| Param::from(pat.clone()))
    .collect::<Vec<_>>();

  if should_print_arrow_params_without_parens(cx, arrow_expr) {
    parts.push(print_params(cx, &params)?);
  } else {
    parts.push(Doc::new_group(
      print_params(cx, &params)?,
      false,
      None,
      None,
    ));
  }

  Ok(Doc::new_concat(parts))
}

fn should_print_arrow_params_without_parens(
  cx: &mut AstPrinter,
  arrow_expr: &ArrowExpr,
) -> bool {
  false
}

fn print_arrow_body(
  body_doc: Doc,
  body: &BlockStmtOrExpr,
  should_put_body_on_same_line: bool,
) -> anyhow::Result<Doc> {
  // TODO
  let expand_last_arg = false;

  let trailing_comma = if expand_last_arg {
    Doc::new_if_break(",".into(), "".into(), None)
  } else {
    "".into()
  };

  // if the arrow function is expanded as last argument, we are adding a
  // level of indentation and need to add a softline to align the closing )
  // with the opening (, or if it's inside a JSXExpression (e.g. an attribute)
  // we should align the expression's closing } with the line with the opening {.
  let trailing_space = if expand_last_arg {
    Doc::softline()
  } else {
    "".into()
  };

  if should_put_body_on_same_line && should_add_parens_if_not_break(body) {
    let doc = Doc::new_concat(vec![
      " ".into(),
      Doc::new_group(
        Doc::new_concat(vec![
          Doc::new_if_break("".into(), "(".into(), None),
          Doc::new_indent(Doc::new_concat(vec![Doc::softline(), body_doc])),
          Doc::new_if_break("".into(), ")".into(), None),
          trailing_comma,
          trailing_space,
        ]),
        false,
        None,
        None,
      ),
    ]);
    return Ok(doc);
  }

  let body_doc = if should_always_add_parens(body) {
    Doc::new_group(
      Doc::new_concat(vec![
        "(".into(),
        Doc::new_indent(Doc::new_concat(vec![Doc::softline(), body_doc])),
        Doc::softline(),
        ")".into(),
      ]),
      false,
      None,
      None,
    )
  } else {
    body_doc
  };

  let doc = if should_put_body_on_same_line {
    Doc::new_concat(vec![" ".into(), body_doc])
  } else {
    Doc::new_concat(vec![
      Doc::new_indent(Doc::new_concat(vec![Doc::line(), body_doc])),
      trailing_comma,
      trailing_space,
    ])
  };
  Ok(doc)
}

// We handle sequence expressions as the body of arrows specially,
// so that the required parentheses end up on their own lines.
fn should_always_add_parens(body: &BlockStmtOrExpr) -> bool {
  match body {
    BlockStmtOrExpr::BlockStmt(_) => false,
    BlockStmtOrExpr::Expr(expr) => expr.is_seq(),
  }
}

// In order to avoid confusion between
// a => a ? a : a
// a <= a ? a : a
fn should_add_parens_if_not_break(body: &BlockStmtOrExpr) -> bool {
  // TODO: cache
  let expr = match body {
    BlockStmtOrExpr::BlockStmt(_) => return false,
    BlockStmtOrExpr::Expr(expr) => expr,
  };
  expr.is_cond()
    && starts_with_no_lookahead_token(expr, |expr| expr.is_object())
}

fn print_function(
  cx: &mut AstPrinter,
  function: &Function,
  ident: Option<&Ident>,
) -> anyhow::Result<Doc> {
  let mut parts = vec!["function ".into()];
  if let Some(ident) = ident {
    parts.push(ident.sym.as_str().into());
  }

  parts.push(Doc::new_group(
    print_params(cx, &function.params)?,
    false,
    None,
    None,
  ));

  if let Some(body) = &function.body {
    parts.push(" ".into());
    parts.push(cx.print_block_stmt(body, false)?);
  }

  let doc = Doc::new_concat(parts);

  Ok(doc)
}

fn print_params(cx: &mut AstPrinter, params: &[Param]) -> anyhow::Result<Doc> {
  if params.is_empty() {
    return Ok(Doc::new_concat(vec!["(".into(), ")".into()]));
  }

  let mut printed = Vec::new();
  let params_len = params.len();
  for (i, param) in params.iter().enumerate() {
    let is_last = i == params_len - 1;
    printed.push(cx.print_pat(&param.pat)?);
    if !is_last {
      printed.push(",".into());
      printed.push(Doc::line());
    }
  }

  Ok(Doc::new_concat(vec![
    "(".into(),
    Doc::new_indent(Doc::new_concat(
      [&[Doc::softline()], printed.as_slice()].concat(),
    )),
    Doc::softline(),
    ")".into(),
  ]))
}
