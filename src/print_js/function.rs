use std::ops::Range;

use swc_common::{BytePos, Span, Spanned};
use swc_ecma_ast::{
  ArrowExpr, BlockStmtOrExpr, Expr, FnDecl, FnExpr, Function, Ident, Param,
  ReturnStmt, ThrowStmt,
};
use swc_ecma_visit::{
  fields::{
    ArrowExprField, BlockStmtOrExprField, CallExprField, ExprField,
    NewExprField, ReturnStmtField, ThrowStmtField,
  },
  AstParentKind,
};

use crate::{
  ast_path::{fake_path, sub, sub_box, var, ARef, Path},
  ast_printer::{AstPrinter, Comma, SrcItem},
  ast_util::starts_with_no_lookahead_token,
  doc::Doc,
  print_js::comments::print_leading_comments,
};

use super::{assign::AssignmentLayout, comments::print_trailing_comments};

pub fn print_fn_decl(
  cx: &mut AstPrinter,
  fn_decl: &FnDecl,
) -> anyhow::Result<Doc> {
  let name_doc = cx.print_ident(&fn_decl.ident);
  print_function(cx, &fn_decl.function, Some(name_doc))
}

pub fn print_fn_expr(
  cx: &mut AstPrinter,
  fn_expr: &FnExpr,
) -> anyhow::Result<Doc> {
  let name_doc = fn_expr.ident.as_ref().map(|ident| cx.print_ident(ident));
  print_function(cx, &fn_expr.function, name_doc)
}

#[derive(Clone, Debug, Default)]
pub struct ArrowArgs {
  pub assignment_layout: Option<AssignmentLayout>,
  pub expand_first_arg: bool,
  pub expand_last_arg: bool,
}

pub fn print_arrow_expr(
  cx: &mut AstPrinter,
  arrow_expr: Path<ArrowExpr>,
  args: &ArrowArgs,
) -> anyhow::Result<Doc> {
  let should_print_as_chain = arrow_expr
    .node
    .body
    .as_expr()
    .map(|b| b.is_arrow())
    .unwrap_or(false);

  struct LocalVars {
    sig_docs: Vec<Doc>,
    should_break_chain: bool,
    should_print_as_chain: bool,
    should_put_body_on_same_line: bool,
  }
  let mut vars = LocalVars {
    sig_docs: Vec::new(),
    should_break_chain: false,
    should_print_as_chain,
    should_put_body_on_same_line: true,
  };

  fn rec<'a>(
    cx: &mut AstPrinter,
    vars: &mut LocalVars,
    arrow_expr: Path<'a, ArrowExpr>,
  ) -> anyhow::Result<Doc> {
    let sig_doc = print_arrow_sig(cx, arrow_expr.node)?;

    if vars.sig_docs.is_empty() {
      vars.sig_docs.push(sig_doc);
    } else {
      // TODO: comments
      vars.sig_docs.push(sig_doc);
    }

    if vars.should_print_as_chain {
      vars.should_break_chain = vars.should_break_chain
        || arrow_expr.node.params.iter().any(|p| !p.is_ident());
    }

    let body = sub_box!(arrow_expr, ArrowExpr, body, Body);

    match body.node {
      BlockStmtOrExpr::Expr(expr) => {
        let expr = var!(body, BlockStmtOrExpr, expr.as_ref(), Expr);
        match expr.node {
          Expr::Arrow(arrow_expr) => {
            let arrow_expr = var!(expr, Expr, arrow_expr, Arrow);
            return rec(cx, vars, arrow_expr);
          }
          _ => (),
        }
      }
      _ => (),
    }

    let doc = match body.node {
      BlockStmtOrExpr::BlockStmt(block_stmt) => {
        let block_stmt = var!(body, BlockStmtOrExpr, block_stmt, BlockStmt);
        cx.print_block_stmt(block_stmt.node, false)?
      }
      BlockStmtOrExpr::Expr(expr) => {
        let expr = var!(body, BlockStmtOrExpr, expr.as_ref(), Expr);
        cx.print_expr_path(expr)?
      }
    };

    let doc =
      print_arrow_body(doc, body.node, vars.should_put_body_on_same_line)?;

    Ok(doc)
  }
  let body_doc = rec(cx, &mut vars, arrow_expr)?;

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

  let sigs_doc =
    print_arrow_sigs(cx, vars.sig_docs, vars.should_break_chain, args)?;

  let mut should_break_sigs = false;
  let mut should_indent_sigs = false;
  if should_print_as_chain && (is_callee || args.assignment_layout.is_some()) {
    should_indent_sigs = true;
    should_break_sigs = args.assignment_layout
      == Some(AssignmentLayout::ChainTailArrowChain)
      || (is_callee && !vars.should_put_body_on_same_line);
  }
  let sigs_doc = if should_indent_sigs {
    Doc::new_indent(Doc::new_concat(vec![Doc::softline(), sigs_doc]))
  } else {
    sigs_doc
  };

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
  args: &ArrowArgs,
) -> anyhow::Result<Doc> {
  if sig_docs.len() == 1 {
    return Ok(sig_docs.get(0).unwrap().clone());
  }

  if args.assignment_layout.is_some() {
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

  let params_range = (arrow_expr.span_lo(), arrow_expr.body.span_lo());

  if should_print_arrow_params_without_parens(cx, arrow_expr) {
    parts.push(print_params(cx, params_range, &params)?);
  } else {
    parts.push(Doc::new_group(
      print_params(cx, params_range, &params)?,
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
    && !starts_with_no_lookahead_token(expr, |expr| expr.is_object())
}

fn print_function(
  cx: &mut AstPrinter,
  function: &Function,
  ident: Option<Doc>,
) -> anyhow::Result<Doc> {
  let mut parts = vec!["function ".into()];
  if let Some(ident) = ident {
    parts.push(ident);
  }

  let open_brace_pos = cx
    .iter_ascii_chars(function.span_lo())
    .find_map(|c| match c {
      SrcItem::Ascii(c, pos) if c == '(' => Some(pos),
      _ => None,
    })
    .ok_or_else(|| anyhow::Error::msg("open_brace_pos"))?;
  let maybe_newline = cx
    .iter_ascii_chars(BytePos(open_brace_pos.0 + 1))
    .skip_while(|item| {
      matches!(item, SrcItem::Ascii(' ' | '\t', _) | SrcItem::Comment(_))
    })
    .next();
  let params_start = match maybe_newline {
    Some(SrcItem::Ascii('\n', pos)) => pos,
    _ => BytePos(open_brace_pos.0 + 1),
  };

  parts.push(print_trailing_comments(
    cx,
    function.span_lo(),
    params_start,
  ));

  parts.push(Doc::new_group(
    print_params(
      cx,
      (
        params_start,
        function
          .body
          .as_ref()
          .map_or_else(|| function.span_hi(), |body| body.span_lo()),
      ),
      &function.params,
    )?,
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

pub fn print_params(
  cx: &mut AstPrinter,
  range: (BytePos, BytePos),
  params: &[Param],
) -> anyhow::Result<Doc> {
  let mut from_pos = range.0;

  if params.is_empty() {
    return Ok(Doc::new_concat(vec!["(".into(), ")".into()]));
  }

  let mut parts = Vec::new();
  let params_len = params.len();
  for (i, param) in params.iter().enumerate() {
    let param_path = fake_path(param);
    let param_start = param.span_lo();
    parts.push(print_leading_comments(cx, from_pos, param_start));
    from_pos = param.span_hi();

    let is_last = i == params_len - 1;
    parts.push(cx.print_pat_path(sub!(param_path, Param, pat, Pat))?);
    if !is_last {
      parts.push(",".into());
      parts.push(Doc::line());
    }
  }

  parts.push(print_trailing_comments(cx, from_pos, range.1));

  Ok(Doc::new_concat(vec![
    "(".into(),
    Doc::new_indent(Doc::new_concat(
      [&[Doc::softline()], parts.as_slice()].concat(),
    )),
    Doc::new_if_break(
      if params.last().map_or(false, |p| !p.pat.is_rest())
        && cx.should_print_comma == Comma::All
      {
        ","
      } else {
        ""
      }
      .into(),
      Doc::none(),
      None,
    ),
    Doc::softline(),
    ")".into(),
  ]))
}

pub fn print_return_stmt(
  cx: &mut AstPrinter,
  return_stmt: Path<ReturnStmt>,
) -> anyhow::Result<Doc> {
  let arg = return_stmt.sub_opt_box(
    |p| p.arg.as_ref(),
    |p, c| ARef::ReturnStmt(p, ReturnStmtField::Arg),
  );

  Ok(Doc::new_concat(vec![
    "return".into(),
    print_return_or_throw_argument(cx, arg)?,
  ]))
}

pub fn print_throw_stmt(
  cx: &mut AstPrinter,
  throw_stmt: Path<ThrowStmt>,
) -> anyhow::Result<Doc> {
  let arg = throw_stmt
    .sub(|p| (ARef::ThrowStmt(p, ThrowStmtField::Arg), p.arg.as_ref()));

  Ok(Doc::new_concat(vec![
    "throw".into(),
    print_return_or_throw_argument(cx, Some(arg))?,
  ]))
}

fn print_return_or_throw_argument(
  cx: &mut AstPrinter,
  arg: Option<Path<Expr>>,
) -> anyhow::Result<Doc> {
  let semi = if cx.semi { ";" } else { "" };

  let mut parts = Vec::new();

  if let Some(arg) = arg {
    let arg_doc = cx.print_expr(arg.node)?;

    let arg_doc = if arg.node.is_bin() || arg.node.is_seq() {
      Doc::new_group(
        Doc::new_concat(vec![
          Doc::new_if_break("(".into(), "".into(), None),
          Doc::new_indent(Doc::new_concat(vec![Doc::softline(), arg_doc])),
          Doc::softline(),
          Doc::new_if_break(")".into(), "".into(), None),
        ]),
        false,
        None,
        None,
      )
    } else {
      arg_doc
    };

    parts.push(" ".into());
    parts.push(arg_doc);
  }

  // TODO: comments
  let should_print_semi_before_comments = false;

  parts.push(semi.into());

  Ok(Doc::new_concat(parts))
}
