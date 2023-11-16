use swc_ecma_ast::{
  ArrowExpr, BlockStmtOrExpr, FnDecl, FnExpr, Function, Ident, Param,
};

use crate::{ast_printer::AstPrinter, doc::Doc};

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

  fn rec(
    cx: &mut AstPrinter,
    vars: &mut LocalVars,
    arrow_expr: &ArrowExpr,
  ) -> anyhow::Result<Doc> {
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
        swc_ecma_ast::Expr::Arrow(arrow_expr) => {
          return rec(cx, vars, arrow_expr)
        }
        _ => (),
      },
      _ => (),
    }

    let doc = match arrow_expr.body.as_ref() {
      BlockStmtOrExpr::BlockStmt(block_stmt) => {
        cx.print_block_stmt(block_stmt, false)
      }
      BlockStmtOrExpr::Expr(expr) => cx.print_expr(&expr),
    };

    doc
  }
  let body = rec(cx, &mut vars, arrow_expr)?;

  let should_put_body_on_same_line = true;

  let chain_group_id = cx.group_id("arrow-chain");

  let sigs_doc = print_arrow_sigs(cx, vars.sig_docs)?;
  let should_break_sigs = false;

  // TODO: print_arrow_body
  let body = body;

  let doc = Doc::new_group(
    Doc::new_concat(vec![
      Doc::new_group(sigs_doc, should_break_sigs, None, Some(chain_group_id)),
      " =>".into(),
      body,
      "".into(),
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
) -> anyhow::Result<Doc> {
  if sig_docs.len() == 1 {
    return Ok(sig_docs.get(0).unwrap().clone());
  }

  let doc = Doc::new_group(
    Doc::new_indent(Doc::new_concat(
      sig_docs
        .into_iter()
        .intersperse(Doc::new_concat(vec![" =>".into(), Doc::line()]))
        .collect::<Vec<Doc>>(),
    )),
    false,
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
