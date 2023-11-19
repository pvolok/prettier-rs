use swc_ecma_ast::Stmt;

use crate::{ast_printer::AstPrinter, doc::Doc};

pub fn print_stmt_seq<'a>(
  cx: &'a mut AstPrinter,
  stmts: &Vec<Stmt>,
) -> anyhow::Result<Vec<Doc>> {
  let mut body_parts = Vec::new();
  for (i, stmt) in stmts.iter().enumerate() {
    let is_last = i == stmts.len() - 1;

    body_parts.push(cx.print_stmt(stmt)?);
    if !is_last {
      body_parts.push(Doc::hardline());
      if cx.is_next_line_empty(stmts.iter(), i) {
        body_parts.push(Doc::hardline());
      }
    }
  }

  Ok(body_parts)
}
