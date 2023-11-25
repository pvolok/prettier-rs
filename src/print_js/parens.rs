use swc_common::Spanned;
use swc_ecma_ast::{BinExpr, BlockStmtOrExpr, Expr};

use crate::{
  ast_path::{ARef, Path},
  ast_printer::AstPrinter,
  ast_util::starts_with_no_lookahead_token,
};

pub fn needs_parens(cx: &mut AstPrinter, expr: &Path<Expr>) -> bool {
  match expr.node {
    Expr::Object(object_lit) => {
      let cur_obj_expr_span = expr.span();
      let arrow_body = expr.find_ancestor(|aref| match aref {
        ARef::ArrowExpr(arrow, _) => Some(arrow.body.as_ref()),
        _ => None,
      });
      match arrow_body {
        Some(BlockStmtOrExpr::Expr(expr)) => {
          if !expr.is_seq()
            && !expr.is_assign()
            && starts_with_no_lookahead_token(expr, |leftmost| {
              leftmost.span() == cur_obj_expr_span
            })
          {
            return true;
          }
        }
        _ => (),
      }
    }
    Expr::Bin(bin_expr) => match expr.parent.node_ref {
      ARef::BinExpr(BinExpr { op: parent_op, .. }, _) => {
        return bin_expr.op != *parent_op
      }
      _ => (),
    },
    _ => (),
  }

  false
}
