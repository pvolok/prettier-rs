use swc_common::Spanned;
use swc_ecma_ast::{
  BinExpr, BinaryOp, BlockStmtOrExpr, CallExpr, Expr, ForHead, OptChainBase,
  Pat, UnaryOp, UpdateOp,
};
use swc_ecma_visit::fields::{
  BinExprField, CallExprField, CalleeField, CondExprField, ForOfStmtField,
  MemberExprField, NewExprField, OptChainBaseField, OptChainExprField,
};

use crate::{
  ast_path::{ARef, Path},
  ast_printer::AstPrinter,
  ast_util::starts_with_no_lookahead_token,
};

pub fn needs_parens(cx: &mut AstPrinter, expr: &Path<Expr>) -> bool {
  // println!(
  //   "PARENS? {}",
  //   expr
  //     .parents()
  //     .map(|node_ref| format!("{:?}", node_ref.kind()))
  //     .intersperse(" -> ".to_string())
  //     .collect::<String>()
  // );

  match expr.node {
    Expr::Ident(ident) => {
      let id = ident.sym.as_str();

      // `for ((async) of []);` and `for ((let) of []);`
      if ((id == "async") || id == "let")
        && matches!(
          expr.parents().take(3).collect::<Vec<_>>().as_slice(),
          [
            ARef::Pat(_, _),
            ARef::ForHead(_, _),
            ARef::ForOfStmt(for_of_stmt, ForOfStmtField::Left)
          ] if !for_of_stmt.is_await
        )
      {
        return true;
      }

      // `for ((let.a) of []);`
      if id == "let" {
        let left_expr = expr.find_ancestor(|p| match p {
          ARef::ForOfStmt(for_of_stmt, _) => match &for_of_stmt.left {
            ForHead::Pat(pat) => match pat.as_ref() {
              Pat::Expr(expr) => Some(expr.as_ref()),
              _ => None,
            },
            _ => None,
          },
          _ => None,
        });
        if let Some(left_expr) = left_expr {
          let need_parens = starts_with_no_lookahead_token(left_expr, |n| {
            n.span_lo() == left_expr.span_lo()
          });
          if need_parens {
            return true;
          }
        }
      }
    }
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
    Expr::Arrow(_arrow_expr) => match expr.parent.node_ref {
      ARef::BinExpr(_, _) => return true,
      ARef::NewExpr(_, NewExprField::Callee) => return true,
      ARef::Callee(_, CalleeField::Expr) => return true,
      ARef::MemberExpr(_, MemberExprField::Obj) => return true,
      // ARef::OptChainExpr(_, OptChainExprField::Base) => return true,
      ARef::TaggedTpl(_, _) => return true,
      ARef::UnaryExpr(_, _) => return true,
      ARef::AwaitExpr(_, _) => return true,
      ARef::CondExpr(_, CondExprField::Test) => return true,
      _ => (),
    },
    Expr::Bin(bin_expr) => match expr.parent.node_ref {
      ARef::BinExpr(BinExpr { op: parent_op, .. }, _) => {
        return bin_expr.op != *parent_op
      }
      _ => (),
    },
    Expr::Update(update_expr) => match expr.parent.node_ref {
      ARef::UnaryExpr(parent, _) => {
        return match (update_expr.op, parent.op) {
          (UpdateOp::PlusPlus, UnaryOp::Plus)
          | (UpdateOp::MinusMinus, UnaryOp::Minus)
            if update_expr.prefix =>
          {
            true
          }
          _ => false,
        }
      }

      ARef::MemberExpr(_, MemberExprField::Obj) => return true,
      // TODO: opt member
      ARef::TaggedTpl(_, _) => return true,
      ARef::Callee(_, _) => return true,
      ARef::NewExpr(_, NewExprField::Callee) => return true,
      ARef::BinExpr(parent, BinExprField::Left)
        if parent.op == BinaryOp::Exp =>
      {
        return true
      }
      _ => (),
    },
    Expr::Unary(unary_expr) => (),
    Expr::Yield(_) if matches!(expr.parent.node_ref, ARef::AwaitExpr(_, _)) => {
      return true
    }
    Expr::Yield(_) | Expr::Await(_) => match expr.parent.node_ref {
      ARef::TaggedTpl(_, _) => return true,
      ARef::UnaryExpr(_, _) => return true,
      ARef::BinExpr(_, _) => return true,
      ARef::SpreadElement(_, _) => return true,
      ARef::MemberExpr(_, MemberExprField::Obj) => return true,
      // TODO: opt chain
      ARef::Callee(_, _) => return true,
      ARef::NewExpr(_, NewExprField::Callee) => return true,

      ARef::CondExpr(_, CondExprField::Test) => return true,

      _ => return false,
    },
    Expr::OptChain(_) => match expr.parent.node_ref {
      ARef::Callee(_, _) => return true,
      ARef::NewExpr(_, NewExprField::Callee) => return true,
      ARef::MemberExpr(_, MemberExprField::Obj) => return true,
      _ => (),
    },
    _ => (),
  }

  false
}
