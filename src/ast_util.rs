use swc_ecma_ast::{
  Callee, Expr, Lit, Module, OptChainBase, PatOrExpr, Stmt, UnaryOp,
};
use swc_ecma_visit::{Fold, FoldWith};

use crate::ast_printer::AstPrinter;

const LONE_SHORT_ARGUMENT_THRESHOLD_RATE: f32 = 0.25;
pub fn is_lone_short_argument(cx: &mut AstPrinter, expr: &Expr) -> bool {
  let print_width = cx.print_width;

  // TODO: comment

  let threshold =
    (print_width as f32 * LONE_SHORT_ARGUMENT_THRESHOLD_RATE).floor() as usize;

  match expr {
    Expr::This(_) => return true,
    Expr::Ident(ident) if ident.sym.as_str().len() <= threshold => return true,
    Expr::Unary(unary_expr) => match (unary_expr.op, unary_expr.arg.as_ref()) {
      (UnaryOp::Plus | UnaryOp::Minus, Expr::Lit(lit)) => match lit {
        Lit::Num(_) => return true,
        _ => (),
      },
      _ => return is_lone_short_argument(cx, &unary_expr.arg),
    },
    Expr::Lit(lit) => match lit {
      Lit::Regex(regex) => return regex.exp.len() <= threshold,
      Lit::Str(str) => return str.raw.as_ref().unwrap().len() <= threshold,
      _ => return true,
    },
    Expr::Tpl(tpl) => {
      let quasi = tpl.quasis.first();
      return if let Some(quasi) = quasi {
        tpl.exprs.is_empty()
          && quasi.raw.chars().count() <= threshold
          && !quasi.raw.contains("\n")
      } else {
        true
      };
    }
    Expr::Call(call_expr) if call_expr.args.is_empty() => {
      match &call_expr.callee {
        Callee::Expr(expr) => match expr.as_ref() {
          Expr::Ident(ident) => {
            return ident.sym.chars().count() <= threshold - 2
          }
          _ => (),
        },
        _ => (),
      }
    }
    _ => (),
  }

  expr.is_lit()
}

pub fn starts_with_no_lookahead_token(
  expr: &Expr,
  pred: fn(&Expr) -> bool,
) -> bool {
  match expr {
    Expr::Bin(bin_expr) => starts_with_no_lookahead_token(&bin_expr.left, pred),
    Expr::Assign(assign_expr) => match &assign_expr.left {
      PatOrExpr::Expr(expr) => starts_with_no_lookahead_token(&expr, pred),
      PatOrExpr::Pat(_) => todo!(),
    },
    Expr::Member(member_expr) => {
      starts_with_no_lookahead_token(&member_expr.obj, pred)
    }
    Expr::OptChain(opt_chain_expr) => {
      match opt_chain_expr.base.as_ref() {
        OptChainBase::Member(member_expr) => {
          starts_with_no_lookahead_token(&member_expr.obj, pred)
        }
        OptChainBase::Call(opt_call) => {
          if opt_call.callee.is_fn_expr() {
            // IIFEs are always already parenthesized
            false
          } else {
            starts_with_no_lookahead_token(&opt_call.callee, pred)
          }
        }
      }
    }
    Expr::TaggedTpl(tagged_tpl) => {
      if tagged_tpl.tag.is_fn_expr() {
        // IIFEs are always already parenthesized
        false
      } else {
        starts_with_no_lookahead_token(&tagged_tpl.tag, pred)
      }
    }
    Expr::Call(call_expr) => {
      match &call_expr.callee {
        Callee::Expr(expr) => {
          if expr.is_fn_expr() {
            // IIFEs are always already parenthesized
            false
          } else {
            starts_with_no_lookahead_token(&expr, pred)
          }
        }
        _ => pred(expr),
      }
    }
    Expr::Cond(cond_expr) => {
      starts_with_no_lookahead_token(&cond_expr.test, pred)
    }
    Expr::Update(update_expr) => {
      !update_expr.prefix
        && starts_with_no_lookahead_token(&update_expr.arg, pred)
    }
    Expr::Seq(seq_expr) => {
      starts_with_no_lookahead_token(seq_expr.exprs.first().unwrap(), pred)
    }
    expr => pred(expr),
  }
}

pub fn skip_parens(expr: &Expr) -> &Expr {
  match expr {
    Expr::Paren(parent_expr) => &parent_expr.expr,
    _ => expr,
  }
}

struct AstCleaner;

impl swc_ecma_visit::Fold for AstCleaner {
  fn fold_expr(&mut self, n: Expr) -> Expr {
    match n {
      Expr::Paren(paren_expr) => *paren_expr.expr.fold_with(self),
      n => n.fold_children_with(self),
    }
  }
}

pub fn clean_ast<N: FoldWith<AstCleaner>>(node: N) -> N {
  node.fold_with(&mut AstCleaner)
}
