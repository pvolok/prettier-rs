use swc_ecma_ast::{
  BlockStmtOrExpr, Callee, Expr, Lit, ObjectPatProp, Pat, PatOrExpr, PropName,
  PropOrSpread,
};
use swc_ecma_visit::AstParentKind;

use crate::{
  ast_path::fake_path,
  ast_printer::AstPrinter,
  ast_util::{is_lone_short_argument, skip_parens},
  doc::Doc,
};

use super::{bin_expr::should_inline_bin_expr, function::print_arrow_expr};

pub enum AssignmentLeft<'a> {
  Pat(&'a Pat),
  PatOrExpr(&'a PatOrExpr),
  PropOrSpread(&'a PropOrSpread),
  PropName(&'a PropName),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AssignmentLayout {
  Chain,
  ChainTailArrowChain,
  ChainTail,
  BreakAfterOp,
  NeverBreakAfterOp,
  BreakLhs,
  Fluid,
}

pub fn print_assignment(
  cx: &mut AstPrinter,
  left_doc: Doc,
  left: AssignmentLeft,
  op_doc: Doc,
  right: &Expr,
) -> anyhow::Result<Doc> {
  let layout = choose_layout(cx, &left_doc, left, right);

  // dbg
  // let op_doc =
  //   Doc::new_concat(vec![op_doc, format!("<{:?}>", layout).as_str().into()]);

  let right_doc = match skip_parens(right) {
    Expr::Arrow(arrow_expr) => {
      print_arrow_expr(cx, fake_path(arrow_expr), Some(layout))?
    }
    _ => cx.print_expr(right)?,
  };

  let doc = match layout {
    // Parts of assignment chains aren't wrapped in groups.
    // Once one of them breaks, the chain breaks too.
    AssignmentLayout::Chain => Doc::new_concat(vec![
      Doc::new_group(left_doc, false, None, None),
      op_doc,
      Doc::line(),
      right_doc,
    ]),
    AssignmentLayout::ChainTailArrowChain => Doc::new_concat(vec![
      Doc::new_group(left_doc, false, None, None),
      op_doc,
      right_doc,
    ]),
    AssignmentLayout::ChainTail => Doc::new_concat(vec![
      Doc::new_group(left_doc, false, None, None),
      op_doc,
      Doc::new_indent(Doc::new_concat(vec![Doc::line(), right_doc])),
    ]),
    // First break after operator, then the sides are broken independently on their own lines
    AssignmentLayout::BreakAfterOp => Doc::new_group(
      Doc::new_concat(vec![
        Doc::new_group(left_doc, false, None, None),
        op_doc,
        Doc::new_group(
          Doc::new_indent(Doc::new_concat(vec![Doc::line(), right_doc])),
          false,
          None,
          None,
        ),
      ]),
      false,
      None,
      None,
    ),
    // First break right-hand side, then left-hand side
    AssignmentLayout::NeverBreakAfterOp => Doc::new_group(
      Doc::new_concat(vec![
        Doc::new_group(left_doc, false, None, None),
        op_doc,
        " ".into(),
        right_doc,
      ]),
      false,
      None,
      None,
    ),
    AssignmentLayout::BreakLhs => Doc::new_group(
      Doc::new_concat(vec![
        left_doc,
        op_doc,
        " ".into(),
        Doc::new_group(right_doc, false, None, None),
      ]),
      false,
      None,
      None,
    ),
    // First break right-hand side, then after operator
    AssignmentLayout::Fluid => {
      let group_id = cx.group_id("assignment");
      Doc::new_group(
        Doc::new_concat(vec![
          Doc::new_group(left_doc, false, None, None),
          op_doc,
          Doc::new_group(
            Doc::new_indent(Doc::line()),
            false,
            None,
            Some(group_id),
          ),
          Doc::line_suffix_boundary(),
          Doc::new_indent_if_break(right_doc, Some(group_id), false),
        ]),
        false,
        None,
        None,
      )
    }
  };
  Ok(doc)
}

fn choose_layout(
  cx: &mut AstPrinter,
  left_doc: &Doc,
  left: AssignmentLeft,
  right: &Expr,
) -> AssignmentLayout {
  // Short assignment chains (only 2 segments) are NOT formatted as chains.
  //   1) a = b = c; (expression statements)
  //   2) var/let/const a = b = c;

  let is_tail = !right.is_assign();

  let path_3 = if cx.stack.len() >= 3 {
    &cx.stack[..3]
  } else {
    &cx.stack[..]
  };
  let should_use_chain_formatting = match path_3 {
    [AstParentKind::AssignExpr(_), AstParentKind::AssignExpr(_) | AstParentKind::VarDeclarator(_), AstParentKind::ExprStmt(_) | AstParentKind::VarDecl(_)] => {
      true
    }
    _ => false,
  };
  if should_use_chain_formatting {
    return if !is_tail {
      AssignmentLayout::Chain
    } else {
      let is_arrow_arrow = match right {
        Expr::Arrow(arrow_expr) => match arrow_expr.body.as_ref() {
          BlockStmtOrExpr::Expr(expr) if expr.is_arrow() => true,
          _ => false,
        },
        _ => false,
      };
      if is_arrow_arrow {
        AssignmentLayout::ChainTailArrowChain
      } else {
        AssignmentLayout::ChainTail
      }
    };
  }

  let is_head_of_long_chain = match right {
    Expr::Assign(assign_expr) if assign_expr.right.is_assign() => true,
    _ => false,
  };

  if is_head_of_long_chain {
    return AssignmentLayout::BreakAfterOp;
  }

  match right {
    Expr::Call(call_expr) => match &call_expr.callee {
      Callee::Expr(expr) => match expr.as_ref() {
        Expr::Ident(ident) if ident.sym.as_str() == "require" => {
          return AssignmentLayout::NeverBreakAfterOp
        }
        _ => (),
      },
      _ => (),
    },
    _ => (),
  };

  let can_break_left_doc = left_doc.can_break();

  let path_2 = if cx.stack.len() >= 2 {
    &cx.stack[..2]
  } else {
    &cx.stack[..]
  };
  let is_arrow_var_declarator = match path_2 {
    [AstParentKind::AssignExpr(_), AstParentKind::VarDeclarator(_)]
      if right.is_arrow() =>
    {
      true
    }
    _ => false,
  };
  if is_complex_destructuring(left)
    || is_arrow_var_declarator && can_break_left_doc
  {
    return AssignmentLayout::BreakLhs;
  }

  let has_short_key = false;
  if should_break_after_operator(cx, right, has_short_key) {
    return AssignmentLayout::BreakAfterOp;
  }

  if !can_break_left_doc
    && match right {
      Expr::Tpl(_) => true,
      Expr::TaggedTpl(_) => true,
      Expr::Lit(lit) => match lit {
        Lit::Bool(_) => true,
        Lit::Num(_) => true,
        _ => false,
      },
      Expr::Class(_) => true,
      _ => has_short_key,
    }
  {
    return AssignmentLayout::NeverBreakAfterOp;
  }

  AssignmentLayout::Fluid
}

fn is_complex_destructuring(left: AssignmentLeft) -> bool {
  let object_pat = match left {
    AssignmentLeft::Pat(pat) => match pat {
      Pat::Object(object_pat) => object_pat,
      _ => return false,
    },
    AssignmentLeft::PatOrExpr(pat_or_expr) => match pat_or_expr {
      PatOrExpr::Expr(_) => return false,
      PatOrExpr::Pat(pat) => match pat.as_ref() {
        Pat::Object(object_pat) => object_pat,
        _ => return false,
      },
    },
    AssignmentLeft::PropOrSpread(_) => todo!(),
    AssignmentLeft::PropName(_) => return false,
  };

  object_pat.props.len() > 2
    && object_pat.props.iter().any(|prop| match prop {
      ObjectPatProp::KeyValue(_) => true,
      ObjectPatProp::Assign(assign_pat_prop) => assign_pat_prop.value.is_some(),
      ObjectPatProp::Rest(_) => false,
    })
}

fn should_break_after_operator(
  cx: &mut AstPrinter,
  right: &Expr,
  has_short_key: bool,
) -> bool {
  match right {
    Expr::Bin(bin_expr) if !should_inline_bin_expr(bin_expr) => return true,
    Expr::Cond(conv_expr) => {
      return match conv_expr.test.as_ref() {
        Expr::Bin(bin_expr) if !should_inline_bin_expr(bin_expr) => true,
        _ => false,
      }
    }
    Expr::Seq(_) => return true,
    Expr::Class(class_expr) => return !class_expr.class.decorators.is_empty(),
    _ => (),
  }

  if has_short_key {
    return false;
  }

  let mut node = right;
  loop {
    match node {
      Expr::Unary(unary_expr) => {
        node = &unary_expr.arg;
      }
      Expr::Yield(yield_expr) => {
        if let Some(expr) = yield_expr.arg.as_ref() {
          node = expr.as_ref();
        } else {
          break;
        }
      }
      Expr::Await(await_expr) => {
        node = &await_expr.arg;
      }
      Expr::Lit(lit) if matches!(lit, Lit::Str(_)) => return true,
      _ => {
        break;
      }
    }
  }

  is_poorly_breakable_member_or_call_chain(cx, node, false)
}

/**
 * A chain with no calls at all or whose calls are all without arguments or with lone short arguments,
 * excluding chains printed by `printMemberChain`
 */
fn is_poorly_breakable_member_or_call_chain(
  cx: &mut AstPrinter,
  expr: &Expr,
  deep: bool,
) -> bool {
  match expr {
    Expr::Call(call_expr) => {
      let is_poorly_breakable_call = call_expr.args.is_empty()
        || match call_expr.args.as_slice() {
          [arg] => is_lone_short_argument(cx, &arg.expr),
          _ => false,
        };
      if !is_poorly_breakable_call {
        return false;
      }

      match &call_expr.callee {
        Callee::Expr(expr) => {
          is_poorly_breakable_member_or_call_chain(cx, &expr, true)
        }
        _ => false,
      }
    }
    Expr::Member(member_expr) => {
      is_poorly_breakable_member_or_call_chain(cx, &member_expr.obj, true)
    }
    _ => deep && (expr.is_ident() || expr.is_this()),
  }
}
