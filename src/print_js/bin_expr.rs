use swc_ecma_ast::{BinExpr, BinaryOp, Expr};

use crate::{
  ast_path::{sub, sub_box, var, Path},
  ast_printer::AstPrinter,
  doc::Doc,
};

pub fn print_bin_expr(
  cx: &mut AstPrinter,
  bin_expr: Path<BinExpr>,
) -> anyhow::Result<Doc> {
  print_bin_expr_inner(cx, bin_expr, false)
}

pub fn print_bin_expr_inner(
  cx: &mut AstPrinter,
  bin_expr: Path<BinExpr>,
  is_nested: bool,
) -> anyhow::Result<Doc> {
  let mut parts = Vec::new();

  let left = sub_box!(bin_expr, BinExpr, left, Left);

  // Put all operators with the same precedence level in the same
  // group. The reason we only need to do this with the `left`
  // expression is because given an expression like `1 + 2 - 3`, it
  // is always parsed like `((1 + 2) - 3)`, meaning the `left` side
  // is where the rest of the expression will exist. Binary
  // expressions on the right side mean they have a difference
  // precedence level and should be treated as a separate group, so
  // print them normally. (This doesn't hold for the `**` operator,
  // which is unique in that it is right-associative.)
  match left.node {
    Expr::Bin(left_bin_expr)
      if should_flatten(bin_expr.node.op, left_bin_expr.op) =>
    {
      let left_bin_expr = var!(left, Expr, left_bin_expr, Bin);
      parts.push(print_bin_expr_inner(cx, left_bin_expr, true)?);
    }
    _ => {
      parts.push(Doc::new_group(cx.print_expr_path(left)?, false, None, None));
    }
  }

  let should_inline = should_inline_bin_expr(bin_expr.node);
  let line_before_operator = false;

  let op_doc = Doc::from(bin_expr.node.op.as_str());

  let right = Doc::new_concat(vec![
    op_doc,
    if should_inline {
      " ".into()
    } else {
      Doc::line()
    },
    cx.print_expr_path(sub_box!(bin_expr, BinExpr, right, Right))?,
  ]);

  let should_break = false;
  let should_group = true;

  parts.push(if line_before_operator { "" } else { " " }.into());
  parts.push(if should_group {
    Doc::new_group(right, should_break, None, None)
  } else {
    right
  });

  Ok(Doc::new_concat(parts))
}

fn get_precedence(op: BinaryOp) -> u8 {
  match op {
    BinaryOp::NullishCoalescing => 1,
    BinaryOp::LogicalOr => 2,
    BinaryOp::LogicalAnd => 3,
    BinaryOp::BitOr => 4,
    BinaryOp::BitXor => 5,
    BinaryOp::BitAnd => 6,
    BinaryOp::EqEq | BinaryOp::NotEq | BinaryOp::EqEqEq | BinaryOp::NotEqEq => {
      7
    }
    BinaryOp::Lt
    | BinaryOp::LtEq
    | BinaryOp::Gt
    | BinaryOp::GtEq
    | BinaryOp::In
    | BinaryOp::InstanceOf => 8,
    BinaryOp::LShift | BinaryOp::RShift | BinaryOp::ZeroFillRShift => 9,
    BinaryOp::Add | BinaryOp::Sub => 10,
    BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 11,
    BinaryOp::Exp => 12,
  }
}

fn should_flatten(parent_op: BinaryOp, node_op: BinaryOp) -> bool {
  if get_precedence(node_op) != get_precedence(parent_op) {
    return false;
  }

  // ** is right-associative
  // x ** y ** z --> x ** (y ** z)
  if parent_op == BinaryOp::Exp {
    return false;
  }

  // x == y == z --> (x == y) == z
  if is_equality_op(parent_op) && is_equality_op(node_op) {
    return false;
  }

  // x * y % z --> (x * y) % z
  if (node_op == BinaryOp::Mod && is_multiplicative_op(parent_op))
    || parent_op == BinaryOp::Mod && is_multiplicative_op(node_op)
  {
    return false;
  }

  // x * y / z --> (x * y) / z
  // x / y * z --> (x / y) * z
  if node_op != parent_op
    && is_multiplicative_op(node_op)
    && is_multiplicative_op(parent_op)
  {
    return false;
  }

  // x << y << z --> (x << y) << z
  if is_bitshift_op(parent_op) && is_bitshift_op(node_op) {
    return false;
  }

  return true;
}

fn is_equality_op(op: BinaryOp) -> bool {
  match op {
    BinaryOp::EqEq | BinaryOp::NotEq | BinaryOp::EqEqEq | BinaryOp::NotEqEq => {
      true
    }
    _ => false,
  }
}

fn is_multiplicative_op(op: BinaryOp) -> bool {
  match op {
    BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => true,
    _ => false,
  }
}

fn is_bitshift_op(op: BinaryOp) -> bool {
  match op {
    BinaryOp::RShift | BinaryOp::ZeroFillRShift | BinaryOp::LShift => true,
    _ => false,
  }
}

pub fn should_inline_bin_expr(bin_expr: &BinExpr) -> bool {
  match bin_expr.op {
    BinaryOp::LogicalOr | BinaryOp::LogicalAnd => (),
    _ => return false,
  }

  match bin_expr.right.as_ref() {
    Expr::Object(object_lit) if object_lit.props.len() > 0 => {
      return true;
    }
    Expr::Array(array_lit) if array_lit.elems.len() > 0 => {
      return true;
    }
    Expr::JSXElement(_) | Expr::JSXFragment(_) => return true,
    _ => (),
  }

  return false;
}
