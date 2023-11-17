use swc_ecma_ast::{CondExpr, MemberProp};
use swc_ecma_visit::{fields::*, AstParentNodeRef};

use crate::{
  ast_path::Path,
  ast_printer::AstPrinter,
  doc::{Doc, DocAlign},
};

pub fn print_cond(
  cx: &mut AstPrinter,
  cond_expr: Path<CondExpr>,
) -> anyhow::Result<Doc> {
  let cons = cond_expr.node.cons.as_ref();
  let alt = cond_expr.node.alt.as_ref();

  let is_test = if let Some(parent) = cond_expr.parent {
    match parent.node_ref {
      AstParentNodeRef::CondExpr(_, field) => match field {
        CondExprField::Test => true,
        _ => false,
      },
      _ => false,
    }
  } else {
    false
  };
  let force_no_indent = if let Some(parent) = cond_expr.parent {
    match parent.node_ref {
      AstParentNodeRef::CondExpr(_, field) => match field {
        CondExprField::Test => false,
        _ => true,
      },
      _ => false,
    }
  } else {
    false
  };

  let mut parts = Vec::new();

  let is_jsx = false;
  if is_jsx {
    todo!()
  } else {
    let cons_doc = cx.print_expr(&cons)?;
    let alt_doc = cx.print_expr(&alt)?;
    let part = Doc::new_concat(vec![
      Doc::line(),
      "? ".into(),
      if cons.is_cond() {
        Doc::new_if_break("".into(), "(".into(), None)
      } else {
        Doc::from("")
      },
      Doc::new_align(cons_doc, DocAlign::Num(2)),
      if cons.is_cond() {
        Doc::new_if_break("".into(), ")".into(), None)
      } else {
        Doc::from("")
      },
      Doc::line(),
      ": ".into(),
      Doc::new_align(alt_doc, DocAlign::Num(2)),
    ]);

    let doc = if cond_expr
      .parent
      .map_or(true, |parent| match parent.node_ref {
        AstParentNodeRef::CondExpr(_, field) => match field {
          CondExprField::Test | CondExprField::Alt => true,
          _ => false,
        },
        _ => true,
      }) {
      part
    } else {
      Doc::new_align(part, DocAlign::Num((cx.tab_width - 2).max(0) as _))
    };
    parts.push(doc);
  }

  // TODO: comments
  let should_break = false;

  let maybe_group = |doc| {
    if cond_expr.parent.map_or(true, |p| {
      !matches!(p.node_ref, AstParentNodeRef::CondExpr(_, _))
    }) {
      Doc::new_group(doc, should_break, None, None)
    } else if should_break {
      Doc::new_concat(vec![doc, Doc::break_parent()])
    } else {
      doc
    }
  };

  // Break the closing paren to keep the chain right after it:
  // (a
  //   ? b
  //   : c
  // ).call()
  let break_closing_paren = !is_jsx
    && match &cond_expr.parent {
      Some(parent) => match &parent.node_ref {
        AstParentNodeRef::MemberExpr(member_expr, _) => {
          match member_expr.prop {
            MemberProp::Computed(_) => false,
            _ => true,
          }
        }
        _ => false,
      },
      None => false,
    };

  let should_extra_indent =
    should_extra_indent_for_conditional_expression(&cond_expr);

  let doc = maybe_group(Doc::new_concat(vec![
    print_ternary_test(cx, cond_expr.clone())?,
    if force_no_indent {
      Doc::new_concat(parts)
    } else {
      Doc::new_indent(Doc::new_concat(parts))
    },
    if break_closing_paren && !should_extra_indent {
      Doc::softline()
    } else {
      "".into()
    },
  ]));

  let doc = if is_test || should_extra_indent {
    Doc::new_group(
      Doc::new_concat(vec![
        Doc::new_indent(Doc::new_concat(vec![Doc::softline(), doc])),
        Doc::softline(),
      ]),
      false,
      None,
      None,
    )
  } else {
    doc
  };
  Ok(doc)
}

fn should_extra_indent_for_conditional_expression(
  expr: &Path<CondExpr>,
) -> bool {
  let mut parent = expr.parent.as_ref();
  let mut direct = true;
  while let Some(parent_) = parent {
    match parent_.node_ref {
      AstParentNodeRef::CallExpr(_, CallExprField::Callee)
      | AstParentNodeRef::MemberExpr(_, MemberExprField::Obj) => {
        parent = parent_.parent.as_ref();
        direct = false;
      }

      AstParentNodeRef::NewExpr(_, NewExprField::Callee) => {
        parent = parent_.parent.as_ref();
        direct = false;
        break;
      }

      _ => break,
    }
  }

  // Do not add indent to direct `ConditionalExpression`
  if direct {
    return false;
  }

  let parent = if let Some(parent) = parent {
    parent
  } else {
    return false;
  };

  match parent.node_ref {
    AstParentNodeRef::AssignExpr(_, AssignExprField::Right) => true,
    AstParentNodeRef::VarDeclarator(_, VarDeclaratorField::Init) => true,
    AstParentNodeRef::ReturnStmt(_, ReturnStmtField::Arg) => true,
    AstParentNodeRef::ThrowStmt(_, ThrowStmtField::Arg) => true,
    AstParentNodeRef::UnaryExpr(_, UnaryExprField::Arg) => true,
    AstParentNodeRef::YieldExpr(_, YieldExprField::Arg) => true,
    _ => false,
  }
}

fn print_ternary_test(
  cx: &mut AstPrinter,
  cond_expr: Path<CondExpr>,
) -> anyhow::Result<Doc> {
  let test_doc = cx.print_expr(&cond_expr.node.test)?;
  /*
   *     a
   *       ? b
   *       : multiline
   *         test
   *         node
   *       ^^ align(2)
   *       ? d
   *       : e
   */
  if let Some(parent) = cond_expr.parent {
    match parent.node_ref {
      AstParentNodeRef::CondExpr(_, CondExprField::Alt) => {
        return Ok(Doc::new_align(test_doc, DocAlign::Num(2)));
      }
      _ => (),
    }
  }
  Ok(test_doc)
}
