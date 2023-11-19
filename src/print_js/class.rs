use swc_ecma_ast::{
  Class, ClassDecl, ClassMember, ClassMethod, ClassProp, Expr, Function,
  MethodKind, PrivateMethod, PropName,
};

use crate::{
  ast_printer::AstPrinter,
  doc::{Doc, RDoc},
};

use super::{assign::print_assignment, function::print_params};

pub fn print_class_decl(
  cx: &mut AstPrinter,
  class_decl: &ClassDecl,
) -> anyhow::Result<Doc> {
  let mut parts = vec!["class".into()];

  let group_mode = false;

  let mut parts_group = Vec::new();
  let mut extends_parts = Vec::new();

  parts_group.push(" ".into());
  parts_group.push(Doc::new_text(class_decl.ident.sym.to_string()));

  if let Some(super_class) = &class_decl.class.super_class {
    // TODO: See printSuperClass()
    let super_class_doc =
      Doc::new_concat(vec!["extends ".into(), cx.print_expr(&super_class)?]);

    if group_mode {
      extends_parts.push(Doc::line());
      extends_parts.push(Doc::group(super_class_doc));
    } else {
      extends_parts.push(" ".into());
      extends_parts.push(super_class_doc);
    }
  }

  if group_mode {
    todo!();
  } else {
    parts.extend_from_slice(&parts_group);
    parts.extend_from_slice(&extends_parts);
  }

  parts.push(" ".into());
  parts.push(print_class(cx, &class_decl.class)?);

  Ok(Doc::new_concat(parts))
}

pub fn print_class(cx: &mut AstPrinter, class: &Class) -> anyhow::Result<Doc> {
  let mut parts = Vec::new();

  for (i, class_member) in class.body.iter().enumerate() {
    let is_last = i == class.body.len() - 1;

    let doc = print_class_member(cx, class_member)?;
    parts.push(doc);

    if !cx.semi && class_member.is_class_prop() {
      todo!();
    }

    if !is_last {
      parts.push(Doc::hardline());

      if cx.is_next_line_empty(class.body.iter(), i) {
        parts.push(Doc::hardline());
      }
    }
  }

  let doc = Doc::new_concat(vec![
    "{".into(),
    if !parts.is_empty() {
      Doc::new_concat(vec![
        Doc::new_indent(Doc::new_concat(vec![
          Doc::hardline(),
          Doc::new_concat(parts),
        ])),
        Doc::hardline(),
      ])
    } else {
      "".into()
    },
    "}".into(),
  ]);
  Ok(doc)
}

pub fn print_class_member(
  cx: &mut AstPrinter,
  class_member: &ClassMember,
) -> anyhow::Result<Doc> {
  match class_member {
    ClassMember::Constructor(_) => todo!(),
    ClassMember::Method(class_method) => print_class_method(cx, class_method),
    ClassMember::PrivateMethod(private_method) => {
      print_private_method(cx, private_method)
    }
    ClassMember::ClassProp(class_prop) => print_class_prop(cx, class_prop),
    ClassMember::PrivateProp(_) => todo!(),
    ClassMember::TsIndexSignature(_) => todo!(),
    ClassMember::Empty(_) => Ok(Doc::none()),
    ClassMember::StaticBlock(_) => todo!(),
    ClassMember::AutoAccessor(acc) => todo!(),
  }
}

fn print_class_method(cx: &mut AstPrinter, class_method: &ClassMethod) -> RDoc {
  let mut parts = Vec::new();
  if class_method.is_static {
    parts.push("static ".into());
  }

  match class_method.kind {
    MethodKind::Method => {
      if class_method.function.is_async {
        parts.push("async ".into());
      }
    }
    MethodKind::Getter => {
      parts.push("get ".into());
    }
    MethodKind::Setter => {
      parts.push("set ".into());
    }
  }

  if class_method.function.is_generator {
    parts.push("*".into());
  }

  parts.push(print_prop_name(cx, &class_method.key)?);

  if class_method.is_optional {
    parts.push("?".into());
  }

  parts.push(print_method_body(cx, &class_method.function)?);

  Ok(Doc::new_concat(parts))
}

fn print_private_method(
  cx: &mut AstPrinter,
  private_method: &PrivateMethod,
) -> RDoc {
  let mut parts = Vec::new();
  if private_method.is_static {
    parts.push("static ".into());
  }

  match private_method.kind {
    MethodKind::Method => {
      if private_method.function.is_async {
        parts.push("async ".into());
      }
    }
    MethodKind::Getter => {
      parts.push("get ".into());
    }
    MethodKind::Setter => {
      parts.push("set ".into());
    }
  }

  if private_method.function.is_generator {
    parts.push("*".into());
  }

  parts.push("#".into());
  parts.push(cx.print_ident(&private_method.key.id)?);

  if private_method.is_optional {
    parts.push("?".into());
  }

  parts.push(print_method_body(cx, &private_method.function)?);

  Ok(Doc::new_concat(parts))
}

pub fn print_class_prop(cx: &mut AstPrinter, class_prop: &ClassProp) -> RDoc {
  let mut parts = Vec::new();
  let semi = Doc::from(if cx.semi { ";" } else { "" });

  if class_prop.is_static {
    parts.push("static ".into());
  }

  if class_prop.is_override {
    parts.push("override ".into());
  }
  if class_prop.readonly {
    parts.push("readonly ".into());
  }

  parts.push(print_prop_name(cx, &class_prop.key)?);
  if class_prop.is_optional {
    parts.push("?".into());
  }

  let left_doc = Doc::new_concat(parts);

  if let Some(value) = class_prop.value.as_ref() {
    let doc = print_assignment(
      cx,
      left_doc,
      super::assign::AssignmentLeft::PropName(&class_prop.key),
      " =".into(),
      &value,
    )?;
    Ok(Doc::new_concat(vec![doc, semi]))
  } else {
    Ok(Doc::new_concat(vec![left_doc, semi]))
  }
}

fn print_prop_name(cx: &mut AstPrinter, prop_name: &PropName) -> RDoc {
  // TODO: See printPropertyKey() for special cases
  let doc = match prop_name {
    PropName::Ident(ident) => cx.print_ident(ident)?,
    PropName::Str(str) => cx.print_str(str),
    PropName::Num(number) => cx.print_number(number),
    PropName::Computed(computed_prop_name) => Doc::new_concat(vec![
      "[".into(),
      cx.print_expr(computed_prop_name.expr.as_ref())?,
      "]".into(),
    ]),
    PropName::BigInt(big_int) => cx.print_big_int(big_int),
  };
  Ok(doc)
}

fn print_method_body(cx: &mut AstPrinter, function: &Function) -> RDoc {
  let params_doc = print_params(cx, &function.params)?;

  let _should_break_params = false;
  let _should_group_params = false;

  let mut parts = Vec::new();

  parts.push(Doc::group(params_doc));

  if let Some(body) = &function.body {
    parts.push(" ".into());
    parts.push(cx.print_block_stmt(body, false)?);
  } else if cx.semi {
    parts.push(";".into());
  }

  Ok(Doc::new_concat(parts))
}

fn print_heritage_clauses(cx: &mut AstPrinter, super_class: &Expr) -> RDoc {
  Ok(Doc::new_concat(vec![
    Doc::line(),
    "extends".into(),
    Doc::group(Doc::new_indent(Doc::new_concat(vec![
      Doc::line(),
      cx.print_expr(super_class)?,
    ]))),
  ]))
}
