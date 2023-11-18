use swc_ecma_ast::{Class, ClassDecl, ClassMember, PropName};

use crate::{ast_printer::AstPrinter, doc::Doc};

use super::function::print_function;

pub fn print_class_decl(
  cx: &mut AstPrinter,
  class_decl: &ClassDecl,
) -> anyhow::Result<Doc> {
  let mut parts = vec!["class".into()];

  let group_mode = false;

  let mut parts_group = Vec::new();
  let mut extends_parts = Vec::new();

  parts_group.push(" ".into());
  parts_group.push(Doc::new_text(class_decl.ident.to_string()));

  if let Some(super_class) = &class_decl.class.super_class {
    todo!()
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
    ClassMember::Method(class_method) => {
      let mut parts = Vec::new();
      if class_method.is_static {
        parts.push("static ".into());
      }

      let key_doc = match &class_method.key {
        PropName::Ident(ident) => Doc::new_text(ident.to_string()),
        PropName::Str(_) => todo!(),
        PropName::Num(_) => todo!(),
        PropName::Computed(_) => todo!(),
        PropName::BigInt(_) => todo!(),
      };
      parts.push(print_function(cx, &class_method.function, Some(key_doc))?);

      Ok(Doc::new_concat(parts))
    }
    ClassMember::PrivateMethod(_) => todo!(),
    ClassMember::ClassProp(_) => todo!(),
    ClassMember::PrivateProp(_) => todo!(),
    ClassMember::TsIndexSignature(_) => todo!(),
    ClassMember::Empty(_) => todo!(),
    ClassMember::StaticBlock(_) => todo!(),
    ClassMember::AutoAccessor(_) => todo!(),
  }
}
