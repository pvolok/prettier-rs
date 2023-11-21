use swc_ecma_ast::{
  DefaultDecl, ExportAll, ExportDecl, ExportDefaultDecl, ExportDefaultExpr,
  ExportSpecifier, ImportDecl, ImportSpecifier, ModuleDecl, ModuleExportName,
  NamedExport,
};

use crate::{
  ast_printer::AstPrinter,
  doc::{Doc, RDoc},
};

use super::{class::print_class_expr, function::print_fn_expr};

pub fn print_module_decl(
  cx: &mut AstPrinter,
  module_decl: &ModuleDecl,
) -> RDoc {
  match module_decl {
    ModuleDecl::Import(import_decl) => print_import_decl(cx, import_decl),
    ModuleDecl::ExportDecl(export_decl) => print_export_decl(cx, export_decl),
    ModuleDecl::ExportNamed(named_export) => {
      print_named_export(cx, named_export)
    }
    ModuleDecl::ExportDefaultDecl(export_default_decl) => {
      print_default_export_decl(cx, export_default_decl)
    }
    ModuleDecl::ExportDefaultExpr(export_default_expr) => {
      print_export_default_expr(cx, export_default_expr)
    }
    ModuleDecl::ExportAll(export_all) => print_export_all(cx, export_all),
    ModuleDecl::TsImportEquals(_) => todo!("ts"),
    ModuleDecl::TsExportAssignment(_) => todo!("ts"),
    ModuleDecl::TsNamespaceExport(_) => todo!("ts"),
  }
}

pub fn print_import_decl(
  cx: &mut AstPrinter,
  import_decl: &ImportDecl,
) -> RDoc {
  let mut parts = vec!["import ".into()];

  if !import_decl.specifiers.is_empty() {
    let mut standalone_specifiers = vec![];
    let mut grouped_specifiers = vec![];
    for specifier in &import_decl.specifiers {
      match specifier {
        ImportSpecifier::Namespace(_) => todo!(),
        ImportSpecifier::Default(export_def_specifier) => {
          standalone_specifiers
            .push(cx.print_ident(&export_def_specifier.local));
        }
        ImportSpecifier::Named(export_named_specifier) => {
          let mut spec_parts =
            vec![cx.print_ident(&export_named_specifier.local)];
          if let Some(exported) = &export_named_specifier.imported {
            spec_parts.push(" as ".into());
            spec_parts.push(print_module_export_name(cx, exported));
          }
          grouped_specifiers.push(Doc::new_concat(spec_parts));
        }
      }
    }

    let has_standalone_specs = !standalone_specifiers.is_empty();
    parts.push(Doc::new_concat(Doc::join(
      &", ".into(),
      standalone_specifiers,
    )));

    if !grouped_specifiers.is_empty() {
      if has_standalone_specs {
        parts.push(", ".into());
      }

      // TODO: check comments
      let can_break = grouped_specifiers.len() > 1 || has_standalone_specs;

      if can_break {
        let bracket_space = if cx.bracket_spacing {
          Doc::line()
        } else {
          Doc::softline()
        };
        parts.push(Doc::group(Doc::new_concat(vec![
          "{".into(),
          Doc::new_indent(Doc::new_concat(vec![
            bracket_space.clone(),
            Doc::new_concat(Doc::join(
              &Doc::new_concat(vec![",".into(), Doc::line()]),
              grouped_specifiers,
            )),
          ])),
          Doc::new_if_break(",".into(), Doc::none(), None),
          bracket_space,
          "}".into(),
        ])));
      } else {
        let bracket_space: Doc =
          if cx.bracket_spacing { " " } else { "" }.into();
        parts.push("{".into());
        parts.push(bracket_space.clone());
        parts.extend_from_slice(&grouped_specifiers);
        parts.push(bracket_space.clone());
        parts.push("}".into());
      }
    }
  } else {
    parts.push("{}".into());
  }

  // TODO: comments

  parts.push(" from ".into());
  parts.push(cx.print_str(&import_decl.src));

  if cx.semi {
    parts.push(";".into());
  }

  Ok(Doc::new_concat(parts))
}

pub fn print_export_decl(
  cx: &mut AstPrinter,
  export_decl: &ExportDecl,
) -> RDoc {
  let mut parts = vec!["export".into()];

  // TODO: comments

  parts.push(" ".into());
  parts.push(cx.print_decl(&export_decl.decl)?);

  Ok(Doc::new_concat(parts))
}

pub fn print_default_export_decl(
  cx: &mut AstPrinter,
  export_default_decl: &ExportDefaultDecl,
) -> RDoc {
  let mut parts = vec!["export default".into()];

  // TODO: comments

  parts.push(" ".into());
  let decl_doc = match &export_default_decl.decl {
    DefaultDecl::Class(class_expr) => print_class_expr(cx, class_expr)?,
    DefaultDecl::Fn(fn_expr) => print_fn_expr(cx, fn_expr)?,
    DefaultDecl::TsInterfaceDecl(_) => todo!("ts"),
  };
  parts.push(decl_doc);

  Ok(Doc::new_concat(parts))
}

pub fn print_export_default_expr(
  cx: &mut AstPrinter,
  export_default_expr: &ExportDefaultExpr,
) -> RDoc {
  let mut parts = vec!["export default".into()];

  // TODO: comments

  parts.push(" ".into());
  parts.push(cx.print_expr(&export_default_expr.expr)?);

  if cx.semi {
    parts.push(";".into());
  }

  Ok(Doc::new_concat(parts))
}

pub fn print_named_export(
  cx: &mut AstPrinter,
  named_export: &NamedExport,
) -> RDoc {
  let mut parts = vec!["export ".into()];

  // TODO: comments

  if !named_export.specifiers.is_empty() {
    let mut standalone_specifiers = vec![];
    let mut grouped_specifiers = vec![];
    for specifier in &named_export.specifiers {
      match specifier {
        ExportSpecifier::Namespace(_) => todo!(),
        ExportSpecifier::Default(export_def_specifier) => {
          standalone_specifiers
            .push(cx.print_ident(&export_def_specifier.exported));
        }
        ExportSpecifier::Named(export_named_specifier) => {
          let mut spec_parts =
            vec![print_module_export_name(cx, &export_named_specifier.orig)];
          if let Some(exported) = &export_named_specifier.exported {
            spec_parts.push(" as ".into());
            spec_parts.push(print_module_export_name(cx, exported));
          }
          grouped_specifiers.push(Doc::new_concat(spec_parts));
        }
      }
    }

    let has_standalone_specs = !standalone_specifiers.is_empty();
    parts.push(Doc::new_concat(Doc::join(
      &", ".into(),
      standalone_specifiers,
    )));

    if !grouped_specifiers.is_empty() {
      if has_standalone_specs {
        parts.push(", ".into());
      }

      // TODO: check comments
      let can_break = grouped_specifiers.len() > 1 || has_standalone_specs;

      if can_break {
        let bracket_space = if cx.bracket_spacing {
          Doc::line()
        } else {
          Doc::softline()
        };
        parts.push(Doc::group(Doc::new_concat(vec![
          "{".into(),
          Doc::new_indent(Doc::new_concat(vec![
            bracket_space.clone(),
            Doc::new_concat(Doc::join(
              &Doc::new_concat(vec![",".into(), Doc::line()]),
              grouped_specifiers,
            )),
          ])),
          Doc::new_if_break(",".into(), Doc::none(), None),
          bracket_space,
          "}".into(),
        ])));
      } else {
        let bracket_space: Doc =
          if cx.bracket_spacing { " " } else { "" }.into();
        parts.push("{".into());
        parts.push(bracket_space.clone());
        parts.extend_from_slice(&grouped_specifiers);
        parts.push(bracket_space.clone());
        parts.push("}".into());
      }
    }
  } else {
    parts.push("{}".into());
  }

  if let Some(src) = named_export.src.as_ref() {
    parts.push(" from ".into());
    parts.push(cx.print_str(src));
  }

  if cx.semi {
    parts.push(";".into());
  }

  Ok(Doc::new_concat(parts))
}

pub fn print_export_all(cx: &mut AstPrinter, export_all: &ExportAll) -> RDoc {
  let mut parts = vec!["export".into()];

  // TODO: comments

  parts.push(" * from ".into());
  parts.push(cx.print_str(&export_all.src));

  if cx.semi {
    parts.push(";".into());
  }

  Ok(Doc::new_concat(parts))
}

fn print_module_export_name(
  cx: &mut AstPrinter,
  module_export_name: &ModuleExportName,
) -> Doc {
  match module_export_name {
    ModuleExportName::Ident(ident) => cx.print_ident(ident),
    ModuleExportName::Str(str) => cx.print_str(str),
  }
}
