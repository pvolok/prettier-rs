#![feature(concat_idents)]
#![feature(iter_intersperse)]
#![feature(slice_take)]

use std::{io::Read, path::PathBuf};

use doc_printer::{print_doc, DocWriter};
use swc_common::{comments::SingleThreadedComments, sync::Lrc, SourceMap};
use swc_ecma_parser::EsConfig;

use crate::{
  ast_path::fake_path, ast_printer::AstPrinter, ast_util::clean_ast,
};

mod ast_path;
mod ast_printer;
mod ast_util;
mod doc;
mod doc_printer;
mod print_js;

fn main() {
  let es_config = EsConfig {
    jsx: true,
    fn_bind: true,
    decorators: true,
    ..Default::default()
  };

  let mut errors = Vec::new();

  let cm: Lrc<SourceMap> = Default::default();
  let comments = SingleThreadedComments::default();

  let arg1 = std::env::args().skip(1).next().unwrap_or_default();
  let src_file = if arg1 == "-" {
    let mut src = String::new();
    std::io::stdin().read_to_string(&mut src).unwrap();
    cm.new_source_file(swc_common::FileName::Anon, src)
  } else {
    let path = PathBuf::from(arg1);
    cm.load_file(&path).unwrap()
  };

  let module_ast = swc_ecma_parser::parse_file_as_module(
    &src_file,
    swc_ecma_parser::Syntax::Es(es_config),
    swc_ecma_ast::EsVersion::EsNext,
    Some(&comments),
    &mut errors,
  )
  .unwrap();

  // Remove parens from ast
  let module_ast = clean_ast(module_ast);

  let module_path = fake_path(&module_ast);

  // println!("COMMENTS:\n{:#?}\n", comments);

  let mut printer = AstPrinter::new(cm, src_file.clone(), comments);
  let doc = printer.print_module(module_path).unwrap();

  if std::env::var("DOC").is_ok() {
    println!("DOC:\n{}\n", doc.debug());
  }

  let mut out_str = String::new();
  let mut out = DocWriter::String(&mut out_str);
  print_doc(&mut out, &src_file, &doc).unwrap();

  // println!("OUT:\n{}", out_str);
  print!("{}", out_str);
}
