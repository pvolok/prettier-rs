#![feature(slice_take)]

use std::path::PathBuf;

use doc_printer::{print_doc, DocWriter};
use swc_common::{comments::SingleThreadedComments, sync::Lrc, SourceMap};
use swc_ecma_parser::EsConfig;

use crate::ast_printer::AstPrinter;

mod ast_printer;
mod doc;
mod doc_printer;
mod print_js;

fn main() {
  let es_config = EsConfig::default();

  let mut errors = Vec::new();

  let cm: Lrc<SourceMap> = Default::default();
  let path = PathBuf::from("example.js");
  let src_file = cm.load_file(&path).unwrap();

  let comments = SingleThreadedComments::default();
  let module_ast = swc_ecma_parser::parse_file_as_module(
    &src_file,
    swc_ecma_parser::Syntax::Es(es_config),
    swc_ecma_ast::EsVersion::EsNext,
    Some(&comments),
    &mut errors,
  )
  .unwrap();

  println!("COMMENTS:\n{:#?}\n", comments);

  let mut printer = AstPrinter::new(src_file.clone(), comments);
  let doc = printer.print_module(&module_ast).unwrap();

  println!("DOC:\n{:#?}\n", doc);

  let mut out_str = String::new();
  let mut out = DocWriter::String(&mut out_str);
  print_doc(&mut out, &src_file, &doc).unwrap();

  println!("OUT:\n{}", out_str);
}
