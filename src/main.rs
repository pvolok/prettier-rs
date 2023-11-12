use std::path::PathBuf;

use doc_printer::{print_doc, DocWriter};
use swc_common::{sync::Lrc, SourceMap};
use swc_ecma_parser::EsConfig;

use crate::ast_printer::DocPrinter;

mod ast_printer;
mod doc;
mod doc_printer;

fn main() {
  let es_config = EsConfig::default();

  let mut errors = Vec::new();

  let cm: Lrc<SourceMap> = Default::default();
  let path = PathBuf::from("example.js");
  let src_file = cm.load_file(&path).unwrap();

  let ast = swc_ecma_parser::parse_file_as_module(
    &src_file,
    swc_ecma_parser::Syntax::Es(es_config),
    swc_ecma_ast::EsVersion::EsNext,
    None,
    &mut errors,
  )
  .unwrap();

  let mut printer = DocPrinter::new(src_file.clone());
  let doc = printer.print_module(&ast).unwrap();

  println!("DOC:\n{:?}\n", doc);

  let mut out_str = String::new();
  let mut out = DocWriter::String(&mut out_str);
  print_doc(&mut out, &src_file, &doc).unwrap();

  println!("OUT:\n{}", out_str);
}
