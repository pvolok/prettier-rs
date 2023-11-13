use std::rc::Rc;

use swc_common::{SourceFile, Spanned};
use swc_ecma_ast::{
  CallExpr, Expr, ExprStmt, Lit, Module, ModuleItem, Program, Stmt,
};

use crate::doc::Doc;

pub struct DocPrinter {
  src_file: Rc<SourceFile>,
}

impl DocPrinter {
  pub fn new(src_file: Rc<SourceFile>) -> Self {
    Self { src_file }
  }
}

impl DocPrinter {
  pub fn print_program(&mut self, program: &Program) -> anyhow::Result<Doc> {
    match program {
      Program::Module(module) => self.print_module(module),
      Program::Script(_) => todo!(),
    }
  }

  pub fn print_module(&mut self, module: &Module) -> anyhow::Result<Doc> {
    let mut contents = Vec::new();

    let is_next_line_empty = {
      let src_file = self.src_file.clone();
      move |items: &[ModuleItem], index: usize| -> bool {
        let cur = if let Some(cur) = items.get(index) {
          cur
        } else {
          return false;
        };
        let next = if let Some(next) = items.get(index + 1) {
          next
        } else {
          return false;
        };

        src_file.lookup_line(cur.span_hi()).unwrap_or(0) + 1
          < src_file.lookup_line(next.span_lo()).unwrap_or(0)
      }
    };

    let body_len = module.body.len();
    for (i, module_item) in module.body.iter().enumerate() {
      let item = self.print_module_item(module_item)?;
      contents.push(item);

      if i != body_len - 1 {
        contents.push(Doc::new_line(true, false, false));

        if is_next_line_empty(&module.body, i) {
          contents.push(Doc::new_line(true, false, false));
        }
      }
    }

    let doc = Doc::new_concat(contents);

    Ok(doc)
  }

  fn print_module_item(
    &mut self,
    module_item: &ModuleItem,
  ) -> anyhow::Result<Doc> {
    match module_item {
      ModuleItem::ModuleDecl(_) => todo!(),
      ModuleItem::Stmt(stmt) => self.print_stmt(stmt),
    }
  }

  fn print_stmt(&mut self, stmt: &Stmt) -> anyhow::Result<Doc> {
    match stmt {
      Stmt::Block(_) => todo!(),
      Stmt::Empty(_) => todo!(),
      Stmt::Debugger(_) => todo!(),
      Stmt::With(_) => todo!(),
      Stmt::Return(_) => todo!(),
      Stmt::Labeled(_) => todo!(),
      Stmt::Break(_) => todo!(),
      Stmt::Continue(_) => todo!(),
      Stmt::If(_) => todo!(),
      Stmt::Switch(_) => todo!(),
      Stmt::Throw(_) => todo!(),
      Stmt::Try(_) => todo!(),
      Stmt::While(_) => todo!(),
      Stmt::DoWhile(_) => todo!(),
      Stmt::For(_) => todo!(),
      Stmt::ForIn(_) => todo!(),
      Stmt::ForOf(_) => todo!(),
      Stmt::Decl(_) => todo!(),
      Stmt::Expr(expr_stmt) => self.print_expr_stmt(expr_stmt),
    }
  }

  fn print_expr_stmt(&mut self, expr_stmt: &ExprStmt) -> anyhow::Result<Doc> {
    Ok(Doc::new_concat(vec![
      self.print_expr(&expr_stmt.expr)?,
      ";".into(),
    ]))
  }

  fn print_expr(&mut self, expr: &Expr) -> anyhow::Result<Doc> {
    let doc = match expr {
      swc_ecma_ast::Expr::This(_) => todo!(),
      swc_ecma_ast::Expr::Array(_) => todo!(),
      swc_ecma_ast::Expr::Object(_) => todo!(),
      swc_ecma_ast::Expr::Fn(_) => todo!(),
      swc_ecma_ast::Expr::Unary(_) => todo!(),
      swc_ecma_ast::Expr::Update(_) => todo!(),
      swc_ecma_ast::Expr::Bin(_) => todo!(),
      swc_ecma_ast::Expr::Assign(_) => todo!(),
      swc_ecma_ast::Expr::Member(_) => todo!(),
      swc_ecma_ast::Expr::SuperProp(_) => todo!(),
      swc_ecma_ast::Expr::Cond(_) => todo!(),
      swc_ecma_ast::Expr::Call(call_expr) => self.print_call_expr(call_expr)?,
      swc_ecma_ast::Expr::New(_) => todo!(),
      swc_ecma_ast::Expr::Seq(_) => todo!(),
      swc_ecma_ast::Expr::Ident(ident) => Doc::new_text(ident.sym.to_string()),
      swc_ecma_ast::Expr::Lit(lit) => self.print_lit(lit)?,
      swc_ecma_ast::Expr::Tpl(_) => todo!(),
      swc_ecma_ast::Expr::TaggedTpl(_) => todo!(),
      swc_ecma_ast::Expr::Arrow(_) => todo!(),
      swc_ecma_ast::Expr::Class(_) => todo!(),
      swc_ecma_ast::Expr::Yield(_) => todo!(),
      swc_ecma_ast::Expr::MetaProp(_) => todo!(),
      swc_ecma_ast::Expr::Await(_) => todo!(),
      swc_ecma_ast::Expr::Paren(_) => todo!(),
      swc_ecma_ast::Expr::JSXMember(_) => todo!(),
      swc_ecma_ast::Expr::JSXNamespacedName(_) => todo!(),
      swc_ecma_ast::Expr::JSXEmpty(_) => todo!(),
      swc_ecma_ast::Expr::JSXElement(_) => todo!(),
      swc_ecma_ast::Expr::JSXFragment(_) => todo!(),
      swc_ecma_ast::Expr::TsTypeAssertion(_) => todo!(),
      swc_ecma_ast::Expr::TsConstAssertion(_) => todo!(),
      swc_ecma_ast::Expr::TsNonNull(_) => todo!(),
      swc_ecma_ast::Expr::TsAs(_) => todo!(),
      swc_ecma_ast::Expr::TsInstantiation(_) => todo!(),
      swc_ecma_ast::Expr::TsSatisfies(_) => todo!(),
      swc_ecma_ast::Expr::PrivateName(_) => todo!(),
      swc_ecma_ast::Expr::OptChain(_) => todo!(),
      swc_ecma_ast::Expr::Invalid(_) => todo!(),
    };

    Ok(doc)
  }

  fn print_call_expr(&mut self, call_expr: &CallExpr) -> anyhow::Result<Doc> {
    let callee_doc = match &call_expr.callee {
      swc_ecma_ast::Callee::Super(_) => Doc::Text("super".to_string()),
      swc_ecma_ast::Callee::Import(_) => Doc::Text("import".to_string()),
      swc_ecma_ast::Callee::Expr(expr) => self.print_expr(expr)?,
    };

    let args_len = call_expr.args.len();
    let args = call_expr
      .args
      .iter()
      .enumerate()
      .map(|(i, expr_or_spread)| {
        let expr_doc = self.print_expr(&expr_or_spread.expr)?;
        let doc = if expr_or_spread.spread.is_some() {
          Doc::new_concat(vec![Doc::new_text("...".to_string()), expr_doc])
        } else {
          expr_doc
        };
        let doc = if i != args_len - 1 {
          Doc::Array(vec![
            doc,
            Doc::Text(",".to_string()),
            Doc::Line {
              hard: false,
              soft: false,
              literal: false,
            },
          ])
        } else {
          doc
        };
        Ok(doc)
      })
      .collect::<anyhow::Result<Vec<Doc>>>()?;

    let doc = Doc::new_concat(vec![
      callee_doc,
      Doc::new_text("(".to_string()),
      Doc::new_concat(args),
      Doc::new_text(")".to_string()),
    ]);

    let doc = Doc::new_group(Box::new(doc), false, None, None);

    Ok(doc)
  }

  fn print_lit(&mut self, lit: &Lit) -> anyhow::Result<Doc> {
    let doc = match lit {
      Lit::Str(str) => Doc::new_text(str.value.to_string()),
      Lit::Bool(bool) => Doc::new_text(format!("{:?}", bool)),
      Lit::Null(_) => Doc::new_text(format!("null")),
      Lit::Num(num) => Doc::new_text(format!("{:?}", num.value)),
      Lit::BigInt(_) => todo!(),
      Lit::Regex(_) => todo!(),
      Lit::JSXText(_) => todo!(),
    };

    Ok(doc)
  }
}
