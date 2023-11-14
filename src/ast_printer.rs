use std::rc::Rc;

use swc_common::{BytePos, SourceFile, Spanned};
use swc_ecma_ast::{
  CallExpr, Decl, Expr, ExprStmt, FnDecl, Lit, Module, ModuleItem, Pat,
  Program, Stmt,
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

  fn is_next_line_empty<T: Spanned>(&self, items: &[T], index: usize) -> bool {
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
    self.have_line_between_spans(cur.span_hi(), next.span_lo())
  }

  fn have_line_between_spans(&self, cur_hi: BytePos, next_lo: BytePos) -> bool {
    self.src_file.lookup_line(cur_hi).unwrap_or(0) + 1
      < self.src_file.lookup_line(next_lo).unwrap_or(0)
  }

  pub fn print_module(&mut self, module: &Module) -> anyhow::Result<Doc> {
    let mut contents = Vec::new();

    let body_len = module.body.len();
    for (i, module_item) in module.body.iter().enumerate() {
      let item = self.print_module_item(module_item)?;
      contents.push(item);

      if i != body_len - 1 {
        contents.push(Doc::new_line(true, false, false));

        if self.is_next_line_empty(&module.body, i) {
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
      Stmt::Decl(decl) => self.print_decl(decl),
      Stmt::Expr(expr_stmt) => self.print_expr_stmt(expr_stmt),
    }
  }

  fn print_decl(&mut self, decl: &Decl) -> anyhow::Result<Doc> {
    match decl {
      Decl::Class(_) => todo!(),
      Decl::Fn(fn_decl) => self.print_fn_decl(fn_decl),
      Decl::Var(_) => todo!(),
      Decl::Using(_) => todo!(),
      Decl::TsInterface(_) => todo!(),
      Decl::TsTypeAlias(_) => todo!(),
      Decl::TsEnum(_) => todo!(),
      Decl::TsModule(_) => todo!(),
    }
  }

  fn print_fn_decl(&mut self, fn_decl: &FnDecl) -> anyhow::Result<Doc> {
    let mut print_params = |fn_decl: &FnDecl| -> anyhow::Result<Doc> {
      let params = &fn_decl.function.params;
      if params.is_empty() {
        return Ok(Doc::new_concat(vec!["(".into(), ")".into()]));
      }

      let mut printed = Vec::new();
      let params_len = params.len();
      for (i, param) in params.iter().enumerate() {
        let is_last = i == params_len - 1;
        printed.push(self.print_pat(&param.pat)?);
        if !is_last {
          printed.push(",".into());
          printed.push(Doc::line());
        }
      }

      Ok(Doc::new_concat(vec![
        "(".into(),
        Doc::new_indent(Doc::new_concat(
          [&[Doc::softline()], printed.as_slice()].concat(),
        )),
        Doc::softline(),
        ")".into(),
      ]))
    };

    let doc = Doc::new_concat(vec![
      "function ".into(),
      fn_decl.ident.sym.as_str().into(),
      Doc::new_group(print_params(fn_decl)?, false, None, None),
    ]);

    Ok(doc)
  }

  fn print_pat(&mut self, pat: &Pat) -> anyhow::Result<Doc> {
    let doc = match pat {
      Pat::Ident(ident) => {
        let mut parts = Vec::new();
        parts.push(ident.id.sym.as_str().into());
        Doc::new_concat(parts)
      }
      Pat::Array(_) => todo!(),
      Pat::Rest(_) => todo!(),
      Pat::Object(_) => todo!(),
      Pat::Assign(_) => todo!(),
      Pat::Invalid(_) => todo!(),
      Pat::Expr(_) => todo!(),
    };

    Ok(doc)
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

    let args = self.print_call_expr_args(call_expr)?;

    let doc = Doc::new_concat(vec![callee_doc, args]);

    Ok(doc)
  }

  fn print_call_expr_args(
    &mut self,
    call_expr: &CallExpr,
  ) -> anyhow::Result<Doc> {
    let mut any_arg_empty_line = false;
    let args_len = call_expr.args.len();

    let all_args_broken_out = |args: Vec<Doc>| -> Doc {
      Doc::new_group(
        Doc::new_concat(vec![
          "(".into(),
          Doc::new_indent(Doc::new_concat(
            [Doc::line()].into_iter().chain(args).collect(),
          )),
          ",".into(),
          Doc::line(),
          ")".into(),
        ]),
        true,
        None,
        None,
      )
    };

    let args = call_expr
      .args
      .iter()
      .enumerate()
      .map(|(i, expr_or_spread)| {
        let is_last = i == args_len - 1;
        let expr_doc = self.print_expr(&expr_or_spread.expr)?;
        let doc = if expr_or_spread.spread.is_some() {
          Doc::new_concat(vec![Doc::new_text("...".to_string()), expr_doc])
        } else {
          expr_doc
        };
        let doc = if !is_last {
          let mut parts = vec![doc, ",".into()];

          if self.is_next_line_empty(&call_expr.args, i) {
            any_arg_empty_line = true;
            parts.push(Doc::hardline());
            parts.push(Doc::hardline());
          } else {
            parts.push(Doc::line());
          }
          Doc::Array(parts)
        } else {
          doc
        };
        Ok(doc)
      })
      .collect::<anyhow::Result<Vec<Doc>>>()?;

    if any_arg_empty_line {
      return Ok(all_args_broken_out(args));
    }

    let doc = Doc::new_group(
      Doc::new_concat(vec![
        "(".into(),
        Doc::new_indent(Doc::new_concat(
          [Doc::softline()].into_iter().chain(args).collect(),
        )),
        Doc::new_if_break(",".into(), "".into()),
        Doc::softline(),
        ")".into(),
      ])
      .into(),
      false,
      None,
      None,
    );

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
