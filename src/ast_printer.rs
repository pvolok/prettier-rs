use std::rc::Rc;

use swc_common::{
  comments::SingleThreadedComments, BytePos, SourceFile, Spanned,
};
use swc_ecma_ast::{
  ArrayLit, AssignExpr, BlockStmt, CallExpr, Decl, Expr, ExprOrSpread,
  ExprStmt, ForHead, ForOfStmt, ForStmt, Lit, MemberExpr, MemberProp, Module,
  ModuleItem, NewExpr, ObjectLit, Pat, PatOrExpr, Program, Prop, PropName,
  PropOrSpread, Stmt, Tpl, VarDecl, VarDeclKind, VarDeclOrExpr, VarDeclarator,
  YieldExpr,
};
use swc_ecma_visit::{
  fields::{AssignExprField, BlockStmtField, ForStmtField},
  AstParentKind,
};

use crate::{
  doc::{Doc, GroupId},
  doc_printer::{print_doc, string_width, DocWriter},
  print_js::{
    assign::{print_assignment, AssignmentLeft},
    bin_expr::print_bin_expr,
    comments::print_dangling_comments,
    function::{print_arrow_expr, print_fn_decl, print_fn_expr},
  },
};

pub struct AstPrinter {
  src_file: Rc<SourceFile>,
  comments: SingleThreadedComments,
  last_group_id: usize,

  pub stack: Vec<AstParentKind>,

  pub tab_width: i32,
  pub print_width: i32,
}

impl AstPrinter {
  pub fn new(
    src_file: Rc<SourceFile>,
    comments: SingleThreadedComments,
  ) -> Self {
    Self {
      src_file,
      comments,
      last_group_id: 0,

      stack: Vec::new(),

      tab_width: 2,
      print_width: 80,
    }
  }
}

impl AstPrinter {
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

  pub fn group_id(&mut self, name: &'static str) -> GroupId {
    self.last_group_id += 1;
    GroupId(self.last_group_id, name)
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

  pub fn push(&mut self, parent: AstParentKind) {
    self.stack.push(parent);
  }

  pub fn pop(&mut self) {
    self.stack.pop();
  }

  fn print_stmt(&mut self, stmt: &Stmt) -> anyhow::Result<Doc> {
    match stmt {
      Stmt::Block(block_stmt) => self.print_block_stmt(block_stmt, true),
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
      Stmt::For(for_stmt) => self.print_for_stmt(for_stmt),
      Stmt::ForIn(_) => todo!(),
      Stmt::ForOf(for_of_stmt) => self.print_for_of_stmt(for_of_stmt),
      Stmt::Decl(decl) => self.print_decl(decl),
      Stmt::Expr(expr_stmt) => self.print_expr_stmt(expr_stmt),
    }
  }

  pub fn print_block_stmt(
    &mut self,
    block_stmt: &BlockStmt,
    empty_hardline: bool,
  ) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();

    parts.push("{".into());

    let mut body_parts = Vec::new();
    for (i, stmt) in block_stmt.stmts.iter().enumerate() {
      self.push(AstParentKind::BlockStmt(BlockStmtField::Stmts(i)));
      let is_last = i == block_stmt.stmts.len() - 1;

      body_parts.push(self.print_stmt(stmt)?);
      if !is_last {
        body_parts.push(Doc::hardline());
        if self.is_next_line_empty(&block_stmt.stmts, i) {
          body_parts.push(Doc::hardline());
        }
      }

      self.pop();
    }
    if !body_parts.is_empty() {
      parts.push(Doc::new_indent(Doc::new_concat(
        [Doc::hardline()].into_iter().chain(body_parts).collect(),
      )));
      parts.push(Doc::hardline());
    } else if empty_hardline {
      parts.push(Doc::hardline());
    }

    parts.push("}".into());

    Ok(Doc::new_concat(parts))
  }

  fn print_for_stmt(&mut self, for_stmt: &ForStmt) -> anyhow::Result<Doc> {
    self.push(AstParentKind::ForStmt(ForStmtField::Body));
    let body_doc = match for_stmt.body.as_ref() {
      Stmt::Block(_) => {
        Doc::new_concat(vec![" ".into(), self.print_stmt(&for_stmt.body)?])
      }
      Stmt::Empty(_) => ";".into(),
      _ => Doc::new_indent(Doc::new_concat(vec![
        Doc::line(),
        self.print_stmt(&for_stmt.body)?,
      ])),
    };
    self.pop();

    if for_stmt.init.is_none()
      && for_stmt.test.is_none()
      && for_stmt.update.is_none()
    {
      let doc = Doc::new_group(
        Doc::new_concat(vec!["for (;;)".into(), body_doc]),
        false,
        None,
        None,
      );
      return Ok(doc);
    }

    self.push(AstParentKind::ForStmt(ForStmtField::Init));
    let init_doc = if let Some(init) = &for_stmt.init {
      match init {
        VarDeclOrExpr::VarDecl(var_decl) => self.print_var_decl(var_decl, true),
        VarDeclOrExpr::Expr(expr) => self.print_expr(expr),
      }?
    } else {
      "".into()
    };
    self.pop();

    self.push(AstParentKind::ForStmt(ForStmtField::Test));
    let test_doc = for_stmt
      .test
      .as_ref()
      .map(|e| self.print_expr(&e))
      .transpose()?
      .unwrap_or_else(|| "".into());
    self.pop();

    self.push(AstParentKind::ForStmt(ForStmtField::Update));
    let update_doc = for_stmt
      .update
      .as_ref()
      .map(|e| self.print_expr(&e))
      .transpose()?
      .unwrap_or_else(|| "".into());
    self.pop();

    let doc = Doc::new_group(
      Doc::new_concat(vec![
        "for (".into(),
        Doc::new_group(
          Doc::new_concat(vec![
            Doc::new_indent(Doc::new_concat(vec![
              Doc::softline(),
              init_doc,
              ";".into(),
              Doc::line(),
              test_doc,
              ";".into(),
              Doc::line(),
              update_doc,
            ])),
            Doc::softline(),
          ]),
          false,
          None,
          None,
        ),
        ")".into(),
        body_doc,
      ]),
      false,
      None,
      None,
    );
    Ok(doc)
  }

  fn print_for_of_stmt(
    &mut self,
    for_of_stmt: &ForOfStmt,
  ) -> anyhow::Result<Doc> {
    let body_doc = match for_of_stmt.body.as_ref() {
      Stmt::Block(_) => {
        Doc::new_concat(vec![" ".into(), self.print_stmt(&for_of_stmt.body)?])
      }
      Stmt::Empty(_) => ";".into(),
      _ => Doc::new_indent(Doc::new_concat(vec![
        Doc::line(),
        self.print_stmt(&for_of_stmt.body)?,
      ])),
    };

    let maybe_await = if for_of_stmt.is_await { " await" } else { "" }.into();

    let left_doc = match &for_of_stmt.left {
      ForHead::VarDecl(var_decl) => self.print_var_decl(var_decl, true),
      ForHead::UsingDecl(using_decl) => todo!(),
      ForHead::Pat(pat) => self.print_pat(pat),
    }?;

    let doc = Doc::new_group(
      Doc::new_concat(vec![
        "for".into(),
        maybe_await,
        " (".into(),
        left_doc,
        " of ".into(),
        self.print_expr(&for_of_stmt.right)?,
        ")".into(),
        body_doc,
      ]),
      false,
      None,
      None,
    );
    Ok(doc)
  }

  fn print_decl(&mut self, decl: &Decl) -> anyhow::Result<Doc> {
    match decl {
      Decl::Class(_) => todo!(),
      Decl::Fn(fn_decl) => print_fn_decl(self, fn_decl),
      Decl::Var(var_decl) => self.print_var_decl(var_decl, false),
      Decl::Using(_) => todo!(),
      Decl::TsInterface(_) => todo!(),
      Decl::TsTypeAlias(_) => todo!(),
      Decl::TsEnum(_) => todo!(),
      Decl::TsModule(_) => todo!(),
    }
  }

  fn print_var_decl(
    &mut self,
    var_decl: &VarDecl,
    in_for: bool,
  ) -> anyhow::Result<Doc> {
    let decls = var_decl
      .decls
      .iter()
      .map(|dtor| self.print_var_declarator(dtor))
      .collect::<anyhow::Result<Vec<Doc>>>()?;

    let mut parts = Vec::new();

    let kind = match var_decl.kind {
      VarDeclKind::Var => "var",
      VarDeclKind::Let => "let",
      VarDeclKind::Const => "const",
    };
    parts.push(kind.into());

    if let Some((first, rest)) = decls.split_first() {
      let has_value = var_decl.decls.iter().any(|decl| decl.init.is_some());

      parts.push(" ".into());
      parts.push(first.clone());

      parts.push(Doc::new_indent(Doc::new_concat(
        rest
          .into_iter()
          .map(|decl| {
            Doc::new_concat(vec![
              ",".into(),
              if has_value {
                Doc::hardline()
              } else {
                Doc::line()
              },
              decl.clone(),
            ])
          })
          .collect(),
      )));
    }

    if !in_for {
      parts.push(";".into());
    }

    let doc = Doc::new_group(Doc::new_concat(parts), false, None, None);
    Ok(doc)
  }

  fn print_var_declarator(
    &mut self,
    var_declarator: &VarDeclarator,
  ) -> anyhow::Result<Doc> {
    let left_doc = self.print_pat(&var_declarator.name)?;

    fn calc_doc_width(doc: &Doc) -> i32 {
      match doc {
        Doc::Align(doc, _) => calc_doc_width(doc),
        Doc::Group { contents, .. } => calc_doc_width(&contents),
        Doc::Fill { items, .. } => items.iter().map(calc_doc_width).sum(),
        Doc::IfBreak { flat_doc, .. } => calc_doc_width(&flat_doc),
        Doc::Indent(doc) => calc_doc_width(doc),
        Doc::IndentIfBreak { contents, .. } => calc_doc_width(&contents),
        Doc::LineSuffix(_) => todo!(),
        Doc::LineSuffixBoundary => todo!(),
        Doc::BreakParent => todo!(),
        Doc::Trim => todo!(),
        Doc::Line { soft, .. } => {
          if *soft {
            0
          } else {
            1
          }
        }
        Doc::Cursor => todo!(),
        Doc::Label(_, _) => todo!(),
        Doc::Text(s) => string_width(s),
        Doc::Array(elems) => elems.iter().map(calc_doc_width).sum(),
      }
    }

    if let Some(init) = &var_declarator.init {
      let right_doc = self.print_expr(init)?;

      const MIN_OVERLAP_FOR_BREAK: i32 = 3;
      //   ↓↓ - insufficient overlap for a line break
      // key1: longValue1,
      //   ↓↓↓↓↓↓ - overlap is long enough to break
      // key2abcd:
      //   longValue2
      let has_short_key =
        calc_doc_width(&left_doc) < self.tab_width + MIN_OVERLAP_FOR_BREAK;

      let should_break_after_operator = match init.as_ref() {
        Expr::Array(array_lit) if array_lit.elems.len() > 0 => false,
        Expr::Object(object_lit) if object_lit.props.len() > 0 => false,
        _ => true,
      };
      if should_break_after_operator {
        // return group([group(leftDoc), operator, group(indent([line, rightDoc]))]);
        let doc = Doc::new_group(
          Doc::new_concat(vec![
            Doc::new_group(left_doc, false, None, None),
            " =".into(),
            Doc::new_group(
              Doc::new_indent(Doc::new_concat(vec![Doc::line(), right_doc])),
              false,
              None,
              None,
            ),
          ]),
          false,
          None,
          None,
        );
        return Ok(doc);
      }

      let group_id = self.group_id("assignment");
      let doc = Doc::new_group(
        Doc::new_concat(vec![
          Doc::new_group(left_doc, false, None, None),
          " =".into(),
          Doc::new_group(
            Doc::new_indent(Doc::line()),
            false,
            None,
            Some(group_id),
          ),
          // TODO lineSuffixBoundary
          Doc::new_indent_if_break(right_doc, Some(group_id), false),
        ]),
        false,
        None,
        None,
      );
      Ok(doc)
    } else {
      Ok(left_doc)
    }
  }

  pub fn print_pat(&mut self, pat: &Pat) -> anyhow::Result<Doc> {
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
    let mut parts = Vec::new();

    parts.push(self.print_expr(&expr_stmt.expr)?);
    parts.push(";".into());

    if self
      .comments
      .with_trailing(expr_stmt.span_hi(), |comments| !comments.is_empty())
    {
      parts.push(" ".into());
      self.comments.with_trailing(
        expr_stmt.span_hi(),
        |comments| -> anyhow::Result<()> {
          parts.push(print_dangling_comments(comments)?);
          Ok(())
        },
      )?;
    }

    Ok(Doc::new_concat(parts))
  }

  pub fn print_expr(&mut self, expr: &Expr) -> anyhow::Result<Doc> {
    let doc = match expr {
      Expr::This(_this_expr) => "this".into(),
      Expr::Array(array_lit) => self.print_array_lit(array_lit)?,
      Expr::Object(object_lit) => self.print_object_lit(object_lit)?,
      Expr::Fn(fn_expr) => print_fn_expr(self, fn_expr)?,
      Expr::Unary(_) => todo!(),
      Expr::Update(_) => todo!(),
      Expr::Bin(bin_expr) => print_bin_expr(self, bin_expr)?,
      Expr::Assign(assign_expr) => self.print_assign_expr(assign_expr)?,
      Expr::Member(member_expr) => self.print_member_expr(member_expr)?,
      Expr::SuperProp(super_props_expr) => todo!(),
      Expr::Cond(_) => todo!(),
      Expr::Call(call_expr) => self.print_call_expr(call_expr)?,
      Expr::New(new_expr) => self.print_new_expr(new_expr)?,
      Expr::Seq(_) => todo!(),
      Expr::Ident(ident) => Doc::new_text(ident.sym.to_string()),
      Expr::Lit(lit) => self.print_lit(lit)?,
      Expr::Tpl(tpl) => self.print_tpl(tpl)?,
      Expr::TaggedTpl(_) => todo!(),
      Expr::Arrow(arrow_expr) => print_arrow_expr(self, arrow_expr, None)?,
      Expr::Class(_) => todo!(),
      Expr::Yield(yield_expr) => self.print_yield_expr(yield_expr)?,
      Expr::MetaProp(_) => todo!(),
      Expr::Await(_) => todo!(),
      Expr::Paren(paren_expr) => self.print_expr(&paren_expr.expr)?,
      Expr::JSXMember(_) => todo!(),
      Expr::JSXNamespacedName(_) => todo!(),
      Expr::JSXEmpty(_) => todo!(),
      Expr::JSXElement(_) => todo!(),
      Expr::JSXFragment(_) => todo!(),
      Expr::TsTypeAssertion(_) => todo!(),
      Expr::TsConstAssertion(_) => todo!(),
      Expr::TsNonNull(_) => todo!(),
      Expr::TsAs(_) => todo!(),
      Expr::TsInstantiation(_) => todo!(),
      Expr::TsSatisfies(_) => todo!(),
      Expr::PrivateName(_) => todo!(),
      Expr::OptChain(_) => todo!(),
      Expr::Invalid(_) => todo!(),
    };

    Ok(doc)
  }

  fn print_array_lit(&mut self, array_lit: &ArrayLit) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();

    let open_bracket = "[";
    let close_bracket = "]";

    if array_lit.elems.is_empty() {
      parts.push(open_bracket.into());
      parts.push(close_bracket.into());
    } else {
      // We can unwrap because array is not empty.
      let last_elem = array_lit.elems.last().unwrap();
      let can_have_trailing_comma =
        last_elem.as_ref().map_or(true, |el| el.spread.is_none());

      // JavaScript allows you to have empty elements in an array which
      // changes its length based on the number of commas. The algorithm
      // is that if the last argument is null, we need to force insert
      // a comma to ensure JavaScript recognizes it.
      //   [,].length === 1
      //   [1,].length === 1
      //   [1,,].length === 2
      let needs_forced_trailing_comma = last_elem.is_none();

      let group_id = self.group_id("array");

      // TODO
      let should_break = false;

      let should_use_concise_formatting = array_lit.elems.iter().all(|el| {
        el.as_ref().map_or(false, |el| {
          matches!(el.expr.as_ref(), Expr::Lit(Lit::Num(_)))
        })
      });

      let trailing_comma = if !can_have_trailing_comma {
        "".into()
      } else if needs_forced_trailing_comma {
        ",".into()
      } else if should_use_concise_formatting {
        Doc::new_if_break(",".into(), "".into(), Some(group_id))
      } else {
        Doc::new_if_break(",".into(), "".into(), None)
      };

      let elems_doc = if should_use_concise_formatting {
        let mut elems_parts = Vec::new();
        for (i, elem) in array_lit.elems.iter().enumerate() {
          let is_last = i == array_lit.elems.len() - 1;

          let elem_doc = if let Some(elem) = elem {
            self.print_expr(&elem.expr)?
          } else {
            Doc::from("")
          };
          let comma = if is_last {
            trailing_comma.clone()
          } else {
            ",".into()
          };
          elems_parts.push(Doc::new_concat(vec![elem_doc, comma]));

          if !is_last {
            if self.is_next_line_empty(&array_lit.elems, i) {
              elems_parts
                .push(Doc::new_concat(vec![Doc::hardline(), Doc::hardline()]));
            } else {
              elems_parts.push(Doc::line());
            }
          }
        }

        Doc::new_fill(elems_parts)
      } else {
        let mut elems_parts = Vec::new();
        for (i, elem) in array_lit.elems.iter().enumerate() {
          let is_last = i == array_lit.elems.len() - 1;

          let mut elem_parts = Vec::new();
          if let Some(elem) = elem {
            if elem.spread.is_some() {
              elem_parts.push("...".into());
            }
            elem_parts.push(self.print_expr(&elem.expr)?);
          }
          elems_parts.push(Doc::new_group(
            Doc::new_concat(elem_parts),
            false,
            None,
            None,
          ));

          if !is_last {
            elems_parts.push(",".into());
            elems_parts.push(Doc::line());
            if self.is_next_line_empty(&array_lit.elems, i) {
              elems_parts.push(Doc::softline());
            }
          }
        }

        elems_parts.push(trailing_comma);

        Doc::new_concat(elems_parts)
      };

      parts.push(Doc::new_group(
        Doc::new_concat(vec![
          open_bracket.into(),
          Doc::new_indent(Doc::new_concat(vec![Doc::softline(), elems_doc])),
          Doc::softline(),
          close_bracket.into(),
        ]),
        should_break,
        None,
        Some(group_id),
      ))
    }

    Ok(Doc::new_concat(parts))
  }

  fn print_object_lit(
    &mut self,
    object_lit: &ObjectLit,
  ) -> anyhow::Result<Doc> {
    let separator = Doc::from(",");

    let mut is_prev_line_empty = false;

    let props = object_lit
      .props
      .iter()
      .enumerate()
      .map(|(i, prop)| {
        let prop_doc = match prop {
          PropOrSpread::Spread(_) => todo!(),
          PropOrSpread::Prop(prop) => match prop.as_ref() {
            Prop::Shorthand(ident) => ident.sym.as_str().into(),
            Prop::KeyValue(key_value_prop) => {
              let key: Doc = match &key_value_prop.key {
                PropName::Ident(ident) => ident.sym.as_str().into(),
                PropName::Str(_) => todo!(),
                PropName::Num(_) => todo!(),
                PropName::Computed(_) => todo!(),
                PropName::BigInt(_) => todo!(),
              };
              let value = self.print_expr(&key_value_prop.value)?;

              let group_id = self.group_id("assigment");
              Doc::new_group(
                Doc::new_concat(vec![
                  key,
                  ":".into(),
                  Doc::new_group(Doc::line(), false, None, Some(group_id)),
                  Doc::new_indent_if_break(value, Some(group_id), false),
                ]),
                false,
                None,
                None,
              )
            }
            Prop::Assign(_) => todo!(),
            Prop::Getter(_) => todo!(),
            Prop::Setter(_) => todo!(),
            Prop::Method(_) => todo!(),
          },
        };

        let mut prop_parts = Vec::new();
        if i > 0 {
          prop_parts.push(separator.clone());
          prop_parts.push(Doc::line());
          if is_prev_line_empty {
            prop_parts.push(Doc::hardline());
          }
        }
        prop_parts.push(Doc::new_group(prop_doc, false, None, None));

        is_prev_line_empty = self.is_next_line_empty(&object_lit.props, i);

        Ok(Doc::new_concat(prop_parts))
      })
      .collect::<anyhow::Result<Vec<Doc>>>()?;

    let parts = if props.is_empty() {
      vec!["{".into(), "}".into()]
    } else {
      vec![
        "{".into(),
        Doc::new_indent(Doc::new_concat(
          [Doc::line()].into_iter().chain(props).collect(),
        )),
        Doc::new_if_break(",".into(), "".into(), None),
        Doc::line(),
        "}".into(),
      ]
    };

    Ok(Doc::new_concat(parts))
  }

  fn print_assign_expr(
    &mut self,
    assign_expr: &AssignExpr,
  ) -> anyhow::Result<Doc> {
    self.push(AstParentKind::AssignExpr(AssignExprField::Left));
    let left_doc = match &assign_expr.left {
      PatOrExpr::Expr(expr) => self.print_expr(&expr)?,
      PatOrExpr::Pat(pat) => self.print_pat(&pat)?,
    };
    self.pop();

    let op_doc =
      Doc::new_concat(vec![" ".into(), assign_expr.op.as_str().into()]);

    return print_assignment(
      self,
      left_doc,
      AssignmentLeft::PatOrExpr(&assign_expr.left),
      op_doc,
      &assign_expr.right,
    );

    let is_assignment = true;

    self.push(AstParentKind::AssignExpr(AssignExprField::Right));
    let right_doc = self.print_expr(&assign_expr.right)?;
    self.pop();

    let should_break_after_operator = match assign_expr.right.as_ref() {
      Expr::Array(array_lit) if array_lit.elems.len() > 0 => false,
      Expr::Object(object_lit) if object_lit.props.len() > 0 => false,
      _ => true,
    };
    if should_break_after_operator {
      let doc = Doc::new_group(
        Doc::new_concat(vec![
          Doc::new_group(left_doc, false, None, None),
          " =".into(),
          Doc::new_group(
            Doc::new_indent(Doc::new_concat(vec![Doc::line(), right_doc])),
            false,
            None,
            None,
          ),
        ]),
        false,
        None,
        None,
      );
      return Ok(doc);
    }

    let group_id = self.group_id("assignment");
    let doc = Doc::new_group(
      Doc::new_concat(vec![
        Doc::new_group(left_doc, false, None, None),
        " =".into(),
        Doc::new_group(
          Doc::new_indent(Doc::line()),
          false,
          None,
          Some(group_id),
        ),
        // TODO lineSuffixBoundary
        Doc::new_indent_if_break(right_doc, Some(group_id), false),
      ]),
      false,
      None,
      None,
    );
    Ok(doc)
  }

  fn print_member_expr(
    &mut self,
    member_expr: &MemberExpr,
  ) -> anyhow::Result<Doc> {
    let obj = self.print_expr(&member_expr.obj)?;
    let lookup = match &member_expr.prop {
      MemberProp::Ident(ident) => {
        Doc::new_concat(vec![".".into(), ident.sym.as_str().into()])
      }
      MemberProp::PrivateName(_) => todo!(),
      MemberProp::Computed(prop) => {
        let prop_doc = self.print_expr(&prop.expr)?;
        match prop.expr.as_ref() {
          Expr::Lit(Lit::Num(_)) => {
            Doc::new_concat(vec!["[".into(), prop_doc, "]".into()])
          }
          _ => Doc::new_group(
            Doc::new_concat(vec![
              "[".into(),
              Doc::new_indent(Doc::new_concat(vec![Doc::softline(), prop_doc])),
              Doc::softline(),
              "]".into(),
            ]),
            false,
            None,
            None,
          ),
        }
      }
    };

    Ok(Doc::new_concat(vec![obj, lookup]))
  }

  fn print_call_expr(&mut self, call_expr: &CallExpr) -> anyhow::Result<Doc> {
    let callee_doc = match &call_expr.callee {
      swc_ecma_ast::Callee::Super(_) => "super".into(),
      swc_ecma_ast::Callee::Import(_) => "import".into(),
      swc_ecma_ast::Callee::Expr(expr) => self.print_expr(expr)?,
    };

    let args = self.print_call_expr_args(&call_expr.args)?;

    let doc = Doc::new_concat(vec![callee_doc, args]);

    Ok(doc)
  }

  fn print_call_expr_args(
    &mut self,
    args: &[ExprOrSpread],
  ) -> anyhow::Result<Doc> {
    let mut any_arg_empty_line = false;
    let args_len = args.len();

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

    let args = args
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

          if self.is_next_line_empty(args, i) {
            any_arg_empty_line = true;
            parts.push(Doc::hardline());
            parts.push(Doc::hardline());
          } else {
            parts.push(Doc::line());
          }
          Doc::new_concat(parts)
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
        Doc::new_if_break(",".into(), "".into(), None),
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

  fn print_new_expr(&mut self, new_expr: &NewExpr) -> anyhow::Result<Doc> {
    let callee_doc = self.print_expr(&new_expr.callee)?;

    let args = if let Some(args) = &new_expr.args {
      self.print_call_expr_args(args)?
    } else {
      self.print_call_expr_args(&[])?
    };

    let doc = Doc::new_concat(vec!["new ".into(), callee_doc, args]);

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

  fn print_tpl(&mut self, tpl: &Tpl) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();

    let expr_docs = tpl
      .exprs
      .iter()
      .map(|expr| self.print_expr(expr))
      .collect::<anyhow::Result<Vec<Doc>>>()?;

    let is_simple = true;

    let expr_docs = if is_simple {
      expr_docs
        .into_iter()
        .map(|doc| {
          let mut out_str = String::new();
          let mut out = DocWriter::String(&mut out_str);
          print_doc(&mut out, &self.src_file, &doc).unwrap();
          Doc::new_text(out_str)
        })
        .collect::<Vec<Doc>>()
    } else {
      expr_docs
    };

    parts.push("`".into());

    let prev_quasi_indent_size = 0;
    for (i, quasi) in tpl.quasis.iter().enumerate() {
      parts.push(quasi.raw.as_str().into());

      if let Some(expr_doc) = expr_docs.get(i) {
        parts.push(Doc::new_group(
          Doc::new_concat(vec!["${".into(), expr_doc.clone(), "}".into()]),
          false,
          None,
          None,
        ));
      }
    }

    parts.push("`".into());

    Ok(Doc::new_concat(parts))
  }

  fn print_yield_expr(
    &mut self,
    yield_expr: &YieldExpr,
  ) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();
    parts.push("yield".into());

    if yield_expr.delegate {
      parts.push("*".into());
    }

    if let Some(arg) = &yield_expr.arg {
      parts.push(" ".into());
      parts.push(self.print_expr(&arg)?);
    }

    Ok(Doc::new_concat(parts))
  }
}
