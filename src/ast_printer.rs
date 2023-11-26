use std::rc::Rc;

use swc_common::{
  comments::SingleThreadedComments, BytePos, SourceFile, SourceMap, Span,
  Spanned,
};
use swc_ecma_ast::{
  AssignExpr, AwaitExpr, BigInt, BlockStmt, CatchClause, ComputedPropName,
  Decl, DoWhileStmt, Expr, ExprStmt, ForHead, ForInStmt, ForOfStmt, ForStmt,
  Ident, IfStmt, Lit, MemberExpr, MemberProp, Module, ModuleItem, Number,
  ObjectLit, ObjectPat, ObjectPatProp, OptChainBase, Pat, PatOrExpr, Program,
  Prop, PropName, PropOrSpread, RestPat, SeqExpr, Stmt, Str, SuperProp,
  SuperPropExpr, SwitchCase, SwitchStmt, TaggedTpl, Tpl, TryStmt, UnaryExpr,
  UpdateExpr, VarDecl, VarDeclKind, VarDeclOrExpr, VarDeclarator, WhileStmt,
  YieldExpr,
};
use swc_ecma_visit::{
  fields::{
    AssignExprField, ExprField, ForStmtField, OptChainBaseField,
    OptChainExprField, ParamField, SeqExprField,
  },
  AstParentKind, AstParentNodeRef,
};

use crate::{
  ast_path::{fake_path, sub_box, var, ARef, Path},
  doc::{Doc, GroupId, RDoc},
  doc_printer::{print_doc, string_width, DocWriter},
  print_js::{
    array::{print_array_lit, print_array_pat},
    assign::{print_assignment, AssignmentLeft},
    bin_expr::print_bin_expr,
    call::{print_call_expr, print_new_expr, print_opt_call},
    class::{print_class_decl, print_class_expr},
    comments::{
      print_dangling_comments, print_leading_comments, print_trailing_comments,
      Cmts,
    },
    function::{
      print_arrow_expr, print_fn_decl, print_fn_expr, print_return_stmt,
      print_throw_stmt,
    },
    jsx::print_jsx_element,
    module::print_module_decl,
    parens::needs_parens,
    statement::print_stmt_seq,
    ternary::print_cond,
  },
  src_cursor::SrcCursor,
};

pub struct AstPrinter {
  pub cm: Rc<SourceMap>,
  pub src_file: Rc<SourceFile>,
  pub comments: SingleThreadedComments,
  pub cmts: Cmts,
  last_group_id: usize,

  pub stack: Vec<AstParentKind>,

  pub tab_width: i32,
  pub print_width: i32,
  pub semi: bool,
  pub bracket_spacing: bool,
  pub bracket_same_line: bool,
  pub jsx_single_quote: bool,
  pub jsx_bracket_spacing: bool,
  pub single_attribute_per_line: bool,
  pub should_print_comma: Comma,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Comma {
  All,
}

impl AstPrinter {
  pub fn new(
    cm: Rc<SourceMap>,
    src_file: Rc<SourceFile>,
    comments: SingleThreadedComments,
    cmts: Cmts,
  ) -> Self {
    Self {
      cm,
      src_file,
      comments,
      cmts,
      last_group_id: 0,

      stack: Vec::new(),

      tab_width: 2,
      print_width: 80,
      semi: true,
      bracket_spacing: true,
      bracket_same_line: false,
      jsx_single_quote: false,
      jsx_bracket_spacing: false,
      single_attribute_per_line: false,
      should_print_comma: Comma::All,
    }
  }
}

impl AstPrinter {
  pub fn print_program(&mut self, program: &Program) -> anyhow::Result<Doc> {
    match program {
      Program::Module(module) => todo!(),
      Program::Script(_) => todo!(),
    }
  }

  pub fn is_next_line_empty<T: Spanned>(
    &self,
    items: impl ExactSizeIterator<Item = T>,
    index: usize,
  ) -> bool {
    let mut items = items.skip(index);
    let cur = items.next();
    let next = items.next();

    let cur = if let Some(cur) = cur {
      cur
    } else {
      return false;
    };
    let next = if let Some(next) = next {
      next
    } else {
      return false;
    };
    self.have_line_between_spans(cur.span_hi(), next.span_lo())
  }

  pub fn have_line_between_spans(
    &self,
    cur_hi: BytePos,
    next_lo: BytePos,
  ) -> bool {
    self.src_file.lookup_line(cur_hi).unwrap_or(0) + 1
      < self.src_file.lookup_line(next_lo).unwrap_or(0)
  }

  pub fn group_id(&mut self, name: &'static str) -> GroupId {
    self.last_group_id += 1;
    GroupId(self.last_group_id, name)
  }

  pub fn print_module(
    &mut self,
    module: Path<'_, Module>,
  ) -> anyhow::Result<Doc> {
    let mut contents = Vec::new();

    let mut cmts_start = self.src_file.start_pos;

    let body = module.body();
    let body_len = body.len();
    for (i, module_item) in body.iter().enumerate() {
      contents.push(print_leading_comments(
        self,
        cmts_start,
        module_item.span_lo(),
      ));
      cmts_start = module_item.span_hi();

      let item = self.print_module_item(module_item.clone())?;
      contents.push(item);

      if i != body_len - 1 {
        contents.push(Doc::new_line(true, false, false));

        if self.is_next_line_empty(body.iter(), i) {
          contents.push(Doc::new_line(true, false, false));
        }
      }
    }

    contents.push(print_trailing_comments(
      self,
      cmts_start,
      self.src_file.end_pos,
    ));

    contents.push(Doc::hardline());

    let doc = Doc::new_concat(contents);

    Ok(doc)
  }

  fn print_module_item(
    &mut self,
    module_item: Path<'_, ModuleItem>,
  ) -> anyhow::Result<Doc> {
    match module_item.node {
      ModuleItem::ModuleDecl(module_decl) => {
        print_module_decl(self, module_decl)
      }
      ModuleItem::Stmt(stmt) => self.print_stmt(stmt),
    }
  }

  pub fn push(&mut self, parent: AstParentKind) {
    self.stack.push(parent);
  }

  pub fn pop(&mut self) {
    self.stack.pop();
  }

  pub fn print_stmt(&mut self, stmt: &Stmt) -> anyhow::Result<Doc> {
    let semi = if self.semi { ";" } else { "" }.into();

    let doc = match stmt {
      Stmt::Block(block_stmt) => self.print_block_stmt(block_stmt, true),
      Stmt::Empty(_) => Ok("".into()),
      Stmt::Debugger(_) => Ok(Doc::new_concat(vec!["debugger".into(), semi])),
      Stmt::With(with_stmt) => {
        let body_doc = self.print_stmt(&with_stmt.body)?;
        Ok(Doc::new_group(
          Doc::new_concat(vec![
            "with (".into(),
            self.print_expr(&with_stmt.obj)?,
            ")".into(),
            self.adjust_clause(with_stmt.body.as_ref(), body_doc, false),
          ]),
          false,
          None,
          None,
        ))
      }
      Stmt::Return(return_stmt) => {
        print_return_stmt(self, fake_path(return_stmt))
      }
      Stmt::Labeled(labeled_stmt) => {
        let parts = if labeled_stmt.body.is_empty() {
          vec![self.print_ident(&labeled_stmt.label), ":;".into()]
        } else {
          vec![
            self.print_ident(&labeled_stmt.label),
            ": ".into(),
            self.print_stmt(&labeled_stmt.body)?,
          ]
        };
        Ok(Doc::new_concat(parts))
      }
      Stmt::Break(break_stmt) => {
        let mut parts = vec!["break".into()];
        if let Some(label) = &break_stmt.label {
          parts.push(" ".into());
          parts.push(Doc::new_text(label.to_string()));
        }
        parts.push(semi);
        Ok(Doc::new_concat(parts))
      }
      Stmt::Continue(continue_stmt) => {
        let mut parts = vec!["continue".into()];
        if let Some(label) = &continue_stmt.label {
          parts.push(" ".into());
          parts.push(Doc::new_text(label.to_string()));
        }
        parts.push(semi);
        Ok(Doc::new_concat(parts))
      }
      Stmt::If(if_stmt) => self.print_if_stmt(if_stmt),
      Stmt::Switch(switch_stmt) => self.print_switch_stmt(switch_stmt),
      Stmt::Throw(throw_stmt) => print_throw_stmt(self, fake_path(throw_stmt)),
      Stmt::Try(try_stmt) => self.print_try_stmt(try_stmt.as_ref()),
      Stmt::While(while_stmt) => self.print_while_stmt(while_stmt),
      Stmt::DoWhile(do_while_stmt) => self.print_do_while_stmt(do_while_stmt),
      Stmt::For(for_stmt) => self.print_for_stmt(for_stmt),
      Stmt::ForIn(for_in_stmt) => self.print_for_in_stmt(for_in_stmt),
      Stmt::ForOf(for_of_stmt) => self.print_for_of_stmt(for_of_stmt),
      Stmt::Decl(decl) => self.print_decl(decl),
      Stmt::Expr(expr_stmt) => self.print_expr_stmt(expr_stmt),
    }?;

    Ok(doc)
  }

  pub fn print_block_stmt(
    &mut self,
    block_stmt: &BlockStmt,
    empty_hardline: bool,
  ) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();

    parts.push("{".into());

    let body_parts = print_stmt_seq(self, &block_stmt.stmts)?;
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

  fn print_if_stmt(&mut self, if_stmt: &IfStmt) -> anyhow::Result<Doc> {
    let cons_doc = self.print_stmt(&if_stmt.cons)?;
    let cons = self.adjust_clause(&if_stmt.cons, cons_doc, false);

    let opening = Doc::new_group(
      Doc::new_concat(vec![
        "if (".into(),
        Doc::new_group(
          Doc::new_concat(vec![
            Doc::new_indent(Doc::new_concat(vec![
              Doc::softline(),
              self.print_expr(&if_stmt.test)?,
            ])),
            Doc::softline(),
          ]),
          false,
          None,
          None,
        ),
        ")".into(),
        cons,
      ]),
      false,
      None,
      None,
    );

    let mut parts = Vec::new();

    parts.push(opening);

    if let Some(alt) = if_stmt.alt.as_ref() {
      let comment_on_own_line = false;
      let else_on_same_line = if_stmt.cons.is_block() && !comment_on_own_line;
      if else_on_same_line {
        parts.push(" ".into());
      } else {
        parts.push(Doc::hardline());
      }

      // TODO: comments

      parts.push("else".into());
      let alt_doc = self.print_stmt(&alt)?;
      parts.push(Doc::new_group(
        self.adjust_clause(&alt, alt_doc, alt.is_if_stmt()),
        false,
        None,
        None,
      ));
    }

    Ok(Doc::new_concat(parts))
  }

  fn print_switch_stmt(
    &mut self,
    switch_stmt: &SwitchStmt,
  ) -> anyhow::Result<Doc> {
    let mut parts = vec![
      Doc::group(Doc::new_concat(vec![
        "switch (".into(),
        Doc::new_indent(Doc::new_concat(vec![
          Doc::softline(),
          self.print_expr(switch_stmt.discriminant.as_ref())?,
        ])),
        Doc::softline(),
        ")".into(),
      ])),
      " {".into(),
    ];

    if !switch_stmt.cases.is_empty() {
      let mut cases_parts = Vec::new();
      for (i, switch_case) in switch_stmt.cases.iter().enumerate() {
        let is_last = i == switch_stmt.cases.len() - 1;
        let mut case_parts = vec![self.print_switch_case(switch_case)?];
        if !is_last {
          case_parts.push(Doc::hardline());
          if self.is_next_line_empty(switch_stmt.cases.iter(), i) {
            case_parts.push(Doc::hardline());
          }
        }
        cases_parts.push(Doc::new_concat(case_parts));
      }

      let doc = Doc::new_indent(Doc::new_concat(vec![
        Doc::hardline(),
        Doc::new_concat(cases_parts),
      ]));
      parts.push(doc);
    }

    parts.push(Doc::hardline());
    parts.push("}".into());

    Ok(Doc::new_concat(parts))
  }

  fn print_switch_case(
    &mut self,
    switch_case: &SwitchCase,
  ) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();

    if let Some(test) = &switch_case.test {
      parts.push("case ".into());
      parts.push(self.print_expr(&test)?);
      parts.push(":".into());
    } else {
      parts.push("default:".into());
    }

    if !switch_case.cons.is_empty() {
      // TODO: handle empty statements
      let cons = print_stmt_seq(self, &switch_case.cons)?;

      if switch_case.cons.len() == 1
        && switch_case
          .cons
          .first()
          .map(|stmt| stmt.is_block())
          .unwrap_or(false)
      {
        parts.push(" ".into());
        parts.extend_from_slice(&cons);
      } else {
        parts.push(Doc::new_indent(Doc::new_concat(vec![
          Doc::hardline(),
          Doc::new_concat(cons),
        ])));
      }
    }

    Ok(Doc::new_concat(parts))
  }

  fn print_try_stmt(&mut self, try_stmt: &TryStmt) -> anyhow::Result<Doc> {
    let mut parts =
      vec!["try ".into(), self.print_block_stmt(&try_stmt.block, true)?];
    if let Some(handler) = &try_stmt.handler {
      parts.push(Doc::new_concat(vec![
        " ".into(),
        self.print_catch_clause(&handler)?,
      ]));
    }
    if let Some(finalizer) = &try_stmt.finalizer {
      parts.push(Doc::new_concat(vec![
        " finally ".into(),
        self.print_block_stmt(&finalizer, true)?,
      ]));
    }
    Ok(Doc::new_concat(parts))
  }

  fn print_catch_clause(
    &mut self,
    catch_clause: &CatchClause,
  ) -> anyhow::Result<Doc> {
    if let Some(param) = &catch_clause.param {
      let parameter_has_comments = false;
      let param_doc = self.print_pat(&param)?;

      return Ok(Doc::new_concat(vec![
        "catch ".into(),
        Doc::new_concat(if parameter_has_comments {
          todo!()
        } else {
          vec!["(".into(), param_doc, ") ".into()]
        }),
        self.print_block_stmt(&catch_clause.body, true)?,
      ]));
    }

    Ok(Doc::new_concat(vec![
      "catch ".into(),
      self.print_block_stmt(&catch_clause.body, true)?,
    ]))
  }

  fn print_while_stmt(
    &mut self,
    while_stmt: &WhileStmt,
  ) -> anyhow::Result<Doc> {
    let body_doc = self.print_stmt(&while_stmt.body)?;
    Ok(Doc::new_concat(vec![
      "while (".into(),
      Doc::group(Doc::new_concat(vec![
        Doc::new_indent(Doc::new_concat(vec![
          Doc::softline(),
          self.print_expr(&while_stmt.test)?,
        ])),
        Doc::softline(),
      ])),
      ")".into(),
      self.adjust_clause(while_stmt.body.as_ref(), body_doc, false),
    ]))
  }

  fn print_do_while_stmt(
    &mut self,
    do_while_stmt: &DoWhileStmt,
  ) -> anyhow::Result<Doc> {
    let body_doc = self.print_stmt(&do_while_stmt.body)?;
    let clause = self.adjust_clause(&do_while_stmt.body, body_doc, false);
    let do_body = Doc::group(Doc::new_concat(vec!["do".into(), clause]));
    let mut parts = vec![do_body];

    if do_while_stmt.body.is_block() {
      parts.push(" ".into());
    } else {
      parts.push(Doc::hardline());
    }
    parts.push("while (".into());
    parts.push(Doc::group(Doc::new_concat(vec![
      Doc::new_indent(Doc::new_concat(vec![
        Doc::softline(),
        self.print_expr(&do_while_stmt.test)?,
      ])),
      Doc::softline(),
    ])));
    parts.push(")".into());
    if self.semi {
      parts.push(";".into());
    }

    return Ok(Doc::new_concat(parts));
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

  fn print_for_in_stmt(
    &mut self,
    for_in_stmt: &ForInStmt,
  ) -> anyhow::Result<Doc> {
    let head_doc = match &for_in_stmt.left {
      ForHead::VarDecl(var_decl) => self.print_var_decl(var_decl, true)?,
      ForHead::UsingDecl(_) => todo!(),
      ForHead::Pat(pat) => self.print_pat(pat)?,
    };
    let body_doc = self.print_stmt(&for_in_stmt.body)?;
    let parts = vec![
      "for (".into(),
      head_doc,
      " in ".into(),
      self.print_expr(for_in_stmt.right.as_ref())?,
      ")".into(),
      self.adjust_clause(&for_in_stmt.body, body_doc, false),
    ];

    Ok(Doc::new_concat(parts))
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

  pub fn print_decl(&mut self, decl: &Decl) -> anyhow::Result<Doc> {
    match decl {
      Decl::Class(class_decl) => print_class_decl(self, class_decl),
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

    if let Some(init) = var_declarator.init.as_ref() {
      let op_pos = self
        .cursor(var_declarator.name.span_hi())
        .skip_while(char::is_whitespace)
        .pos;

      return print_assignment(
        self,
        left_doc,
        AssignmentLeft::Pat(&var_declarator.name),
        (" =".into(), op_pos),
        init,
      );
    } else {
      return Ok(left_doc);
    }

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
    self.print_pat_path(fake_path(pat))
  }

  pub fn print_pat_path(&mut self, pat: Path<Pat>) -> anyhow::Result<Doc> {
    let doc = match pat.node {
      Pat::Ident(ident) => self.print_ident(ident),
      Pat::Array(array_pat) => print_array_pat(self, array_pat)?,
      Pat::Rest(rest_pat) => self.print_rest_pat(rest_pat)?,
      Pat::Object(object_pat) => {
        let object_pat = var!(pat, Pat, object_pat, Object);
        self.print_object_pat(object_pat)?
      }
      Pat::Assign(assign_pat) => Doc::new_concat(vec![
        self.print_pat(&assign_pat.left)?,
        " = ".into(),
        self.print_expr(&assign_pat.right)?,
      ]),
      Pat::Invalid(_) => todo!(),
      Pat::Expr(expr) => self.print_expr(&expr)?,
    };

    Ok(doc)
  }

  fn print_rest_pat(&mut self, rest_pat: &RestPat) -> RDoc {
    Ok(Doc::new_concat(vec![
      "...".into(),
      self.print_pat(&rest_pat.arg)?,
    ]))
  }

  fn print_expr_stmt(&mut self, expr_stmt: &ExprStmt) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();

    parts.push(self.print_expr(&expr_stmt.expr)?);
    parts.push(";".into());

    Ok(Doc::new_concat(parts))
  }

  pub fn print_expr(&mut self, expr: &Expr) -> anyhow::Result<Doc> {
    self.print_expr_path(fake_path(expr))
  }

  pub fn print_expr_path(&mut self, expr: Path<Expr>) -> anyhow::Result<Doc> {
    let doc = match expr.node {
      Expr::This(_this_expr) => "this".into(),
      Expr::Array(array_lit) => print_array_lit(self, array_lit)?,
      Expr::Object(object_lit) => self.print_object_lit(object_lit)?,
      Expr::Fn(fn_expr) => print_fn_expr(self, fn_expr)?,
      Expr::Unary(unary_expr) => self.print_unary_expr(unary_expr)?,
      Expr::Update(unpdate_expr) => {
        self.print_update_expr(var!(expr, Expr, unpdate_expr, Update))?
      }
      Expr::Bin(bin_expr) => {
        print_bin_expr(self, var!(expr, Expr, bin_expr, Bin))?
      }
      Expr::Assign(assign_expr) => self.print_assign_expr(assign_expr)?,
      Expr::Member(member_expr) => {
        self.print_member_expr(fake_path(member_expr), false)?
      }
      Expr::SuperProp(super_prop_expr) => {
        self.print_super_prop_expr(fake_path(super_prop_expr), false)?
      }
      Expr::Cond(cond_expr) => {
        print_cond(self, var!(expr, Expr, cond_expr, Cond))?
      }
      Expr::Call(call_expr) => print_call_expr(self, call_expr)?,
      Expr::New(new_expr) => print_new_expr(self, new_expr)?,
      Expr::Seq(seq_expr) => self.print_seq_expr(&fake_path(seq_expr))?,
      Expr::Ident(ident) => Doc::new_text(ident.sym.to_string()),
      Expr::Lit(lit) => self.print_lit(lit)?,
      Expr::Tpl(tpl) => self.print_tpl(tpl)?,
      Expr::TaggedTpl(tagged_tpl) => self.print_tagged_tpl(tagged_tpl)?,
      Expr::Arrow(arrow_expr) => print_arrow_expr(
        self,
        expr.sub(|p| (ARef::Expr(p, ExprField::Arrow), arrow_expr)),
        &Default::default(),
      )?,
      Expr::Class(class_expr) => print_class_expr(self, class_expr)?,
      Expr::Yield(yield_expr) => self.print_yield_expr(yield_expr)?,
      Expr::MetaProp(_) => todo!(),
      Expr::Await(await_expr) => self.print_await_expr(await_expr)?,
      Expr::Paren(paren_expr) => self.print_expr(&paren_expr.expr)?,
      Expr::JSXMember(_) => todo!(),
      Expr::JSXNamespacedName(_) => todo!(),
      Expr::JSXEmpty(_) => todo!(),
      Expr::JSXElement(jsx_element) => print_jsx_element(self, jsx_element)?,
      Expr::JSXFragment(_) => todo!(),
      Expr::TsTypeAssertion(_) => todo!("ts"),
      Expr::TsConstAssertion(_) => todo!("ts"),
      Expr::TsNonNull(_) => todo!("ts"),
      Expr::TsAs(_) => todo!("ts"),
      Expr::TsInstantiation(_) => todo!("ts"),
      Expr::TsSatisfies(_) => todo!("ts"),
      Expr::PrivateName(_) => todo!(),
      Expr::OptChain(opt_chain_expr) => {
        let opt_chain_expr = fake_path(opt_chain_expr);
        let base = opt_chain_expr.sub(|p| {
          (
            ARef::OptChainExpr(p, OptChainExprField::Base),
            p.base.as_ref(),
          )
        });
        match base.node {
          OptChainBase::Member(member_expr) => {
            let member_expr = base.sub(|p| {
              (
                ARef::OptChainBase(p, OptChainBaseField::Member),
                member_expr,
              )
            });
            self.print_member_expr(member_expr, opt_chain_expr.node.optional)?
          }
          OptChainBase::Call(opt_call) => print_opt_call(self, opt_call)?,
        }
      }
      Expr::Invalid(_) => todo!(),
    };

    let mut parts = Vec::new();

    let needs_parens = needs_parens(self, &expr);
    if needs_parens {
      parts.push("(".into());
    }
    parts.push(doc);
    if needs_parens {
      parts.push(")".into());
    }

    let doc = Doc::new_concat(parts);

    Ok(doc)
  }

  fn print_object_lit(
    &mut self,
    object_lit: &ObjectLit,
  ) -> anyhow::Result<Doc> {
    let should_break = object_lit.props.first().map_or(false, |first| {
      self.line_no(object_lit.span_lo()) != self.line_no(first.span_lo())
    });

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

        is_prev_line_empty =
          self.is_next_line_empty(object_lit.props.iter(), i);

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

    Ok(Doc::new_group(
      Doc::new_concat(parts),
      should_break,
      None,
      None,
    ))
  }

  fn print_object_pat(
    &mut self,
    object_pat: Path<ObjectPat>,
  ) -> anyhow::Result<Doc> {
    let should_break = match object_pat.parent.node_ref {
      AstParentNodeRef::Param(_, ParamField::Pat) => false,
      _ => object_pat.node.props.iter().any(|prop| {
        prop
          .as_key_value()
          .map_or(false, |kv| kv.value.is_object() || kv.value.is_array())
      }),
    };

    let separator = Doc::from(",");

    let mut is_prev_line_empty = false;

    let props = object_pat
      .node
      .props
      .iter()
      .enumerate()
      .map(|(i, object_pat_prop)| {
        let prop_doc = match object_pat_prop {
          ObjectPatProp::KeyValue(key_value_prop) => {
            let key: Doc = match &key_value_prop.key {
              PropName::Ident(ident) => ident.sym.as_str().into(),
              PropName::Str(_) => todo!(),
              PropName::Num(_) => todo!(),
              PropName::Computed(_) => todo!(),
              PropName::BigInt(_) => todo!(),
            };
            let value = self.print_pat(&key_value_prop.value)?;

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
          ObjectPatProp::Assign(assign_pat_prop) => {
            let key = self.print_ident(&assign_pat_prop.key);

            if let Some(value) = assign_pat_prop.value.as_ref() {
              let value = self.print_expr(&value)?;

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
            } else {
              key
            }
          }
          ObjectPatProp::Rest(_) => todo!(),
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

        is_prev_line_empty =
          self.is_next_line_empty(object_pat.node.props.iter(), i);

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

    // TODO:
    // If we inline the object as first argument of the parent, we don't want
    // to create another group so that the object breaks before the return
    // type

    Ok(Doc::new_group(
      Doc::new_concat(parts),
      should_break,
      None,
      None,
    ))
  }

  fn print_unary_expr(
    &mut self,
    unary_expr: &UnaryExpr,
  ) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();

    parts.push(unary_expr.op.as_str().into());

    if unary_expr
      .op
      .as_str()
      .chars()
      .all(|c| c.is_ascii_alphabetic())
    {
      parts.push(" ".into());
    }

    parts.push(self.print_expr(&unary_expr.arg)?);

    Ok(Doc::new_concat(parts))
  }

  fn print_update_expr(
    &mut self,
    update_expr: Path<UpdateExpr>,
  ) -> anyhow::Result<Doc> {
    let mut parts = Vec::with_capacity(2);

    if update_expr.node.prefix {
      parts.push(update_expr.node.op.as_str().into());
    }

    let arg = sub_box!(update_expr, UpdateExpr, arg, Arg);
    parts.push(self.print_expr_path(arg)?);

    if !update_expr.node.prefix {
      parts.push(update_expr.node.op.as_str().into());
    }

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
    let op_pos = self
      .cursor(assign_expr.left.span_hi())
      .skip_while(char::is_whitespace)
      .pos;

    return print_assignment(
      self,
      left_doc,
      AssignmentLeft::PatOrExpr(&assign_expr.left),
      (op_doc, op_pos),
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
    member_expr: Path<MemberExpr>,
    optional: bool,
  ) -> anyhow::Result<Doc> {
    let obj = self.print_expr(&member_expr.node.obj)?;

    let lookup = match &member_expr.node.prop {
      MemberProp::Ident(ident) => {
        let op = if optional { "?." } else { "." };
        Doc::new_concat(vec![op.into(), ident.sym.as_str().into()])
      }
      MemberProp::PrivateName(_) => todo!(),
      MemberProp::Computed(prop) => {
        self.print_computed_prop_name(prop, optional)?
      }
    };

    Ok(Doc::new_concat(vec![obj, lookup]))
  }

  fn print_super_prop_expr(
    &mut self,
    super_prop_expr: Path<SuperPropExpr>,
    optional: bool,
  ) -> anyhow::Result<Doc> {
    let lookup = match &super_prop_expr.node.prop {
      SuperProp::Ident(ident) => {
        let op = if optional { "?." } else { "." };
        Doc::new_concat(vec![op.into(), ident.sym.as_str().into()])
      }
      SuperProp::Computed(prop) => {
        self.print_computed_prop_name(prop, optional)?
      }
    };

    Ok(Doc::new_concat(vec!["super".into(), lookup]))
  }

  fn print_computed_prop_name(
    &mut self,
    computed_prop_name: &ComputedPropName,
    optional: bool,
  ) -> RDoc {
    let op = if optional { "?." } else { "" };
    let doc = self.print_expr(&computed_prop_name.expr)?;

    let doc = match computed_prop_name.expr.as_ref() {
      Expr::Lit(Lit::Num(_)) => {
        Doc::new_concat(vec![op.into(), "[".into(), doc, "]".into()])
      }
      _ => Doc::new_group(
        Doc::new_concat(vec![
          op.into(),
          "[".into(),
          Doc::new_indent(Doc::new_concat(vec![Doc::softline(), doc])),
          Doc::softline(),
          "]".into(),
        ]),
        false,
        None,
        None,
      ),
    };
    Ok(doc)
  }

  fn print_seq_expr(
    &mut self,
    seq_expr: &Path<SeqExpr>,
  ) -> anyhow::Result<Doc> {
    let exprs = seq_expr.sub_vec(seq_expr.node.exprs.as_ref(), |p, node, i| {
      (ARef::SeqExpr(p, SeqExprField::Exprs(i)), node.as_ref())
    });
    match seq_expr.parent.node_ref {
      AstParentNodeRef::ExprStmt(_, _) | AstParentNodeRef::ForStmt(_, _) => {
        // For ExpressionStatements and for-loop heads, which are among
        // the few places a SequenceExpression appears unparenthesized, we want
        // to indent expressions after the first.
        let mut parts = Vec::new();
        for (i, expr) in exprs.iter().enumerate() {
          let expr_doc = self.print_expr(expr.node)?;
          if i == 0 {
            parts.push(expr_doc);
          } else {
            parts.push(Doc::new_concat(vec![
              ",".into(),
              Doc::new_indent(Doc::new_concat(vec![Doc::line(), expr_doc])),
            ]));
          }
        }
        return Ok(Doc::new_group(Doc::new_concat(parts), false, None, None));
      }
      _ => (),
    }

    let doc = Doc::new_group(
      Doc::new_concat(
        exprs
          .into_iter()
          .map(|e| self.print_expr(e.node).ok())
          .intersperse(Some(Doc::new_concat(vec![",".into(), Doc::line()])))
          .collect::<Option<Vec<_>>>()
          .unwrap_or_default(),
      ),
      false,
      None,
      None,
    );
    Ok(doc)
  }

  pub fn print_lit(&mut self, lit: &Lit) -> anyhow::Result<Doc> {
    let doc = match lit {
      Lit::Str(str) => self.print_str(str),
      Lit::Bool(bool) => Doc::new_text(format!("{}", bool.value)),
      Lit::Null(_) => Doc::new_text(format!("null")),
      Lit::Num(num) => self.print_number(num),
      Lit::BigInt(big_int) => self.print_big_int(big_int),
      Lit::Regex(regex) => {
        Doc::new_text(format!("/{}/{}", regex.exp, regex.flags))
      }
      Lit::JSXText(_) => todo!(),
    };

    Ok(doc)
  }

  pub fn print_ident(&mut self, ident: &Ident) -> Doc {
    Doc::from(ident.sym.as_str())
  }

  pub fn print_str(&mut self, str: &Str) -> Doc {
    Doc::new_text(format!("\"{}\"", str.value))
  }

  pub fn print_number(&mut self, number: &Number) -> Doc {
    let raw = number.raw.as_ref().unwrap();
    Doc::new_text(format!("{}", raw.to_ascii_lowercase()))
  }

  pub fn print_big_int(&mut self, big_int: &BigInt) -> Doc {
    Doc::new_text(big_int.raw.as_ref().map_or_else(
      || String::new(),
      |raw| raw.to_ascii_lowercase().to_string(),
    ))
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

  fn print_tagged_tpl(
    &mut self,
    tagged_tpl: &TaggedTpl,
  ) -> anyhow::Result<Doc> {
    let doc = Doc::new_concat(vec![
      self.print_expr(&tagged_tpl.tag)?,
      Doc::line_suffix_boundary(),
      self.print_tpl(&tagged_tpl.tpl)?,
    ]);
    Ok(doc)
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

  fn print_await_expr(
    &mut self,
    await_expr: &AwaitExpr,
  ) -> anyhow::Result<Doc> {
    let mut parts = Vec::new();
    parts.push("await".into());

    parts.push(" ".into());
    parts.push(self.print_expr(&await_expr.arg)?);

    // TODO: handle if in call or member

    Ok(Doc::new_concat(parts))
  }

  fn adjust_clause(
    &mut self,
    stmt: &Stmt,
    clause: Doc,
    force_space: bool,
  ) -> Doc {
    if stmt.is_empty() {
      return ";".into();
    }

    if stmt.is_block() || force_space {
      return Doc::new_concat(vec![" ".into(), clause]);
    }

    return Doc::new_indent(Doc::new_concat(vec![Doc::line(), clause]));
  }

  pub fn iter_ascii_chars(&self, byte_pos: BytePos) -> SrcAsciiIter {
    SrcAsciiIter {
      cx: self,
      index: self.pos_to_idx(byte_pos),
    }
  }

  pub fn iter_ascii_chars_rev(&self, byte_pos: BytePos) -> SrcAsciiRevIter {
    SrcAsciiRevIter {
      cx: self,
      index: self.pos_to_idx(byte_pos),
    }
  }

  pub fn span_snippet(&self, span: Span) -> &str {
    &self.src_file.src.as_str()
      [self.pos_to_idx(span.lo)..self.pos_to_idx(span.hi)]
  }

  pub fn line_no(&self, byte_pos: BytePos) -> usize {
    self.src_file.lookup_line(byte_pos).unwrap_or(0)
  }

  pub fn pos_to_idx(&self, pos: BytePos) -> usize {
    (pos.0 - self.src_file.start_pos.0) as usize
  }

  pub fn idx_to_pos(&self, index: usize) -> BytePos {
    BytePos(self.src_file.start_pos.0 + index as u32)
  }

  pub fn cursor(&self, pos: BytePos) -> SrcCursor {
    SrcCursor {
      src: &self.src_file,
      cmts: &self.cmts,
      pos,
    }
  }
}

#[derive(Debug)]
pub enum SrcItem {
  Ascii(char, BytePos),
  NonAscii,
  Comment(Span),
}

pub struct SrcAsciiIter<'a> {
  cx: &'a AstPrinter,
  index: usize,
}

impl Iterator for SrcAsciiIter<'_> {
  type Item = SrcItem;

  fn next(&mut self) -> Option<Self::Item> {
    let src = self.cx.src_file.src.as_bytes();
    if self.index >= src.len() {
      return None;
    }
    let index = self.index;
    self.index += 1;

    let prev_cmt = self.cx.cmts.by_lo.get(&self.cx.idx_to_pos(index));
    if let Some(cmt) = prev_cmt {
      self.index = self.cx.pos_to_idx(cmt.span_hi());
      return Some(SrcItem::Comment(cmt.span()));
    }

    let next_byte = src[index];

    if next_byte.is_ascii() {
      Some(SrcItem::Ascii(next_byte as char, self.cx.idx_to_pos(index)))
    } else {
      Some(SrcItem::NonAscii)
    }
  }
}

pub struct SrcAsciiRevIter<'a> {
  cx: &'a AstPrinter,
  index: usize,
}

impl Iterator for SrcAsciiRevIter<'_> {
  type Item = SrcItem;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index == 0 {
      return None;
    }
    self.index -= 1;
    let index = self.index;

    let prev_cmt = self.cx.cmts.by_hi.get(&self.cx.idx_to_pos(index));
    if let Some(cmt) = prev_cmt {
      self.index = self.cx.pos_to_idx(cmt.span_lo());
      return Some(SrcItem::Comment(cmt.span()));
    }

    let next_byte = self.cx.src_file.src.as_bytes()[index];

    if next_byte.is_ascii() {
      Some(SrcItem::Ascii(next_byte as char, self.cx.idx_to_pos(index)))
    } else {
      Some(SrcItem::NonAscii)
    }
  }
}
