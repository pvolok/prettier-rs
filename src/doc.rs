use std::{
  cell::Cell,
  fmt::{write, Write},
  rc::Rc,
};

#[derive(Debug, Clone)]
pub enum Doc {
  Align(Rc<Doc>, DocAlign), // In Rust, we'll need to choose between i64 or String for `widthOrString`
  Group {
    id: Option<GroupId>, // Placeholder for actual type of ID
    contents: Rc<Doc>,
    break_: Cell<bool>,
    expanded_states: Option<Vec<Doc>>,
  },
  Fill {
    items: Rc<Vec<Doc>>,
    offset: usize,
  },
  IfBreak {
    break_doc: Rc<Doc>,
    flat_doc: Rc<Doc>,
    group_id: Option<GroupId>, // Placeholder for actual type of GroupId
  },
  Indent(Rc<Doc>),
  IndentIfBreak {
    contents: Rc<Doc>,
    group_id: Option<GroupId>, // Placeholder for actual type of GroupId
    negate: bool,
  },
  LineSuffix(Rc<Doc>),
  LineSuffixBoundary,
  BreakParent,
  Trim,
  Line {
    hard: bool,
    soft: bool,
    literal: bool,
  },
  Cursor,
  Label(usize, Rc<Doc>), // Placeholder for actual type of Label
  Text(Rc<str>),
  Array(Rc<Vec<Doc>>),
}

#[derive(Clone, Debug)]
pub enum DocAlign {
  Num(isize),
  Str(Rc<str>),
  Root,
}

pub type RDoc = anyhow::Result<Doc>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct GroupId(pub usize, pub &'static str);

impl Doc {
  pub fn none() -> Self {
    "".into()
  }

  pub fn group(doc: Doc) -> Doc {
    Doc::new_group(doc, false, None, None)
  }

  pub fn conditional_group(states: Vec<Doc>) -> Doc {
    Doc::new_group(states.first().unwrap().clone(), false, Some(states), None)
  }

  pub fn new_indent(contents: Doc) -> Self {
    Doc::Indent(Rc::new(contents))
  }

  pub fn new_align(doc: Doc, align: DocAlign) -> Self {
    Doc::Align(Rc::new(doc), align)
  }

  pub fn new_group(
    contents: Doc,
    break_: bool,
    expanded_states: Option<Vec<Doc>>,
    id: Option<GroupId>,
  ) -> Self {
    Doc::Group {
      contents: Rc::new(contents),
      break_: break_.into(),
      expanded_states,
      id,
    }
  }

  pub fn new_fill(parts: Vec<Doc>) -> Self {
    Doc::Fill {
      items: parts.into(),
      offset: 0,
    }
  }

  pub fn new_if_break(
    break_contents: Doc,
    flat_contents: Doc,
    group_id: Option<GroupId>,
  ) -> Self {
    Doc::IfBreak {
      break_doc: Rc::new(break_contents),
      flat_doc: Rc::new(flat_contents),
      group_id,
    }
  }

  pub fn new_indent_if_break(
    contents: Doc,
    group_id: Option<GroupId>,
    negate: bool,
  ) -> Self {
    Doc::IndentIfBreak {
      contents: Rc::new(contents),
      group_id,
      negate,
    }
  }

  pub fn new_line_suffix(contents: Doc) -> Self {
    Doc::LineSuffix(Rc::new(contents))
  }

  pub fn line_suffix_boundary() -> Self {
    Doc::LineSuffixBoundary
  }

  pub fn break_parent() -> Self {
    Doc::BreakParent
  }

  pub fn trim() -> Self {
    Doc::Trim
  }

  pub fn new_line(hard: bool, soft: bool, literal: bool) -> Self {
    Doc::Line {
      hard,
      soft,
      literal,
    }
  }

  pub fn new_line_suffix_boundary() -> Self {
    Doc::LineSuffixBoundary
  }

  pub fn hardline_without_break_parent() -> Self {
    Doc::Line {
      hard: true,
      soft: false,
      literal: false,
    }
  }

  pub fn line() -> Self {
    Doc::Line {
      hard: false,
      soft: false,
      literal: false,
    }
  }

  pub fn hardline() -> Self {
    Doc::new_concat(vec![
      Doc::Line {
        hard: true,
        soft: false,
        literal: false,
      },
      Doc::BreakParent,
    ])
  }

  pub fn literalline() -> Self {
    Doc::new_concat(vec![
      Doc::Line {
        hard: true,
        soft: false,
        literal: true,
      },
      Doc::BreakParent,
    ])
  }

  pub fn softline() -> Self {
    Doc::Line {
      hard: false,
      soft: true,
      literal: false,
    }
  }

  pub fn cursor() -> Self {
    Doc::Cursor
  }

  pub fn new_label(label: usize, contents: Doc) -> Self {
    Doc::Label(label, Rc::new(contents))
  }

  pub fn new_text(text: String) -> Self {
    Doc::Text(text.into())
  }

  pub fn new_concat(docs: Vec<Doc>) -> Self {
    Doc::Array(docs.into())
  }

  pub fn join(sep: &Doc, items: Vec<Doc>) -> Vec<Doc> {
    let mut ret = Vec::with_capacity((items.len() * 2).saturating_sub(1));
    for (i, item) in items.into_iter().enumerate() {
      if i > 0 {
        ret.push(sep.clone());
      }
      ret.push(item);
    }
    ret
  }

  pub fn can_break(&self) -> bool {
    match self {
      Doc::Align(doc, _) => doc.can_break(),
      Doc::Group { contents, .. } => contents.can_break(),
      Doc::Fill { items, offset } => {
        items.iter().skip(*offset).any(|item| item.can_break())
      }
      Doc::IfBreak {
        break_doc,
        flat_doc,
        ..
      } => break_doc.can_break() || flat_doc.can_break(),
      Doc::Indent(doc) => doc.can_break(),
      Doc::IndentIfBreak { contents, .. } => contents.can_break(),
      Doc::LineSuffix(_) => todo!(),
      Doc::LineSuffixBoundary => todo!(),
      Doc::BreakParent => false,
      Doc::Trim => false,
      Doc::Line { .. } => true,
      Doc::Cursor => false,
      Doc::Label(_, doc) => doc.can_break(),
      Doc::Text(_) => false,
      Doc::Array(items) => items.iter().any(|item| item.can_break()),
    }
  }

  pub fn will_break(&self) -> bool {
    match self {
      Doc::Align(doc, _) => doc.will_break(),
      Doc::Group {
        contents, break_, ..
      } => break_.get() || contents.will_break(),
      Doc::Fill { items, offset } => {
        items.iter().skip(*offset).any(|item| item.will_break())
      }
      Doc::IfBreak {
        break_doc,
        flat_doc,
        ..
      } => break_doc.will_break() || flat_doc.will_break(),
      Doc::Indent(doc) => doc.will_break(),
      Doc::IndentIfBreak { contents, .. } => contents.will_break(),
      Doc::LineSuffix(_) => todo!(),
      Doc::LineSuffixBoundary => todo!(),
      Doc::BreakParent => true,
      Doc::Trim => false,
      Doc::Line { hard, .. } => true,
      Doc::Cursor => false,
      Doc::Label(_, doc) => doc.will_break(),
      Doc::Text(_) => false,
      Doc::Array(items) => items.iter().any(|item| item.will_break()),
    }
  }

  pub fn debug(&self) -> String {
    let mut buf = String::new();

    fn rec(buf: &mut String, ind: usize, doc: &Doc) {
      match doc {
        Doc::Align(doc, align) => {
          write!(buf, "align {:?}", align).unwrap();
          write!(buf, "\n{}", "  ".repeat(ind + 1)).unwrap();
          rec(buf, ind + 1, doc);
        }
        Doc::Group {
          id,
          contents,
          break_,
          expanded_states,
        } => {
          write!(buf, "group break:{:?}, id:{:?}", break_.get(), id).unwrap();
          write!(buf, "\n{}content:", "  ".repeat(ind + 1)).unwrap();
          write!(buf, "\n{}", "  ".repeat(ind + 2)).unwrap();
          rec(buf, ind + 2, &contents);

          write!(buf, "\n{}expanded_states:", "  ".repeat(ind + 1)).unwrap();
          if let Some(expanded_states) = expanded_states {
            for item in expanded_states {
              write!(buf, "\n{}- ", "  ".repeat(ind + 1)).unwrap();
              rec(buf, ind + 2, item);
            }
          }
        }
        Doc::Fill { items, offset } => {
          write!(buf, "fill").unwrap();
          for item in items.as_ref() {
            write!(buf, "\n{}- ", "  ".repeat(ind)).unwrap();
            rec(buf, ind + 1, item);
          }
        }
        Doc::IfBreak {
          break_doc,
          flat_doc,
          group_id,
        } => {
          write!(buf, "if-break id:{:?}", group_id).unwrap();
          write!(buf, "\n{}break:", "  ".repeat(ind + 1)).unwrap();
          write!(buf, "\n{}", "  ".repeat(ind + 2)).unwrap();
          rec(buf, ind + 2, &break_doc);
          write!(buf, "\n{}flat:", "  ".repeat(ind + 1)).unwrap();
          write!(buf, "\n{}", "  ".repeat(ind + 2)).unwrap();
          rec(buf, ind + 2, &flat_doc);
        }
        Doc::Indent(doc) => {
          write!(buf, "indent").unwrap();
          write!(buf, "\n{}", "  ".repeat(ind + 1)).unwrap();
          rec(buf, ind + 1, &doc);
        }
        Doc::IndentIfBreak {
          contents,
          group_id,
          negate,
        } => {
          write!(buf, "indent-if-break id:{:?}, neg:{}", group_id, negate)
            .unwrap();
          write!(buf, "\n{}", "  ".repeat(ind + 1)).unwrap();
          rec(buf, ind + 1, &contents);
        }
        Doc::LineSuffix(doc) => {
          write!(buf, "line-suffix").unwrap();
          write!(buf, "\n{}", "  ".repeat(ind + 1)).unwrap();
          rec(buf, ind + 1, &doc);
        }
        Doc::LineSuffixBoundary => {
          write!(buf, "line-suffix-boundary").unwrap();
        }
        Doc::BreakParent => {
          write!(buf, "break-parent").unwrap();
        }
        Doc::Trim => {
          write!(buf, "trim").unwrap();
        }
        Doc::Line {
          hard,
          soft,
          literal,
        } => {
          write!(buf, "line hard:{} soft:{} lit:{}", hard, soft, literal)
            .unwrap();
        }
        Doc::Cursor => todo!(),
        Doc::Label(_, _) => todo!(),
        Doc::Text(str) => {
          write!(buf, "\"{}\"", str).unwrap();
        }
        Doc::Array(items) => {
          write!(buf, "array:").unwrap();
          for item in items.as_ref() {
            write!(buf, "\n{}- ", "  ".repeat(ind)).unwrap();
            rec(buf, ind + 1, item);
          }
        }
      }
    }
    rec(&mut buf, 0, self);
    buf
  }
}

impl From<&str> for Doc {
  fn from(value: &str) -> Self {
    Doc::Text(value.into())
  }
}
