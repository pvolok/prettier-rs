use std::{borrow::Cow, rc::Rc};

#[derive(Debug, Clone)]
pub enum Doc {
  Align(Rc<Doc>, DocAlign), // In Rust, we'll need to choose between i64 or String for `widthOrString`
  Group {
    id: Option<GroupId>, // Placeholder for actual type of ID
    contents: Rc<Doc>,
    break_: bool,
    expanded_states: Option<Vec<Doc>>,
  },
  Fill(Rc<Vec<Doc>>),
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
  Str(Cow<'static, String>),
  Root,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct GroupId(pub usize, pub &'static str);

impl Doc {
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
      break_,
      expanded_states,
      id,
    }
  }

  pub fn new_fill(parts: Vec<Doc>) -> Self {
    Doc::Fill(parts.into())
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
    let mut ret = Vec::with_capacity(items.len() * 2 - 1);
    for (i, item) in items.into_iter().enumerate() {
      if i > 0 {
        ret.push(sep.clone());
      }
      ret.push(item);
    }
    ret
  }
}

impl From<&str> for Doc {
  fn from(value: &str) -> Self {
    Doc::Text(value.into())
  }
}
