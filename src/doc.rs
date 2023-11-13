use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum Doc {
  Align(Box<Doc>, DocAlign), // In Rust, we'll need to choose between i64 or String for `widthOrString`
  Group {
    id: Option<usize>, // Placeholder for actual type of ID
    contents: Box<Doc>,
    break_: bool,
    expanded_states: Option<Vec<Doc>>,
  },
  Fill(Vec<Doc>),
  IfBreak {
    break_contents: Box<Doc>,
    flat_contents: Box<Doc>,
  },
  Indent(Box<Doc>),
  IndentIfBreak {
    contents: Box<Doc>,
    group_id: Option<usize>, // Placeholder for actual type of GroupId
    negate: bool,
  },
  LineSuffix(Box<Doc>),
  LineSuffixBoundary,
  BreakParent,
  Trim,
  Line {
    hard: bool,
    soft: bool,
    literal: bool,
  },
  Cursor,
  Label(usize, Box<Doc>), // Placeholder for actual type of Label
  Text(String),
  Array(Vec<Doc>),
}

#[derive(Clone, Debug)]
pub enum DocAlign {
  Num(isize),
  Str(Cow<'static, String>),
  Root,
}

impl Doc {
  pub fn new_indent(contents: Doc) -> Self {
    Doc::Indent(Box::new(contents))
  }

  pub fn new_align(doc: Doc, align: DocAlign) -> Self {
    Doc::Align(Box::new(doc), align)
  }

  pub fn new_group(
    contents: Box<Doc>,
    break_: bool,
    expanded_states: Option<Vec<Doc>>,
    id: Option<usize>,
  ) -> Self {
    Doc::Group {
      contents,
      break_,
      expanded_states,
      id,
    }
  }

  pub fn new_fill(parts: Vec<Doc>) -> Self {
    Doc::Fill(parts)
  }

  pub fn new_if_break(break_contents: Doc, flat_contents: Doc) -> Self {
    Doc::IfBreak {
      break_contents: Box::new(break_contents),
      flat_contents: Box::new(flat_contents),
    }
  }

  pub fn new_indent_if_break(
    contents: Doc,
    group_id: Option<usize>,
    negate: bool,
  ) -> Self {
    Doc::IndentIfBreak {
      contents: Box::new(contents),
      group_id,
      negate,
    }
  }

  pub fn new_line_suffix(contents: Doc) -> Self {
    Doc::LineSuffix(Box::new(contents))
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

  pub fn cursor() -> Self {
    Doc::Cursor
  }

  pub fn new_label(label: usize, contents: Doc) -> Self {
    Doc::Label(label, Box::new(contents))
  }

  pub fn new_text(text: String) -> Self {
    Doc::Text(text)
  }

  pub fn new_concat(docs: Vec<Doc>) -> Self {
    Doc::Array(docs)
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
    Doc::Text(value.to_string())
  }
}
