use std::collections::BTreeMap;

use swc_common::{
  comments::{Comment, CommentKind},
  BytePos, Spanned,
};

use crate::{
  ast_printer::{AstPrinter, SrcItem},
  doc::Doc,
};

pub struct Cmts {
  pub by_lo: BTreeMap<BytePos, Comment>,
  pub by_hi: BTreeMap<BytePos, Comment>,
}

impl Cmts {
  pub fn new() -> Self {
    Self {
      by_lo: Default::default(),
      by_hi: Default::default(),
    }
  }

  pub fn add(&mut self, cmt: &Comment) {
    self.by_lo.insert(cmt.span.lo, cmt.clone());
    self.by_hi.insert(cmt.span.hi, cmt.clone());
  }
}

fn print_comment(cmt: &Comment) -> Doc {
  match cmt.kind {
    CommentKind::Line => format!("//{}", cmt.text).as_str().into(),
    CommentKind::Block => {
      // TODO: indentable block comment
      let value_doc = cmt
        .text
        .split("\n")
        .map(Doc::from)
        .intersperse(Doc::literalline())
        .collect();
      let value_doc = Doc::new_concat(value_doc);
      Doc::new_concat(vec!["/*".into(), value_doc, "*/".into()])
    }
  }
}

pub fn print_leading_comments(
  cx: &mut AstPrinter,
  start: BytePos,
  end: BytePos,
) -> Doc {
  let mut parts = Vec::new();

  for cmt in cx.cmts.by_lo.range(start..end.max(start)).map(|(k, v)| v) {
    parts.push(print_comment(cmt));

    match cmt.kind {
      CommentKind::Block => {
        todo!();
      }
      CommentKind::Line => {
        parts.push(Doc::hardline());

        let src_after = &cx.src_file.src.as_str()
          [(cmt.span.hi() - cx.src_file.start_pos).0 as usize..];

        let has_empty_line = src_after
          .trim_start_matches([' ', '\t'])
          .strip_prefix("\n")
          .map_or(false, |src| {
            src.trim_start_matches([' ', '\t']).chars().next() == Some('\n')
          });
        if has_empty_line {
          parts.push(Doc::hardline());
        }
      }
    }
  }

  Doc::new_concat(parts)
}

pub fn print_trailing_comments(
  cx: &mut AstPrinter,
  start: BytePos,
  end: BytePos,
) -> Doc {
  let mut parts = Vec::new();

  let mut prev_is_block = false;
  let mut prev_has_line_suffix = false;

  for cmt in cx.cmts.by_lo.range(start..end.max(start)).map(|(k, v)| v) {
    let cmt_doc = print_comment(cmt);

    let preceding_newline_pos = cx
      .iter_ascii_chars_rev(cmt.span_lo())
      .skip_while(|x| match x {
        SrcItem::Ascii(c, _) => *c == ' ' || *c == '\t',
        _ => false,
      })
      .next()
      .map_or(None, |c| match c {
        SrcItem::Ascii(c, pos) if c == '\n' => Some(pos),
        _ => None,
      });
    if prev_has_line_suffix && !prev_is_block || preceding_newline_pos.is_some()
    {
      let is_line_before_empty =
        if let Some(preceding_newline_pos) = preceding_newline_pos {
          cx.iter_ascii_chars_rev(preceding_newline_pos)
            .skip_while(|x| match x {
              SrcItem::Ascii(c, _) => *c == ' ' || *c == '\t',
              _ => false,
            })
            .next()
            .map_or(false, |c| match c {
              SrcItem::Ascii(c, pos) if c == '\n' => true,
              _ => false,
            })
        } else {
          false
        };

      parts.push(Doc::new_line_suffix(Doc::new_concat(vec![
        Doc::hardline(),
        if is_line_before_empty {
          Doc::hardline()
        } else {
          Doc::none()
        },
        cmt_doc,
      ])));
      prev_has_line_suffix = true;
    } else if cmt.kind == CommentKind::Line || prev_has_line_suffix {
      parts.push(Doc::new_concat(vec![
        Doc::new_line_suffix(Doc::new_concat(vec![" ".into(), cmt_doc])),
        Doc::break_parent(),
      ]));
      prev_has_line_suffix = true;
    } else {
      parts.push(" ".into());
      parts.push(cmt_doc);
    }
    prev_is_block = cmt.kind == CommentKind::Block;
  }

  Doc::new_concat(parts)
}

pub fn print_dangling_comments(
  cx: &mut AstPrinter,
  start: BytePos,
  end: BytePos,
) -> anyhow::Result<Doc> {
  let mut parts = Vec::new();

  for (i, cmt) in cx.cmts.by_lo.range(start..end).map(|(k, v)| v).enumerate() {
    if i > 0 {
      parts.push(Doc::hardline());
    }
    parts.push(print_comment(cmt));
  }

  Ok(Doc::new_concat(parts))
}
