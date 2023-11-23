use swc_common::comments::{Comment, CommentKind};

use crate::{ast_printer::AstPrinter, doc::Doc};

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
  comments: &[Comment],
) -> Doc {
  let mut parts = Vec::new();

  for cmt in comments {
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

pub fn print_dangling_comments(comments: &[Comment]) -> anyhow::Result<Doc> {
  let mut parts = Vec::new();

  for comment in comments {
    parts.push("//".into());
    parts.push(comment.text.as_str().into());
    parts.push(Doc::hardline());
  }

  Ok(Doc::new_concat(parts))
}
