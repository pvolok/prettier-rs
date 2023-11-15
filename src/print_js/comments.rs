use swc_common::comments::Comment;

use crate::doc::Doc;

pub fn print_dangling_comments(comments: &[Comment]) -> anyhow::Result<Doc> {
  let mut parts = Vec::new();

  for comment in comments {
    parts.push("//".into());
    parts.push(comment.text.as_str().into());
    parts.push(Doc::hardline());
  }

  Ok(Doc::new_concat(parts))
}
