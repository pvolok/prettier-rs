use std::ops::Bound;

use swc_common::{BytePos, SourceFile, Span};

use crate::print_js::comments::Cmts;

#[derive(Clone, Copy)]
pub struct SrcCursor<'a> {
  pub src: &'a SourceFile,
  pub cmts: &'a Cmts,
  pub pos: BytePos,
}

pub enum SrcChunk<'a> {
  Str(&'a str),
  Comment(Span),
  Eof,
}

impl<'a> SrcCursor<'a> {
  pub fn peek_chunk(self) -> SrcChunk<'a> {
    if self.src.end_pos <= self.pos {
      return SrcChunk::Eof;
    }
    let prev_starting = self
      .cmts
      .by_lo
      .upper_bound(Bound::Included(&self.pos))
      .value();
    let next_ending = self
      .cmts
      .by_hi
      .lower_bound(Bound::Excluded(&self.pos))
      .value();
    match (prev_starting, next_ending) {
      (Some(a), Some(b)) if a.span.lo == b.span.lo => SrcChunk::Comment(a.span),
      _ => {
        let following_comment = self
          .cmts
          .by_lo
          .lower_bound(Bound::Excluded(&self.pos))
          .value();
        let end_offset = following_comment.map_or_else(
          || self.src.src.len(),
          |cmt| (cmt.span.lo - self.src.start_pos).0 as usize,
        );

        let str = &self.src.src.as_str()
          [(self.pos.0 - self.src.start_pos.0) as usize..end_offset];
        SrcChunk::Str(str)
      }
    }
  }

  pub fn skip_while<P: std::str::pattern::Pattern<'a> + Clone>(
    self,
    p: P,
  ) -> Self {
    let mut cursor = self;
    loop {
      match cursor.peek_chunk() {
        SrcChunk::Str(str) => {
          let new_len = str.trim_start_matches(p.clone()).len();
          let new_cursor = Self {
            pos: cursor.pos + BytePos((str.len() - new_len) as u32),
            ..cursor
          };
          if new_len > 0 {
            return new_cursor;
          } else {
            cursor = new_cursor;
          }
        }
        SrcChunk::Comment(span) => {
          cursor = Self {
            pos: span.hi,
            ..cursor
          };
        }
        SrcChunk::Eof => return cursor,
      }
    }
  }

  pub fn peek_char(self) -> Option<char> {
    match self.peek_chunk() {
      SrcChunk::Str(str) => str.chars().next(),
      SrcChunk::Comment(_) => None,
      SrcChunk::Eof => None,
    }
  }
}
