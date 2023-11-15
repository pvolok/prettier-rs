use std::{borrow::Cow, collections::HashMap, fmt::Write, rc::Rc};

use swc_common::SourceFile;

use crate::doc::{Doc, GroupId};

pub enum DocWriter<'a> {
  String(&'a mut String),
}

impl<'a> Write for DocWriter<'a> {
  fn write_str(&mut self, s: &str) -> std::fmt::Result {
    match self {
      DocWriter::String(out) => out.write_str(s),
    }
  }
}

impl DocWriter<'_> {
  fn trim(&mut self) -> usize {
    match self {
      DocWriter::String(s) => {
        let len = s.trim_end_matches("\t ").len();
        s.truncate(s.len() - len);
        len
      }
    }
  }
}

struct Command {
  ind: Indent,
  mode: BreakMode,
  doc: Doc,
}

#[derive(Clone, Copy, Debug)]
enum BreakMode {
  Break,
  Flat,
}

#[derive(Clone, Copy, Debug)]
enum IndentPart {
  Indent,
}

#[derive(Clone, Debug)]
struct Indent(Option<Rc<(IndentPart, Indent)>>);

impl Indent {
  fn indent(&self) -> Indent {
    Indent(Some(Rc::new((IndentPart::Indent, self.clone()))))
  }

  fn width(&self) -> i32 {
    let mut ind = self;
    let mut width = 0;
    while let Some(level) = &ind.0 {
      let (part, parent) = level.as_ref();
      match part {
        IndentPart::Indent => {
          width += 2;
        }
      }
      ind = parent;
    }
    width
  }
}

impl std::fmt::Display for Indent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.0 {
      Some(ind) => {
        ind.1.fmt(f)?;
        match ind.0 {
          IndentPart::Indent => f.write_str("  ")?,
        };
      }
      None => (),
    }
    Ok(())
  }
}

const PRINT_WIDTH: i32 = 80;

pub fn print_doc(
  out: &mut DocWriter,
  src_file: &SourceFile,
  doc: &Doc,
) -> std::fmt::Result {
  let mut group_mode_map = HashMap::<GroupId, BreakMode>::new();

  let width = PRINT_WIDTH;
  let mut pos: i32 = 0;

  let mut cmds = Vec::<Command>::new();
  let mut line_suffix = Vec::<Command>::new();

  cmds.push(Command {
    ind: Indent(None),
    mode: BreakMode::Break,
    doc: doc.clone(),
  });

  let mut should_remeasure = false;

  while let Some(Command { ind, mode, doc }) = cmds.pop() {
    match doc {
      Doc::Align(_, _) => todo!(),
      Doc::Group {
        id,
        contents,
        break_,
        expanded_states,
      } => {
        match mode {
          BreakMode::Flat if !should_remeasure => {
            cmds.push(Command {
              ind: ind.clone(),
              mode,
              doc: contents.as_ref().clone(),
            });
          }
          _ => {
            should_remeasure = false;

            let next = Command {
              ind: ind.clone(),
              mode: BreakMode::Flat,
              doc: contents.as_ref().clone(),
            };
            if !break_
              && fits(
                &next,
                &cmds,
                width - pos,
                !line_suffix.is_empty(),
                &mut group_mode_map,
                false,
              )
            {
              cmds.push(next);
            } else {
              cmds.push(Command {
                mode: BreakMode::Break,
                ..next
              });
            }
          }
        };

        if let Some(last_cmd) = cmds.last() {
          if let Some(group_id) = id {
            group_mode_map.insert(group_id.clone(), last_cmd.mode);
          }
        }
      }
      // Fills each line with as much code as possible before moving to a new
      // line with the same indentation.
      //
      // Expects doc.parts to be an array of alternating content and
      // whitespace. The whitespace contains the linebreaks.
      //
      // For example:
      //   ["I", line, "love", line, "monkeys"]
      // or
      //   [{ type: group, ... }, softline, { type: group, ... }]
      //
      // It uses this parts structure to handle three main layout cases:
      // * The first two content items fit on the same line without
      //   breaking
      //   -> output the first content item and the whitespace "flat".
      // * Only the first content item fits on the line without breaking
      //   -> output the first content item "flat" and the whitespace with
      //   "break".
      // * Neither content item fits on the line without breaking
      //   -> output the first content item and the whitespace with "break".
      Doc::Fill { items, offset } => {
        let rem = width - pos;

        if let Some(content) = items.get(offset) {
          let content_flat_cmd = Command {
            ind: ind.clone(),
            mode: BreakMode::Flat,
            doc: content.clone(),
          };
          let content_break_cmd = Command {
            ind: ind.clone(),
            mode: BreakMode::Break,
            doc: content.clone(),
          };
          let content_fits = fits(
            &content_flat_cmd,
            &[],
            rem,
            false,
            &mut group_mode_map,
            true,
          );

          if let Some(whitespace) = items.get(offset + 1) {
            let whitespace_flat_cmd = Command {
              ind: ind.clone(),
              mode: BreakMode::Flat,
              doc: whitespace.clone(),
            };
            let whitespace_break_cmd = Command {
              ind: ind.clone(),
              mode: BreakMode::Break,
              doc: whitespace.clone(),
            };

            if let Some(second_content) = items.get(offset + 2) {
              let remaining_cmd = Command {
                ind: ind.clone(),
                mode,
                doc: Doc::Fill {
                  items: items.clone(),
                  offset: offset + 2,
                },
              };
              let first_and_second_flat_cmd = Command {
                ind: ind.clone(),
                mode: BreakMode::Flat,
                doc: Doc::new_concat(vec![
                  content.clone(),
                  whitespace.clone(),
                  second_content.clone(),
                ]),
              };
              let first_and_second_content_fits = fits(
                &first_and_second_flat_cmd,
                &[],
                rem,
                false,
                &mut group_mode_map,
                true,
              );

              if first_and_second_content_fits {
                cmds.push(remaining_cmd);
                cmds.push(whitespace_flat_cmd);
                cmds.push(content_flat_cmd);
              } else if content_fits {
                cmds.push(remaining_cmd);
                cmds.push(whitespace_break_cmd);
                cmds.push(content_flat_cmd);
              } else {
                cmds.push(remaining_cmd);
                cmds.push(whitespace_break_cmd);
                cmds.push(content_break_cmd);
              }
            } else {
              if content_fits {
                cmds.push(whitespace_flat_cmd);
                cmds.push(content_flat_cmd);
              } else {
                cmds.push(whitespace_break_cmd);
                cmds.push(content_break_cmd);
              }
            }
          } else {
            if content_fits {
              cmds.push(content_flat_cmd);
            } else {
              cmds.push(content_break_cmd);
            }
          }
        }
      }
      Doc::IfBreak {
        break_doc,
        flat_doc,
        group_id,
      } => {
        let group_mode =
          group_id.map_or(mode, |id| *group_mode_map.get(&id).unwrap());
        match group_mode {
          BreakMode::Break => {
            cmds.push(Command {
              ind: ind.clone(),
              mode,
              doc: break_doc.as_ref().clone(),
            });
          }
          BreakMode::Flat => {
            cmds.push(Command {
              ind: ind.clone(),
              mode,
              doc: flat_doc.as_ref().clone(),
            });
          }
        };
      }
      Doc::Indent(doc) => {
        cmds.push(Command {
          ind: ind.indent(),
          mode,
          doc: doc.as_ref().clone(),
        });
      }
      Doc::IndentIfBreak {
        contents,
        group_id,
        negate,
      } => {
        let group_mode = group_id
          .map(|id| group_mode_map.get(&id).unwrap())
          .unwrap_or(&mode);
        let contents = match group_mode {
          BreakMode::Break => {
            if negate {
              contents.as_ref().clone()
            } else {
              Doc::new_indent(contents.as_ref().clone())
            }
          }
          BreakMode::Flat => {
            if negate {
              Doc::new_indent(contents.as_ref().clone())
            } else {
              contents.as_ref().clone()
            }
          }
        };
        cmds.push(Command {
          ind: ind.clone(),
          mode,
          doc: contents,
        })
      }
      Doc::LineSuffix(_) => todo!(),
      Doc::LineSuffixBoundary => todo!(),
      Doc::BreakParent => (),
      Doc::Trim => todo!(),
      Doc::Line {
        hard,
        soft,
        literal,
      } => {
        match mode {
          BreakMode::Flat if !hard => {
            if !soft {
              out.write_char(' ')?;
              pos += 1;
            }
          }
          _ => {
            if matches!(mode, BreakMode::Flat) {
              should_remeasure = true;
            }

            if !line_suffix.is_empty() {
              cmds.push(Command {
                ind: ind.clone(),
                mode,
                doc,
              });
              while let Some(cmd) = line_suffix.pop() {
                cmds.push(cmd);
              }
            } else if literal {
              todo!();
            } else {
              write!(out, "\n{}", ind)?;
              pos = ind.width();
            }
          }
        };
      }
      Doc::Cursor => todo!(),
      Doc::Label(_, _) => todo!(),
      Doc::Text(text) => {
        out.write_str(&text)?;
        if !cmds.is_empty() {
          pos += string_width(&text);
        }
      }
      Doc::Array(items) => {
        for item in items.iter().rev() {
          cmds.push(Command {
            ind: ind.clone(),
            mode,
            doc: item.clone(),
          });
        }
      }
    }
  }

  Ok(())
}

fn fits(
  next: &Command,
  rest_commands: &[Command],
  mut width: i32,
  has_line_suffix: bool,
  group_mode_map: &mut HashMap<GroupId, BreakMode>,
  must_be_flat: bool,
) -> bool {
  if width == i32::MAX {
    return true;
  }

  let mut out = String::new();

  let mut cmds: Vec<(BreakMode, Doc)> = vec![(next.mode, next.doc.clone())];
  let mut rest = rest_commands;

  while width >= 0 {
    let (mode, doc) = if let Some(cmd) = cmds.pop() {
      cmd
    } else if let Some(cmd) = rest.take_last() {
      (cmd.mode, cmd.doc.clone())
    } else {
      return true;
    };

    match doc {
      Doc::Align(_, _) => todo!(),
      Doc::Group {
        id,
        contents,
        break_,
        expanded_states,
      } => {
        if must_be_flat && break_ {
          return false;
        }
        let group_mode = if break_ { BreakMode::Break } else { mode };
        // TODO: handle expanded_states
        // let contents =
        //    && groupMode === MODE_BREAK
        //     ? doc.expandedStates.at(-1)
        //     : doc.contents;
        // cmds.push({ mode: groupMode, doc: contents });
        cmds.push((group_mode, contents.as_ref().clone()));
      }
      Doc::IfBreak {
        break_doc,
        flat_doc,
        group_id,
      } => {
        let group_mode = if let Some(group_id) = group_id {
          *group_mode_map.get(&group_id).unwrap_or(&BreakMode::Flat)
        } else {
          mode
        };
        let contents = match group_mode {
          BreakMode::Break => break_doc,
          BreakMode::Flat => flat_doc,
        };
        cmds.push((mode, contents.as_ref().clone()));
      }
      Doc::Indent(doc) => {
        cmds.push((mode, doc.as_ref().clone()));
      }
      Doc::IndentIfBreak {
        contents,
        group_id,
        negate,
      } => cmds.push((mode, contents.as_ref().clone())),
      Doc::LineSuffix(_) => todo!(),
      Doc::LineSuffixBoundary => todo!(),
      Doc::BreakParent => todo!(),
      Doc::Trim => todo!(),
      Doc::Line {
        hard,
        soft,
        literal,
      } => {
        if matches!(mode, BreakMode::Break) || hard {
          return true;
        }
        if !soft {
          out.push(' ');
          width -= 1;
        }
      }
      Doc::Cursor => todo!(),
      Doc::Label(_, _) => todo!(),
      Doc::Text(s) => {
        out.push_str(&s);
        width -= string_width(&s);
      }
      Doc::Array(docs) => {
        for doc in docs.iter().rev() {
          cmds.push((mode, doc.clone()));
        }
      }
      Doc::Fill { items, offset } => {
        for doc in items.iter().skip(offset).rev() {
          cmds.push((mode, doc.clone()));
        }
      }
    }
  }

  false
}

pub fn string_width(s: &str) -> i32 {
  s.chars()
    .map(|c| {
      if c.is_control() {
        return 0;
      }
      if c == '\t' {
        return 2;
      }
      1
    })
    .sum()
}
