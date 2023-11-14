use std::{fmt::Write, rc::Rc};

use swc_common::SourceFile;

use crate::doc::Doc;

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

struct Command<'a> {
  ind: Indent,
  mode: BreakMode,
  doc: &'a Doc,
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
  let mut width = PRINT_WIDTH;
  let mut pos: i32 = 0;

  let mut cmds = Vec::<Command>::new();
  let mut line_suffix = Vec::<Command>::new();

  cmds.push(Command {
    ind: Indent(None),
    mode: BreakMode::Break,
    doc,
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
              doc: contents,
            });
          }
          _ => {
            should_remeasure = false;

            let next = Command {
              ind: ind.clone(),
              mode: BreakMode::Flat,
              doc: &contents,
            };
            if !break_
              && fits(
                &next,
                &cmds,
                width - pos,
                !line_suffix.is_empty(),
                (),
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
      }
      Doc::Fill(_) => todo!(),
      Doc::IfBreak {
        break_doc,
        flat_doc,
      } => {
        match mode {
          BreakMode::Break => {
            cmds.push(Command {
              ind: ind.clone(),
              mode,
              doc: &break_doc,
            });
          }
          BreakMode::Flat => {
            cmds.push(Command {
              ind: ind.clone(),
              mode,
              doc: &flat_doc,
            });
          }
        };
      }
      Doc::Indent(doc) => {
        cmds.push(Command {
          ind: ind.indent(),
          mode,
          doc,
        });
      }
      Doc::IndentIfBreak {
        contents,
        group_id,
        negate,
      } => todo!(),
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
            } else if *literal {
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
        out.write_str(text)?;
        if !cmds.is_empty() {
          pos += string_width(text);
        }
      }
      Doc::Array(items) => {
        for item in items.iter().rev() {
          cmds.push(Command {
            ind: ind.clone(),
            mode,
            doc: item,
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
  group_mode_map: (),
  must_be_flat: bool,
) -> bool {
  if width == i32::MAX {
    return true;
  }

  let mut out = String::new();
  let mut cmds = vec![(next.mode, next.doc)];
  let mut rest = rest_commands;

  while width >= 0 {
    let (mode, doc) = if let Some(cmd) = cmds.pop() {
      cmd
    } else if let Some(cmd) = rest.take_last() {
      (cmd.mode, cmd.doc)
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
        if must_be_flat && *break_ {
          return false;
        }
        let group_mode = if *break_ { BreakMode::Break } else { mode };
        // TODO: handle expanded_states
        // let contents =
        //    && groupMode === MODE_BREAK
        //     ? doc.expandedStates.at(-1)
        //     : doc.contents;
        // cmds.push({ mode: groupMode, doc: contents });
        cmds.push((group_mode, contents));
      }
      Doc::Fill(_) => todo!(),
      Doc::IfBreak {
        break_doc,
        flat_doc,
      } => {
        let contents = match mode {
          BreakMode::Break => break_doc,
          BreakMode::Flat => flat_doc,
        };
        cmds.push((mode, contents));
      }
      Doc::Indent(doc) => {
        cmds.push((mode, doc));
      }
      Doc::IndentIfBreak {
        contents,
        group_id,
        negate,
      } => todo!(),
      Doc::LineSuffix(_) => todo!(),
      Doc::LineSuffixBoundary => todo!(),
      Doc::BreakParent => todo!(),
      Doc::Trim => todo!(),
      Doc::Line {
        hard,
        soft,
        literal,
      } => {
        if matches!(mode, BreakMode::Break) || *hard {
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
        out.push_str(s);
        width -= string_width(s);
      }
      Doc::Array(docs) => {
        for doc in docs.iter().rev() {
          cmds.push((mode, doc));
        }
      }
    }
  }

  false
}

fn string_width(s: &str) -> i32 {
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
