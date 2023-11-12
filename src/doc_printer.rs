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

pub fn print_doc(
  out: &mut DocWriter,
  src_file: &SourceFile,
  doc: &Doc,
) -> std::fmt::Result {
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
            cmds.push(next);
          }
        };
      }
      Doc::Fill(_) => todo!(),
      Doc::IfBreak {
        break_contents,
        flat_contents,
      } => todo!(),
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
      Doc::BreakParent => todo!(),
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
            }
          }
        };
      }
      Doc::Cursor => todo!(),
      Doc::Label(_, _) => todo!(),
      Doc::Text(text) => out.write_str(text)?,
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
