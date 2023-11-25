use swc_common::{BytePos, Spanned};
use swc_ecma_ast::{ArrayLit, ArrayPat, Expr, ExprOrSpread, Lit, Pat};

use crate::{
  ast_printer::AstPrinter,
  doc::{Doc, RDoc},
};

pub fn print_array_lit(
  cx: &mut AstPrinter,
  array_lit: &ArrayLit,
) -> anyhow::Result<Doc> {
  print_array(cx, &array_lit.elems)
}

pub fn print_array_pat(
  cx: &mut AstPrinter,
  array_pat: &ArrayPat,
) -> anyhow::Result<Doc> {
  print_array(cx, &array_pat.elems)
}

trait ArrayElem: Spanned {
  fn print(&self, cx: &mut AstPrinter) -> RDoc;
  fn can_have_trailing_comma(&self) -> bool;
  fn should_break(&self, next: Option<Option<&Self>>) -> bool;
  fn is_num_lit(&self) -> bool;
}

impl ArrayElem for ExprOrSpread {
  fn print(&self, cx: &mut AstPrinter) -> RDoc {
    let doc = cx.print_expr(&self.expr)?;
    Ok(if self.spread.is_some() {
      Doc::new_concat(vec!["...".into(), doc])
    } else {
      doc
    })
  }

  fn can_have_trailing_comma(&self) -> bool {
    self.spread.is_none()
  }

  fn should_break(&self, next: Option<Option<&Self>>) -> bool {
    let expr = match self {
      ExprOrSpread { expr, .. } => expr.as_ref(),
      _ => return false,
    };
    match next {
      Some(Some(next_el)) => match (expr, next_el.expr.as_ref()) {
        (Expr::Array(_), Expr::Array(_)) => (),
        (Expr::Object(_), Expr::Object(_)) => (),
        _ => return false,
      },
      _ => (),
    }

    match expr {
      Expr::Array(array_lit) => array_lit.elems.len() > 1,
      Expr::Object(object_lit) => object_lit.props.len() > 1,
      _ => false,
    }
  }

  fn is_num_lit(&self) -> bool {
    match self.expr.as_ref() {
      Expr::Lit(Lit::Num(_)) => true,
      _ => false,
    }
  }
}

impl ArrayElem for Pat {
  fn print(&self, cx: &mut AstPrinter) -> RDoc {
    cx.print_pat(self)
  }

  fn can_have_trailing_comma(&self) -> bool {
    !self.is_rest()
  }

  fn should_break(&self, next: Option<Option<&Self>>) -> bool {
    match self {
      Pat::Array(_) => (),
      Pat::Object(_) => (),
      Pat::Expr(_) => todo!(),
      _ => return false,
    }
    match next {
      Some(Some(next_el)) => match (self, next_el) {
        (Pat::Array(_), Pat::Array(_)) => (),
        (Pat::Object(_), Pat::Object(_)) => (),
        _ => return false,
      },
      _ => (),
    }

    match self {
      Pat::Array(array_lit) => array_lit.elems.len() > 1,
      Pat::Object(object_lit) => object_lit.props.len() > 1,
      _ => false,
    }
  }

  fn is_num_lit(&self) -> bool {
    false
  }
}

fn print_array<T: ArrayElem>(
  cx: &mut AstPrinter,
  elems: &Vec<Option<T>>,
) -> anyhow::Result<Doc> {
  let mut parts = Vec::new();

  let open_bracket = "[";
  let close_bracket = "]";

  if elems.is_empty() {
    parts.push(open_bracket.into());
    parts.push(close_bracket.into());
  } else {
    // We can unwrap because array is not empty.
    let last_elem = elems.last().unwrap();
    let can_have_trailing_comma = last_elem
      .as_ref()
      .map_or(true, |el| el.can_have_trailing_comma());

    // JavaScript allows you to have empty elements in an array which
    // changes its length based on the number of commas. The algorithm
    // is that if the last argument is null, we need to force insert
    // a comma to ensure JavaScript recognizes it.
    //   [,].length === 1
    //   [1,].length === 1
    //   [1,,].length === 2
    let needs_forced_trailing_comma = last_elem.is_none();

    let group_id = cx.group_id("array");

    let should_break = elems.len() > 1
      && elems.iter().enumerate().all(|(i, el)| {
        el.as_ref().map_or(false, |el| {
          el.should_break(elems.get(i + 1).map(|el| el.as_ref()))
        })
      });

    let should_use_concise_formatting = is_consicely_printed_array(elems);

    let trailing_comma = if !can_have_trailing_comma {
      "".into()
    } else if needs_forced_trailing_comma {
      ",".into()
    } else if should_use_concise_formatting {
      Doc::new_if_break(",".into(), "".into(), Some(group_id))
    } else {
      Doc::new_if_break(",".into(), "".into(), None)
    };

    let elems_doc = if should_use_concise_formatting {
      print_array_elems_consicely(cx, elems, trailing_comma)
    } else {
      print_array_elems(cx, elems, trailing_comma)
    }?;

    parts.push(Doc::new_group(
      Doc::new_concat(vec![
        open_bracket.into(),
        Doc::new_indent(Doc::new_concat(vec![Doc::softline(), elems_doc])),
        Doc::softline(),
        close_bracket.into(),
      ]),
      should_break,
      None,
      Some(group_id),
    ))
  }

  Ok(Doc::new_concat(parts))
}

pub fn print_array_elems<T: ArrayElem>(
  cx: &mut AstPrinter,
  elems: &Vec<Option<T>>,
  trailing_comma: Doc,
) -> anyhow::Result<Doc> {
  let mut elems_parts = Vec::new();
  for (i, elem) in elems.iter().enumerate() {
    let mut elem_parts = Vec::new();
    if let Some(elem) = elem {
      elem_parts.push(elem.print(cx)?);
    }
    elems_parts.push(Doc::new_group(
      Doc::new_concat(elem_parts),
      false,
      None,
      None,
    ));

    if let Some(next) = elems.get(i + 1) {
      elems_parts.push(",".into());
      elems_parts.push(Doc::line());
      if have_line_between_elems(cx, elem.as_ref(), next.as_ref()) {
        elems_parts.push(Doc::softline());
      }
    }
  }

  elems_parts.push(trailing_comma);

  Ok(Doc::new_concat(elems_parts))
}

pub fn print_array_elems_consicely<T: ArrayElem>(
  cx: &mut AstPrinter,
  elems: &Vec<Option<T>>,
  trailing_comma: Doc,
) -> anyhow::Result<Doc> {
  let mut elems_parts = Vec::new();
  for (i, elem) in elems.iter().enumerate() {
    let is_last = i == elems.len() - 1;

    let elem_doc = if let Some(elem) = elem {
      elem.print(cx)?
    } else {
      Doc::from("")
    };
    let comma = if is_last {
      trailing_comma.clone()
    } else {
      ",".into()
    };
    elems_parts.push(Doc::new_concat(vec![elem_doc, comma]));
    let next_elem = elems.get(i + 1);
    if let Some(next) = next_elem {
      if have_line_between_elems(cx, elem.as_ref(), next.as_ref()) {
        elems_parts
          .push(Doc::new_concat(vec![Doc::hardline(), Doc::hardline()]));
      } else {
        elems_parts.push(Doc::line());
      }
    }
  }

  Ok(Doc::new_fill(elems_parts))
}

fn have_line_between_elems<T: ArrayElem>(
  cx: &mut AstPrinter,
  cur: Option<&T>,
  next: Option<&T>,
) -> bool {
  let (elem, next) = match (cur, next) {
    (Some(cur), Some(next)) => (cur, next),
    _ => return false,
  };
  let mut cur_hi = elem.span_hi();
  let rest_src =
    &cx.src_file.src[(cur_hi - cx.src_file.start_pos).0 as usize..];
  for (byte_i, c) in rest_src.char_indices() {
    if c.is_ascii_whitespace() {
    } else if c == ',' {
      cur_hi = elem.span_hi() + BytePos(byte_i as u32);
      break;
    } else if c == '/' {
      // TODO: skip or stop on comments
      todo!()
    } else {
      break;
    }
  }

  let next_lo = next.span_lo();

  cx.have_line_between_spans(cur_hi, next_lo)
}

pub fn is_consicely_printed_array<T: ArrayElem>(
  elems: &Vec<Option<T>>,
) -> bool {
  elems
    .iter()
    .all(|el| el.as_ref().map_or(false, |el| el.is_num_lit()))
}
