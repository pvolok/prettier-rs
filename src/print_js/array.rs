use swc_common::{BytePos, Spanned};
use swc_ecma_ast::{ArrayLit, Expr, ExprOrSpread, Lit};

use crate::{ast_printer::AstPrinter, doc::Doc};

pub fn print_array_lit(
  cx: &mut AstPrinter,
  array_lit: &ArrayLit,
) -> anyhow::Result<Doc> {
  let mut parts = Vec::new();

  let open_bracket = "[";
  let close_bracket = "]";

  if array_lit.elems.is_empty() {
    parts.push(open_bracket.into());
    parts.push(close_bracket.into());
  } else {
    // We can unwrap because array is not empty.
    let last_elem = array_lit.elems.last().unwrap();
    let can_have_trailing_comma =
      last_elem.as_ref().map_or(true, |el| el.spread.is_none());

    // JavaScript allows you to have empty elements in an array which
    // changes its length based on the number of commas. The algorithm
    // is that if the last argument is null, we need to force insert
    // a comma to ensure JavaScript recognizes it.
    //   [,].length === 1
    //   [1,].length === 1
    //   [1,,].length === 2
    let needs_forced_trailing_comma = last_elem.is_none();

    let group_id = cx.group_id("array");

    let should_break = array_lit.elems.len() > 1
      && array_lit.elems.iter().enumerate().all(|(i, el)| {
        let expr = match el.as_ref() {
          Some(ExprOrSpread { expr, .. }) => expr.as_ref(),
          _ => return false,
        };
        match array_lit.elems.get(i + 1).as_ref() {
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
      });

    let should_use_concise_formatting = is_consicely_printed_array(array_lit);

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
      print_array_elems_consicely(cx, array_lit, trailing_comma)
    } else {
      print_array_elems(cx, array_lit, trailing_comma)
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

pub fn print_array_elems(
  cx: &mut AstPrinter,
  array_lit: &ArrayLit,
  trailing_comma: Doc,
) -> anyhow::Result<Doc> {
  let mut elems_parts = Vec::new();
  for (i, elem) in array_lit.elems.iter().enumerate() {
    let mut elem_parts = Vec::new();
    if let Some(elem) = elem {
      if elem.spread.is_some() {
        elem_parts.push("...".into());
      }
      elem_parts.push(cx.print_expr(&elem.expr)?);
    }
    elems_parts.push(Doc::new_group(
      Doc::new_concat(elem_parts),
      false,
      None,
      None,
    ));

    if let Some(next) = array_lit.elems.get(i + 1) {
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

pub fn print_array_elems_consicely(
  cx: &mut AstPrinter,
  array_lit: &ArrayLit,
  trailing_comma: Doc,
) -> anyhow::Result<Doc> {
  let mut elems_parts = Vec::new();
  for (i, elem) in array_lit.elems.iter().enumerate() {
    let is_last = i == array_lit.elems.len() - 1;

    let elem_doc = if let Some(elem) = elem {
      cx.print_expr(&elem.expr)?
    } else {
      Doc::from("")
    };
    let comma = if is_last {
      trailing_comma.clone()
    } else {
      ",".into()
    };
    elems_parts.push(Doc::new_concat(vec![elem_doc, comma]));
    let next_elem = array_lit.elems.get(i + 1);
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

fn have_line_between_elems(
  cx: &mut AstPrinter,
  cur: Option<&ExprOrSpread>,
  next: Option<&ExprOrSpread>,
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

pub fn is_consicely_printed_array(array_lit: &ArrayLit) -> bool {
  array_lit.elems.iter().all(|el| {
    el.as_ref().map_or(false, |el| {
      matches!(el.expr.as_ref(), Expr::Lit(Lit::Num(_)))
    })
  })
}
