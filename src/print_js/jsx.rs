use swc_ecma_ast::{
  JSXAttr, JSXAttrName, JSXAttrOrSpread, JSXAttrValue, JSXClosingElement,
  JSXElement, JSXElementName, JSXFragment, JSXOpeningElement, Lit,
  SpreadElement,
};

use crate::{
  ast_printer::AstPrinter,
  ast_util::{get_preferred_quote, Quote},
  doc::{Doc, RDoc},
};

pub fn print_jsx_element(
  cx: &mut AstPrinter,
  jsx_element: &JSXElement,
) -> RDoc {
  // TODO: comments
  // TODO: parens

  if jsx_element.children.is_empty() {
    let mut parts = Vec::new();
    parts.push(print_jsx_opening_element(cx, &jsx_element.opening)?);
    if let Some(closing) = &jsx_element.closing {
      parts.push(print_jsx_closing_element(cx, closing)?)
    }
    return Ok(Doc::new_concat(parts));
  }

  todo!()
}

fn print_jsx_opening_element(
  cx: &mut AstPrinter,
  jsx_open_el: &JSXOpeningElement,
) -> RDoc {
  let name_has_comments = false;

  // Don't break self-closing elements with no attributes and no comments
  if jsx_open_el.self_closing
    && jsx_open_el.attrs.is_empty()
    && !name_has_comments
  {
    return Ok(Doc::new_concat(vec![
      "<".into(),
      print_jsx_element_name(cx, &jsx_open_el.name)?,
      // TODO: type args
      " />".into(),
    ]));
  }

  let is_single_long_text_attr = match jsx_open_el.attrs.as_slice() {
    [JSXAttrOrSpread::JSXAttr(jsx_attr)] => match &jsx_attr.value {
      Some(JSXAttrValue::Lit(Lit::Str(str))) => str.value.contains("\n"),
      _ => false,
    },
    _ => false,
  };
  if is_single_long_text_attr {
    let mut parts = vec![
      "<".into(),
      print_jsx_element_name(cx, &jsx_open_el.name)?,
      // TODO: type args
      " ".into(),
      // ...path.map(print, "attributes"),
    ];

    for attr in jsx_open_el.attrs.iter() {
      parts.push(print_jsx_attr_or_spread(cx, attr)?);
    }

    if jsx_open_el.self_closing {
      parts.push(" />".into());
    } else {
      parts.push(">".into());
    }

    return Ok(Doc::group(Doc::new_concat(parts)));
  }

  // We should print the opening element expanded if any prop value is a
  // string literal with newlines
  let should_break = jsx_open_el.attrs.iter().any(|attr| match attr {
    JSXAttrOrSpread::JSXAttr(attr) => match attr.value.as_ref() {
      Some(JSXAttrValue::Lit(Lit::Str(str))) => str.value.contains("\n"),
      _ => false,
    },
    JSXAttrOrSpread::SpreadElement(_) => false,
  });

  let attribute_line =
    if cx.single_attribute_per_line && jsx_open_el.attrs.len() > 1 {
      Doc::hardline()
    } else {
      Doc::line()
    };

  let last_attr_has_trailing_comments = false;

  let tag_end = if jsx_open_el.self_closing {
    Doc::new_concat(vec![Doc::line(), "/>".into()])
  } else if
  // Simple tags (no attributes and no comment in tag name) should be
  // kept unbroken regardless of `bracketSameLine`.
  // jsxBracketSameLine is deprecated in favour of bracketSameLine,
  // but is still needed for backwards compatibility.
  (jsx_open_el.attrs.is_empty() && !name_has_comments)
    || ((cx.bracket_same_line || cx.jsx_single_quote)
      // We should print the bracket in a new line for the following cases:
      // <div
      //   // comment
      // >
      // <div
      //   attr // comment
      // >
      && (!name_has_comments || jsx_open_el.attrs.len() > 0)
      && last_attr_has_trailing_comments)
  {
    ">".into()
  } else {
    Doc::new_concat(vec![Doc::softline(), ">".into()])
  };

  let doc = Doc::new_group(
    Doc::new_concat(vec![
      "<".into(),
      print_jsx_element_name(cx, &jsx_open_el.name)?,
      // TODO: type args
      Doc::new_indent(Doc::new_concat(
        jsx_open_el
          .attrs
          .iter()
          .map(|attr| {
            Ok(Doc::new_concat(vec![
              attribute_line.clone(),
              print_jsx_attr_or_spread(cx, attr)?,
            ]))
          })
          .collect::<anyhow::Result<Vec<Doc>>>()?,
      )),
      tag_end,
    ]),
    should_break,
    None,
    None,
  );

  Ok(doc)
}

fn print_jsx_closing_element(
  cx: &mut AstPrinter,
  jsx_close_el: &JSXClosingElement,
) -> RDoc {
  Ok(Doc::new_concat(vec![
    "</".into(),
    print_jsx_element_name(cx, &jsx_close_el.name)?,
    ">".into(),
  ]))
}

fn print_jsx_attr_or_spread(
  cx: &mut AstPrinter,
  jsx_attr_or_spread: &JSXAttrOrSpread,
) -> RDoc {
  match jsx_attr_or_spread {
    JSXAttrOrSpread::JSXAttr(jsx_attr) => print_jsx_attr(cx, jsx_attr),
    JSXAttrOrSpread::SpreadElement(spread_element) => {
      print_spread_element(cx, spread_element)
    }
  }
}

fn print_jsx_attr(cx: &mut AstPrinter, jsx_attr: &JSXAttr) -> RDoc {
  let mut parts = Vec::new();

  parts.push(print_jsx_attr_name(cx, &jsx_attr.name)?);

  if let Some(jsx_attr_value) = &jsx_attr.value {
    let res = match jsx_attr_value {
      JSXAttrValue::Lit(lit) => match lit {
        Lit::Str(str) => {
          let raw = str.raw.as_ref().unwrap();
          // Remove enclosing quotes and unescape
          // all quotes so we get an accurate preferred quote
          let clean = raw[1..raw.len() - 1]
            .replace("&apos;", "'")
            .replace("&quot;", "\"");
          let quote = get_preferred_quote(clean.as_str(), cx.jsx_single_quote);
          let clean = clean.replace(
            quote.str(),
            match quote {
              Quote::Single => "&apos;",
              Quote::Double => "&quot;",
            },
          );

          // TODO: comments
          let str_parts = clean
            .split("\n")
            .map(|s| Doc::new_text(s.to_string()))
            .intersperse(Doc::literalline());

          Doc::new_concat(str_parts.collect())
        }
        lit => cx.print_lit(lit)?,
      },
      JSXAttrValue::JSXExprContainer(_) => todo!(),
      JSXAttrValue::JSXElement(_) => todo!(),
      JSXAttrValue::JSXFragment(_) => todo!(),
    };
    parts.push("=".into());
    parts.push(res);
  }

  Ok(Doc::new_concat(parts))
}

fn print_spread_element(
  cx: &mut AstPrinter,
  spread_element: &SpreadElement,
) -> RDoc {
  todo!()
}

fn print_jsx_attr_name(
  cx: &mut AstPrinter,
  jsx_attr_name: &JSXAttrName,
) -> RDoc {
  match jsx_attr_name {
    JSXAttrName::Ident(ident) => Ok(cx.print_ident(ident)),
    JSXAttrName::JSXNamespacedName(_) => todo!(),
  }
}

fn print_jsx_element_name(
  cx: &mut AstPrinter,
  jsx_element_name: &JSXElementName,
) -> RDoc {
  match jsx_element_name {
    JSXElementName::Ident(ident) => Ok(cx.print_ident(ident)),
    JSXElementName::JSXMemberExpr(_) => todo!(),
    JSXElementName::JSXNamespacedName(_) => todo!(),
  }
}
