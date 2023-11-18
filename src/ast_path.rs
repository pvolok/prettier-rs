use swc_common::{BytePos, Span, Spanned, SyntaxContext};
use swc_ecma_ast::*;
use swc_ecma_visit::{fields::*, AstParentNodeRef};

#[derive(Clone, Copy)]
pub struct PathSeg<'a> {
  pub parent: Option<&'a PathSeg<'a>>,
  pub node_ref: AstParentNodeRef<'a>,
}

#[derive(Clone, Copy)]
pub struct Path<'a, N> {
  pub parent: PathSeg<'a>,
  pub node: &'a N,
}

impl<N: Spanned> Spanned for Path<'_, N> {
  fn span(&self) -> swc_common::Span {
    self.node.span()
  }
}

const INVALID_NODE: Invalid = Invalid {
  span: Span {
    lo: BytePos::DUMMY,
    hi: BytePos::DUMMY,
    ctxt: SyntaxContext::empty(),
  },
};

pub fn fake_path<'a, T>(node: &'a T) -> Path<'a, T> {
  Path {
    parent: PathSeg {
      parent: None,
      node_ref: AstParentNodeRef::Invalid(&INVALID_NODE, InvalidField::Span),
    },
    node,
  }
}

macro_rules! var {
  ($var:ident, $type:ident, $field:expr, $Field:ident) => {
    $var.sub(|p| {
      use swc_ecma_visit::fields::*;
      (
        crate::ast_path::ARef::$type(p, <concat_idents!($type, Field)>::$Field),
        $field,
      )
    })
  };
}
pub(crate) use var;

macro_rules! sub_box {
  ($var:ident, $type:ident, $field:ident, $Field:ident) => {
    $var.sub(|p| {
      use swc_ecma_visit::fields::*;
      (
        crate::ast_path::ARef::$type(p, <concat_idents!($type, Field)>::$Field),
        p.$field.as_ref(),
      )
    })
  };
}
pub(crate) use sub_box;

impl<'a> Path<'a, ArrayLit> {
  pub fn elems(
    &'a self,
  ) -> impl Iterator<Item = Option<Path<'a, ExprOrSpread>>> {
    self.node.elems.iter().enumerate().map(|(i, elem)| {
      elem.as_ref().map(|elem| {
        self.sub(|p| {
          (AstParentNodeRef::ArrayLit(p, ArrayLitField::Elems(i)), elem)
        })
      })
    })
  }
}

impl<'a, N> Path<'a, N> {
  pub fn sub<T>(
    &'a self,
    f: impl Fn(&'a N) -> (AstParentNodeRef, &T),
  ) -> Path<'a, T> {
    let (node_ref, child) = f(self.node);
    Path {
      parent: PathSeg {
        parent: Some(&self.parent),
        node_ref,
      },
      node: child,
    }
  }

  pub fn sub_opt<T>(
    &'a self,
    get: impl Fn(&'a N) -> Option<&T>,
    field: impl Fn(&'a N, &T) -> AstParentNodeRef<'a>,
  ) -> Option<Path<'a, T>> {
    get(self.node).map(|node| self.sub(|p| (field(p, node), node)))
  }

  pub fn sub_opt_box<T>(
    &'a self,
    get: impl Fn(&'a N) -> Option<&Box<T>>,
    field: impl Fn(&'a N, &T) -> AstParentNodeRef<'a>,
  ) -> Option<Path<'a, T>> {
    get(self.node)
      .map(|node| self.sub(|p| (field(p, node.as_ref()), node.as_ref())))
  }

  pub fn sub_vec<T, R>(
    &'a self,
    arr: &'a [T],
    f: impl Fn(&'a N, &'a T, usize) -> (AstParentNodeRef<'a>, &'a R),
  ) -> Vec<Path<'a, R>> {
    arr
      .into_iter()
      .enumerate()
      .map(|(i, item)| {
        let (node_ref, child) = f(self.node, item, i);
        self.sub(|p| (node_ref, child))
      })
      .collect()
  }

  pub fn get_parent(&self, index: u32) -> Option<ARef> {
    let mut parent = &self.parent;
    for _ in 0..index {
      parent = if let Some(parent) = parent.parent {
        parent
      } else {
        return None;
      }
    }
    Some(parent.node_ref)
  }

  pub fn find_ancestor<T>(
    &'a self,
    pred: impl Fn(ARef<'a>) -> Option<T>,
  ) -> Option<T> {
    let mut seg = &self.parent;
    loop {
      let result = pred(seg.node_ref);
      if result.is_some() {
        return result;
      }
      if let Some(parent) = seg.parent {
        seg = parent;
      } else {
        return None;
      }
    }
  }
}

fn from_val<'a, T>(
  parent: &'a PathSeg<'a>,
  node: &'a T,
  node_ref: AstParentNodeRef<'a>,
) -> Path<'a, T> {
  Path {
    parent: PathSeg {
      parent: Some(parent),
      node_ref,
    },
    node,
  }
}

fn from_opt<'a, T>(
  parent: &'a PathSeg<'a>,
  node: &'a Option<T>,
  node_ref: AstParentNodeRef<'a>,
) -> Option<Path<'a, T>> {
  node.as_ref().map(|node| Path {
    parent: PathSeg {
      parent: Some(parent),
      node_ref,
    },
    node,
  })
}

fn from_vec<'a, T, F: Fn(usize) -> AstParentNodeRef<'a>>(
  parent: &'a PathSeg<'a>,
  vec: &'a [T],
  make_ref: F,
) -> Vec<Path<'a, T>> {
  vec
    .iter()
    .enumerate()
    .map(move |(i, node)| Path {
      parent: PathSeg {
        parent: Some(parent),
        node_ref: make_ref(i),
      },
      node,
    })
    .collect()
}

pub type PathVec<'a, N> = Vec<Path<'a, N>>;

impl Path<'_, Module> {
  pub fn body(&self) -> PathVec<ModuleItem> {
    from_vec(&self.parent, &self.node.body, |i| {
      AstParentNodeRef::Module(self.node, ModuleField::Body(i))
    })
  }
}

impl<'a> Path<'a, ObjectLit> {
  pub fn props(&'a self) -> PathVec<PropOrSpread> {
    from_vec(&self.parent, &self.node.props, |i| {
      AstParentNodeRef::ObjectLit(self.node, ObjectLitField::Props(i))
    })
  }
}

impl<'a> Path<'a, BinExpr> {
  pub fn left(&'a self) -> Path<'a, Expr> {
    from_val(
      &self.parent,
      &self.node.left,
      AstParentNodeRef::BinExpr(self.node, BinExprField::Left),
    )
  }

  pub fn right(&'a self) -> Path<'a, Expr> {
    from_val(
      &self.parent,
      &self.node.right,
      AstParentNodeRef::BinExpr(self.node, BinExprField::Right),
    )
  }
}

pub type ARef<'a> = AstParentNodeRef<'a>;

impl<'a> Path<'a, UnaryExpr> {
  pub fn op(&'a self) -> Path<'a, UnaryOp> {
    self.sub(|p| (ARef::UnaryExpr(p, UnaryExprField::Op), &p.op))
  }

  pub fn arg(&'a self) -> Path<'a, Expr> {
    self.sub(|p| (ARef::UnaryExpr(p, UnaryExprField::Arg), p.arg.as_ref()))
  }
}

impl Path<'_, FnExpr> {
  pub fn ident(&self) -> Option<Path<'_, Ident>> {
    from_opt(
      &self.parent,
      &self.node.ident,
      AstParentNodeRef::FnExpr(self.node, FnExprField::Ident),
    )
  }

  pub fn function(&self) -> Path<'_, Function> {
    from_val(
      &self.parent,
      self.node.function.as_ref(),
      AstParentNodeRef::FnExpr(self.node, FnExprField::Function),
    )
  }
}

mod function {}
impl Path<'_, Function> {
  pub fn params(&self) -> Vec<Path<'_, Param>> {
    from_vec(&self.parent, &self.node.params, |i| {
      AstParentNodeRef::Function(self.node, FunctionField::Params(i))
    })
  }

  pub fn body(&self) -> Option<Path<'_, BlockStmt>> {
    from_opt(
      &self.parent,
      &self.node.body,
      AstParentNodeRef::Function(self.node, FunctionField::Body),
    )
  }
}

mod param {}
impl Path<'_, Param> {
  pub fn decorators(&self) -> PathVec<'_, Decorator> {
    from_vec(&self.parent, &self.node.decorators, |i| {
      AstParentNodeRef::Param(self.node, ParamField::Decorators(i))
    })
  }

  pub fn pat(&self) -> Path<'_, Pat> {
    from_val(
      &self.parent,
      &self.node.pat,
      AstParentNodeRef::Param(self.node, ParamField::Pat),
    )
  }
}

impl<'a> Path<'a, UpdateExpr> {
  pub fn op(&'a self) -> Path<'a, UpdateOp> {
    self.sub(|p| (ARef::UpdateExpr(p, UpdateExprField::Op), &p.op))
  }

  pub fn arg(&'a self) -> Path<'a, Expr> {
    self.sub(|p| (ARef::UpdateExpr(p, UpdateExprField::Arg), p.arg.as_ref()))
  }
}
