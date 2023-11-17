use swc_common::Spanned;
use swc_ecma_ast::*;
use swc_ecma_visit::{fields::*, AstParentNodeRef};

#[derive(Clone, Copy)]
pub struct PathSeg<'a> {
  pub parent: &'a Option<PathSeg<'a>>,
  pub node_ref: AstParentNodeRef<'a>,
}

#[derive(Clone, Copy)]
pub struct Path<'a, N> {
  pub parent: Option<PathSeg<'a>>,
  pub node: &'a N,
}

impl<N: Spanned> Spanned for Path<'_, N> {
  fn span(&self) -> swc_common::Span {
    self.node.span()
  }
}

pub fn fake_path<'a, T>(node: &'a T) -> Path<'a, T> {
  Path { parent: None, node }
}

impl<'a> Path<'a, ArrayLit> {
  pub fn elems(
    &'a self,
  ) -> impl Iterator<Item = Option<Path<'a, ExprOrSpread>>> {
    self.node.elems.iter().enumerate().map(|(i, elem)| {
      elem.as_ref().map(|elem| Path {
        parent: Some(PathSeg {
          parent: &self.parent,
          node_ref: AstParentNodeRef::ArrayLit(
            self.node,
            ArrayLitField::Elems(i),
          ),
        }),
        node: elem,
      })
    })
  }
}

fn from_val<'a, T>(
  parent: &'a Option<PathSeg<'a>>,
  node: &'a T,
  node_ref: AstParentNodeRef<'a>,
) -> Path<'a, T> {
  Path {
    parent: Some(PathSeg { parent, node_ref }),
    node,
  }
}

fn from_opt<'a, T>(
  parent: &'a Option<PathSeg<'a>>,
  node: &'a Option<T>,
  node_ref: AstParentNodeRef<'a>,
) -> Option<Path<'a, T>> {
  node.as_ref().map(|node| Path {
    parent: Some(PathSeg { parent, node_ref }),
    node,
  })
}

fn from_vec<'a, T, F: Fn(usize) -> AstParentNodeRef<'a>>(
  parent: &'a Option<PathSeg<'a>>,
  vec: &'a [T],
  make_ref: F,
) -> Vec<Path<'a, T>> {
  vec
    .iter()
    .enumerate()
    .map(move |(i, node)| Path {
      parent: Some(PathSeg {
        parent,
        node_ref: make_ref(i),
      }),
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

impl<'a> Path<'a, UnaryExpr> {
  pub fn op(&'a self) -> Path<'a, UnaryOp> {
    Path {
      parent: Some(PathSeg {
        parent: &self.parent,
        node_ref: AstParentNodeRef::UnaryExpr(self.node, UnaryExprField::Op),
      }),
      node: &self.node.op,
    }
  }

  pub fn arg(&'a self) -> Path<'a, Expr> {
    Path {
      parent: Some(PathSeg {
        parent: &self.parent,
        node_ref: AstParentNodeRef::UnaryExpr(self.node, UnaryExprField::Arg),
      }),
      node: self.node.arg.as_ref(),
    }
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
    Path {
      parent: Some(PathSeg {
        parent: &self.parent,
        node_ref: AstParentNodeRef::UpdateExpr(self.node, UpdateExprField::Op),
      }),
      node: &self.node.op,
    }
  }

  pub fn arg(&'a self) -> Path<'a, Expr> {
    Path {
      parent: Some(PathSeg {
        parent: &self.parent,
        node_ref: AstParentNodeRef::UpdateExpr(self.node, UpdateExprField::Arg),
      }),
      node: self.node.arg.as_ref(),
    }
  }

  pub fn prefix(&'a self) -> Path<'a, bool> {
    Path {
      parent: Some(PathSeg {
        parent: &self.parent,
        node_ref: AstParentNodeRef::UpdateExpr(
          self.node,
          UpdateExprField::Prefix,
        ),
      }),
      node: &self.node.prefix,
    }
  }
}
