use crate::internal::as2::hir::scope::Scope;
use crate::internal::as2::hir::visitor::{MutVisitor, walk_expr, walk_program, walk_statement};
use crate::internal::as2::hir::{BinaryOperator, ConstantKind, Expr, ExprKind, Function};
use crate::internal::as2::resolver::identifiers_to_path;
use crate::internal::definitions::{FieldDefinition, ProgramDefinition};

struct VirtualPropertyOptimizer {
    definition: ProgramDefinition,
    scope: Scope,
}

struct FieldInfo<'a> {
    owner: &'a Expr,
    definition: &'a FieldDefinition,
}

impl VirtualPropertyOptimizer {
    pub fn get_field<'a>(&'a self, expr: &'a ExprKind) -> Option<FieldInfo<'a>> {
        let mut path = identifiers_to_path(expr).ok()?;
        if path.len() < 2 {
            return None;
        }
        let first = path.remove(0);
        let last = path.pop()?;
        let mut type_name = if let Some(first_variable) = self.scope.defined_variables.get(first)
            && let Some(type_name) = &first_variable.type_name
        {
            &type_name.value
        } else {
            return None;
        };

        path.reverse(); // turn [foo, bar, baz] into [baz, bar, foo] for easy iteration
        while let Some(segment) = path.pop() {
            if let Some(type_definition) = self.definition.get_field(type_name, segment) {
                if let Some(next_name) = &type_definition.type_name {
                    type_name = next_name;
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }

        let ExprKind::Field(owner, _) = expr else {
            unreachable!()
        };

        Some(FieldInfo {
            owner,
            definition: self.definition.get_field(type_name, last)?,
        })
    }
}

impl MutVisitor for VirtualPropertyOptimizer {
    fn visit_expr(&mut self, expr: &mut Expr) {
        if let ExprKind::BinaryOperator(
            BinaryOperator::Assign | BinaryOperator::AddAssign,
            path,
            value,
        ) = &mut expr.value
        {
            if let Some(field) = self.get_field(&path.value)
                && field.definition.is_virtual
            {
                expr.value = ExprKind::Call {
                    name: Box::new(Expr::new(
                        expr.span,
                        ExprKind::Field(
                            Box::new(field.owner.clone()),
                            Box::new(Expr::new(
                                expr.span,
                                ExprKind::Constant(ConstantKind::String(format!(
                                    "__set__{}",
                                    field.definition.name.clone()
                                ))),
                            )),
                        ),
                    )),
                    args: vec![*value.clone()],
                };
            };
            return;
        }

        walk_expr(self, expr);

        if let Some(field) = self.get_field(&expr.value)
            && field.definition.is_virtual
        {
            expr.value = ExprKind::Call {
                name: Box::new(Expr::new(
                    expr.span,
                    ExprKind::Field(
                        Box::new(field.owner.clone()),
                        Box::new(Expr::new(
                            expr.span,
                            ExprKind::Constant(ConstantKind::String(format!(
                                "__get__{}",
                                field.definition.name.clone()
                            ))),
                        )),
                    ),
                )),
                args: vec![],
            };
        };
    }

    fn visit_function(&mut self, function: &mut Function) {
        let old_scope = std::mem::replace(&mut self.scope, function.scope.clone());

        for statement in &mut function.body {
            walk_statement(self, statement);
        }

        self.scope = old_scope;
    }
}

pub fn optimize_virtual_properties(program: &mut crate::Program) {
    let definition = ProgramDefinition::from(&*program);
    let mut promoter = VirtualPropertyOptimizer {
        definition,
        scope: Scope::default(),
    };
    walk_program(&mut promoter, program);
}
