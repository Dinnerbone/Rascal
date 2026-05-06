use crate::internal::as2::hir::visitor::{MutVisitor, walk_expr, walk_statement};
use crate::internal::as2::hir::{
    ConstantKind, Declaration, Expr, ExprKind, Function, FunctionArgument, StatementKind,
};
use indexmap::{IndexMap, IndexSet};
use serde::Serialize;

#[derive(Debug, Clone, Serialize, PartialEq, Default)]
pub struct Variable {
    pub used: bool,
    pub used_in_inner_scope: bool,
}
#[derive(Debug, Clone, Serialize, PartialEq, Default)]
pub struct Scope {
    pub defined_variables: IndexMap<String, Variable>,
    pub referenced_variables: IndexSet<String>,
    pub could_reference_anything: bool,
}

impl Scope {
    pub fn for_function(args: &Vec<FunctionArgument>, body: &mut Vec<StatementKind>) -> Self {
        let mut scope = Scope::default();
        scope
            .defined_variables
            .insert("this".to_string(), Variable::default());
        scope
            .defined_variables
            .insert("super".to_string(), Variable::default());
        for arg in args {
            scope
                .defined_variables
                .insert(arg.name.clone(), Variable::default());
        }
        let mut finder = VariableFinder(scope);
        for statement in body {
            finder.visit_statement(statement);
        }
        finder.0
    }

    pub fn for_root(body: &mut Vec<StatementKind>) -> Self {
        let mut finder = VariableFinder(Scope::default());
        for statement in body {
            finder.visit_statement(statement);
        }
        finder.0
    }
}

struct VariableFinder(Scope);

impl MutVisitor for VariableFinder {
    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.value {
            ExprKind::Constant(ConstantKind::Identifier(name)) => {
                if let Some(variable) = self.0.defined_variables.get_mut(name) {
                    variable.used = true;
                } else {
                    self.0.referenced_variables.insert(name.clone());
                }
            }
            ExprKind::GetVariable(_) => {
                self.0.could_reference_anything = true;
            }
            _ => {}
        }
        walk_expr(self, expr)
    }

    fn visit_function(&mut self, function: &mut Function) {
        // A totally different scope! We don't need to walk it, we can just inspect it.
        self.0.could_reference_anything |= function.scope.could_reference_anything;
        for name in &function.scope.referenced_variables {
            if let Some(variable) = self.0.defined_variables.get_mut(name) {
                variable.used = true;
                variable.used_in_inner_scope = true;
            } else {
                self.0.referenced_variables.insert(name.clone());
            }
        }
    }

    fn visit_declaration(&mut self, declaration: &mut Declaration) {
        self.0.defined_variables.insert(
            declaration.name.value.clone(),
            Variable {
                used: true,
                ..Default::default()
            },
        );
        if let Some(value) = &mut declaration.value {
            self.visit_expr(value);
        }
    }

    fn visit_statement(&mut self, statement: &mut StatementKind) {
        if let StatementKind::With { .. } = statement {
            self.0.could_reference_anything = true;
        }
        walk_statement(self, statement);
    }
}
