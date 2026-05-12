use crate::internal::as2::hir::visitor::{MutVisitor, walk_expr, walk_statement};
use crate::internal::as2::hir::{
    ConstantKind, Declaration, EnumeratorTarget, Expr, ExprKind, ForCondition, Function,
    FunctionArgument, StatementKind,
};
use crate::internal::span::Spanned;
use indexmap::{IndexMap, IndexSet};
use serde::Serialize;

#[derive(Debug, Clone, Serialize, PartialEq, Default)]
pub struct Variable {
    pub used: bool,
    pub used_in_inner_scope: bool,
    pub type_name: Option<Spanned<String>>,
}
#[derive(Debug, Clone, Serialize, PartialEq, Default)]
pub struct Scope {
    pub defined_variables: IndexMap<String, Variable>,
    pub referenced_variables: IndexSet<String>,
    pub could_reference_anything: bool,
}

impl Scope {
    pub fn for_function(
        args: &Vec<FunctionArgument>,
        body: &mut Vec<StatementKind>,
        this_type: Option<Spanned<String>>,
        super_type: Option<Spanned<String>>,
    ) -> Self {
        let mut scope = Scope::default();
        // These must be defined before any arguments, in this order: this arguments super _root _parent _global
        scope.defined_variables.insert(
            "this".to_string(),
            Variable {
                type_name: this_type,
                ..Default::default()
            },
        );
        scope
            .defined_variables
            .insert("arguments".to_string(), Variable::default());
        scope.defined_variables.insert(
            "super".to_string(),
            Variable {
                type_name: super_type,
                ..Default::default()
            },
        );
        scope
            .defined_variables
            .insert("_root".to_string(), Variable::default());
        scope
            .defined_variables
            .insert("_parent".to_string(), Variable::default());
        scope
            .defined_variables
            .insert("_global".to_string(), Variable::default());
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

impl VariableFinder {
    fn mark_variable_used(&mut self, name: &str, from_inner: bool) {
        if let Some(variable) = self.0.defined_variables.get_mut(name) {
            variable.used = true;
            variable.used_in_inner_scope |= from_inner;
        } else {
            self.0.referenced_variables.insert(name.to_string());
        }
    }

    fn declare_new_variable(&mut self, name: String, type_name: Option<Spanned<String>>) {
        self.0.defined_variables.insert(
            name,
            Variable {
                used: true,
                type_name,
                ..Default::default()
            },
        );
    }
}

impl MutVisitor for VariableFinder {
    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.value {
            ExprKind::Constant(ConstantKind::Identifier(name)) => {
                self.mark_variable_used(name, false);
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
            self.mark_variable_used(name, true);
        }
    }

    fn visit_declaration(&mut self, declaration: &mut Declaration) {
        self.declare_new_variable(
            declaration.name.value.clone(),
            declaration.type_name.clone(),
        );
        if let Some(value) = &mut declaration.value {
            self.visit_expr(value);
        }
    }

    fn visit_statement(&mut self, statement: &mut StatementKind) {
        match statement {
            StatementKind::With { .. } => {
                self.0.could_reference_anything = true;
            }
            StatementKind::ForIn {
                condition:
                    ForCondition::Enumerate {
                        target: EnumeratorTarget::Variable { name, declare },
                        ..
                    },
                ..
            } => {
                if *declare {
                    self.declare_new_variable(name.clone(), None);
                } else {
                    self.mark_variable_used(name, false);
                }
            }
            _ => {}
        }
        walk_statement(self, statement);
    }
}
