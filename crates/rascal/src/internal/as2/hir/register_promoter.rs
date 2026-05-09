use crate::internal::as2::ast::BinaryOperator;
use crate::internal::as2::hir::visitor::{MutVisitor, walk_expr, walk_statement};
use crate::internal::as2::hir::{ConstantKind, Document, Expr, ExprKind, Function, StatementKind};
use crate::internal::span::Span;
use indexmap::IndexMap;

#[derive(Default)]
struct RegisterPromoter {
    registers: IndexMap<String, u8>,
}

impl MutVisitor for RegisterPromoter {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);

        if let ExprKind::Constant(ConstantKind::Identifier(identifier)) = &expr.value
            && let Some(register) = self.registers.get(identifier)
        {
            expr.value = ExprKind::Constant(ConstantKind::Register(*register));
        }
    }

    fn visit_statement(&mut self, statement: &mut StatementKind) {
        if let Some((register, value)) = match statement {
            StatementKind::Declare(declaration) => self
                .registers
                .get(&declaration.name.value)
                .map(|&r| (r, declaration.value.clone())),
            _ => None,
        } {
            if let Some(value) = value {
                *statement = StatementKind::Expr(Expr::new(
                    Span::default(),
                    ExprKind::BinaryOperator(
                        BinaryOperator::Assign,
                        Box::new(Expr::new(
                            Span::default(),
                            ExprKind::Constant(ConstantKind::Register(register)),
                        )),
                        Box::new(value),
                    ),
                ));
            } else {
                *statement = StatementKind::Block(vec![]);
            }
        }
        walk_statement(self, statement);
    }

    fn visit_function(&mut self, function: &mut Function) {
        let prev_registers = std::mem::take(&mut self.registers);
        if !function.scope.could_reference_anything {
            // Start suppressing everything and then unsuppress what we need
            function.suppress_this = true;
            function.suppress_arguments = true;
            function.suppress_super = true;

            let mut register = 1; // 0 is reserved!
            for (name, variable) in &function.scope.defined_variables {
                if variable.used && !variable.used_in_inner_scope {
                    self.registers.insert(name.clone(), register);
                    if let Some(arg) = function
                        .signature
                        .args
                        .iter_mut()
                        .find(|arg| arg.name == *name)
                    {
                        arg.register = Some(register);
                    }
                    register += 1;
                    if name == "this" {
                        function.preload_this = true;
                        function.suppress_this = false;
                    }
                    if name == "arguments" {
                        function.preload_arguments = true;
                        function.suppress_arguments = false;
                    }
                    if name == "super" {
                        function.preload_super = true;
                        function.suppress_super = false;
                    }
                    if name == "_root" {
                        function.preload_root = true;
                    }
                    if name == "_parent" {
                        function.preload_parent = true;
                    }
                    if name == "_global" {
                        function.preload_global = true;
                    }
                }
            }
            function.register_count = register;
        }

        for statement in &mut function.body {
            self.visit_statement(statement);
        }

        self.registers = prev_registers;
    }
}

pub fn promote_variables_to_registers(document: &mut Document) {
    let mut promoter = RegisterPromoter::default();

    match document {
        Document::Script { statements, .. } => {
            for statement in statements {
                promoter.visit_statement(statement);
            }
        }
        Document::Interface(_interface) => {}
        Document::Class(_class) => {}
        Document::Invalid => {}
    }
}
