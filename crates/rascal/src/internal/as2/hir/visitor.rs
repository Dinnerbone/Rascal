use crate::internal::as2::hir::{
    Declaration, Expr, ExprKind, ForCondition, Function, StatementKind, SwitchElement,
};

pub trait MutVisitor {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);
    }

    fn visit_function(&mut self, function: &mut Function) {
        for statement in &mut function.body {
            walk_statement(self, statement);
        }
    }

    fn visit_statement(&mut self, statement: &mut StatementKind) {
        walk_statement(self, statement);
    }

    fn visit_declaration(&mut self, declaration: &mut Declaration) {
        if let Some(value) = &mut declaration.value {
            self.visit_expr(value);
        }
    }
}

pub fn walk_statement<V: MutVisitor + ?Sized>(visitor: &mut V, statement: &mut StatementKind) {
    match statement {
        StatementKind::Declare(declarations) => {
            for declaration in declarations {
                visitor.visit_declaration(declaration);
            }
        }
        StatementKind::Return(exprs) => {
            for expr in exprs {
                visitor.visit_expr(expr);
            }
        }
        StatementKind::Throw(exprs) => {
            for expr in exprs {
                visitor.visit_expr(expr);
            }
        }
        StatementKind::Expr(expr) => {
            visitor.visit_expr(expr);
        }
        StatementKind::Block(statements) => {
            for statement in statements {
                visitor.visit_statement(statement);
            }
        }
        StatementKind::ForIn { condition, body } => {
            match condition {
                ForCondition::Enumerate { object, .. } => visitor.visit_expr(object),
                ForCondition::Classic {
                    initialize,
                    condition,
                    update,
                } => {
                    if let Some(initialize) = initialize {
                        visitor.visit_statement(initialize);
                    }
                    for expr in condition {
                        visitor.visit_expr(expr);
                    }
                    for expr in update {
                        visitor.visit_expr(expr);
                    }
                }
            }
            visitor.visit_statement(body);
        }
        StatementKind::While { condition, body } => {
            visitor.visit_expr(condition);
            visitor.visit_statement(body);
        }
        StatementKind::If { condition, yes, no } => {
            visitor.visit_expr(condition);
            visitor.visit_statement(yes);
            if let Some(no) = no {
                visitor.visit_statement(no);
            }
        }
        StatementKind::Break => {}
        StatementKind::Continue => {}
        StatementKind::Try(try_catch) => {
            for catch in &mut try_catch.try_body {
                visitor.visit_statement(catch);
            }
            for (_, catch) in &mut try_catch.typed_catches {
                for catch in &mut catch.body {
                    visitor.visit_statement(catch);
                }
            }
            if let Some(catch) = &mut try_catch.catch_all {
                for catch in &mut catch.body {
                    visitor.visit_statement(catch);
                }
            }
            for statement in &mut try_catch.finally {
                visitor.visit_statement(statement);
            }
        }
        StatementKind::WaitForFrame {
            frame,
            scene,
            if_loaded,
        } => {
            visitor.visit_expr(frame);
            if let Some(scene) = scene {
                visitor.visit_expr(scene);
            }
            visitor.visit_statement(if_loaded);
        }
        StatementKind::TellTarget { target, body } => {
            visitor.visit_expr(target);
            visitor.visit_statement(body);
        }
        StatementKind::InlinePCode(_) => {}
        StatementKind::With { target, body } => {
            visitor.visit_expr(target);
            visitor.visit_statement(body);
        }
        StatementKind::Switch { target, elements } => {
            visitor.visit_expr(target);
            for element in elements {
                match element {
                    SwitchElement::Case(case) => visitor.visit_expr(case),
                    SwitchElement::Default => {}
                    SwitchElement::Statement(stmt) => visitor.visit_statement(stmt),
                }
            }
        }
    }
}

pub fn walk_expr<V: MutVisitor + ?Sized>(visitor: &mut V, expr: &mut Expr) {
    match &mut expr.value {
        ExprKind::Constant(_constant) => (),
        ExprKind::Call { name, args } => {
            visitor.visit_expr(name);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        ExprKind::New { name, args } => {
            visitor.visit_expr(name);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        ExprKind::DuplicateMovieClip {
            source,
            target,
            depth,
        } => {
            visitor.visit_expr(source);
            visitor.visit_expr(target);
            visitor.visit_expr(depth);
        }
        ExprKind::AsciiToChar(expr) => visitor.visit_expr(expr),
        ExprKind::MBAsciiToChar(expr) => visitor.visit_expr(expr),
        ExprKind::CallFrame(expr) => visitor.visit_expr(expr),
        ExprKind::GetTime => {}
        ExprKind::BinaryOperator(_, left, right) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        ExprKind::UnaryOperator(_, expr) => visitor.visit_expr(expr),
        ExprKind::Ternary { condition, yes, no } => {
            visitor.visit_expr(condition);
            visitor.visit_expr(yes);
            visitor.visit_expr(no);
        }
        ExprKind::InitObject(values) => {
            for (_key, value) in values {
                visitor.visit_expr(value);
            }
        }
        ExprKind::InitArray(values) => {
            for value in values {
                visitor.visit_expr(value);
            }
        }
        ExprKind::Field(parent, child) => {
            visitor.visit_expr(parent);
            visitor.visit_expr(child);
        }
        ExprKind::TypeOf(expr) => visitor.visit_expr(expr),
        ExprKind::Delete(expr) => visitor.visit_expr(expr),
        ExprKind::Void(expr) => visitor.visit_expr(expr),
        ExprKind::Function(function) => {
            visitor.visit_function(function);
        }
        ExprKind::GetVariable(name) => visitor.visit_expr(name),
        ExprKind::SetVariable(name, value) => {
            visitor.visit_expr(name);
            visitor.visit_expr(value);
        }
        ExprKind::GotoFrame(frame, _) => visitor.visit_expr(frame),
        ExprKind::GetUrl { target, url, .. } => {
            visitor.visit_expr(target);
            visitor.visit_expr(url);
        }
        ExprKind::CastToInteger(value) => visitor.visit_expr(value),
        ExprKind::CastToNumber(value) => visitor.visit_expr(value),
        ExprKind::CastToString(value) => visitor.visit_expr(value),
        ExprKind::StringLength(value) => visitor.visit_expr(value),
        ExprKind::MBStringLength(value) => visitor.visit_expr(value),
        ExprKind::CharToAscii(value) => visitor.visit_expr(value),
        ExprKind::MBCharToAscii(value) => visitor.visit_expr(value),
        ExprKind::Substring {
            string,
            start,
            length,
        } => {
            visitor.visit_expr(string);
            visitor.visit_expr(start);
            visitor.visit_expr(length);
        }
        ExprKind::MBSubstring {
            string,
            start,
            length,
        } => {
            visitor.visit_expr(string);
            visitor.visit_expr(start);
            visitor.visit_expr(length);
        }
        ExprKind::NextFrame => {}
        ExprKind::PreviousFrame => {}
        ExprKind::Play => {}
        ExprKind::Stop => {}
        ExprKind::StopSounds => {}
        ExprKind::StartDrag {
            target,
            constraints,
            ..
        } => {
            visitor.visit_expr(target);
            if let Some((a, b, c, d)) = constraints {
                visitor.visit_expr(a);
                visitor.visit_expr(b);
                visitor.visit_expr(c);
                visitor.visit_expr(d);
            }
        }
        ExprKind::EndDrag => {}
        ExprKind::GetTargetPath(expr) => visitor.visit_expr(expr),
        ExprKind::Trace(expr) => visitor.visit_expr(expr),
        ExprKind::RemoveSprite(expr) => visitor.visit_expr(expr),
        ExprKind::GetRandomNumber(expr) => visitor.visit_expr(expr),
        ExprKind::GetProperty(obj, _) => visitor.visit_expr(obj),
        ExprKind::SetProperty(obj, _, value) => {
            visitor.visit_expr(obj);
            visitor.visit_expr(value);
        }
        ExprKind::ToggleQuality => {}
    }
}
