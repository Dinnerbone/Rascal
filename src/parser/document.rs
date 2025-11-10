use crate::lexer::tokens::{BinaryOperator, Keyword, TokenKind};
use crate::parser::Tokens;
use crate::parser::expr::{Constant, Eval, Expr};
use serde::Serialize;
use winnow::combinator::{alt, cond, cut_err, eof, fail, opt, peek, repeat_till};
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::stream::Stream;
use winnow::token::any;
use winnow::{ModalResult, Parser};

#[derive(Debug, Serialize)]
pub struct Document {
    #[allow(dead_code)]
    expressions: Vec<Expr>,
}

pub fn document(tokens: &mut Tokens<'_>) -> ModalResult<Document> {
    let (expressions, _) = repeat_till(0.., statement.context(StrContext::Label("statement")), eof)
        .parse_next(tokens)?;
    Ok(Document { expressions })
}

fn statement(i: &mut Tokens<'_>) -> ModalResult<Expr> {
    let expr = expr.parse_next(i)?;
    alt((TokenKind::Semicolon, TokenKind::Newline))
        .context(StrContext::Label("end of line"))
        .parse_next(i)?;
    opt(TokenKind::Newline).parse_next(i)?;

    Ok(expr)
}

fn expr(i: &mut Tokens<'_>) -> ModalResult<Expr> {
    let checkpoint = i.checkpoint();
    let token = any.parse_next(i)?;
    match token.kind {
        TokenKind::Keyword(Keyword::Var) => declaration.context(StrContext::Label("declaration")),
        _ => {
            i.reset(&checkpoint);
            return eval.parse_next(i).map(Expr::EVal);
        }
    }
    .parse_next(i)
}

fn eval(i: &mut Tokens<'_>) -> ModalResult<Eval> {
    let token = peek(any).parse_next(i)?;
    match token.kind {
        TokenKind::String => {
            let val = string.parse_next(i)?;
            eval_next(Eval::Constant(Constant::String(val)))
                .context(StrContext::Label("declaration"))
        }
        TokenKind::Identifier => {
            let val = identifier.parse_next(i)?;
            eval_next(Eval::Constant(Constant::Identifier(val)))
                .context(StrContext::Label("declaration"))
        }
        _ => {
            return fail.parse_next(i);
        }
    }
    .parse_next(i)
}

fn eval_next<'i>(prior: Eval) -> impl Parser<Tokens<'i>, Eval, ErrMode<ContextError>> {
    move |i: &mut Tokens<'i>| {
        let token = peek(any).parse_next(i)?;
        let prior = prior.clone();
        match token.kind {
            TokenKind::OpenParen => {
                TokenKind::OpenParen.parse_next(i)?;
                let args = eval_list
                    .context(StrContext::Label("arguments"))
                    .parse_next(i)?;
                TokenKind::CloseParen.parse_next(i)?;
                Ok(Eval::Call {
                    name: Box::new(prior),
                    args,
                })
            }
            TokenKind::BinaryOperator(op) => {
                TokenKind::BinaryOperator(op).parse_next(i)?;
                eval.parse_next(i)
                    .map(|next| Eval::BinaryOperator(op, Box::new(prior), Box::new(next)))
            }
            _ => Ok(prior),
        }
    }
}

fn string(i: &mut Tokens<'_>) -> ModalResult<String> {
    Ok(TokenKind::String.parse_next(i)?.raw.to_string())
}

fn identifier(i: &mut Tokens<'_>) -> ModalResult<String> {
    Ok(TokenKind::Identifier.parse_next(i)?.raw.to_string())
}

fn declaration(i: &mut Tokens<'_>) -> ModalResult<Expr> {
    let name = cut_err(skip_newline(identifier))
        .context(StrContext::Label("variable name"))
        .parse_next(i)?;
    let equals = cut_err(opt(skip_newline(TokenKind::BinaryOperator(
        BinaryOperator::Assign,
    ))))
    .parse_next(i)?
    .is_some();
    let value = cond(equals, skip_newline(eval)).parse_next(i)?;

    Ok(Expr::Declare { name, value })
}

fn eval_list(i: &mut Tokens<'_>) -> ModalResult<Vec<Eval>> {
    Ok(vec![eval.parse_next(i)?])
}

fn skip_newline<'i, O, P>(mut inner: P) -> impl Parser<Tokens<'i>, O, ErrMode<ContextError>>
where
    P: Parser<Tokens<'i>, O, ErrMode<ContextError>>,
{
    move |input: &mut Tokens<'i>| {
        opt(TokenKind::Newline).parse_next(input)?;
        inner.parse_next(input)
    }
}
