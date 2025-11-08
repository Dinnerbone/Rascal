use crate::lexer::tokens::{Keyword, Token, TokenKind};
use crate::parser::Tokens;
use crate::parser::expr::Expr;
use serde::Serialize;
use winnow::combinator::{alt, cond, cut_err, eof, fail, opt, peek, repeat_till};
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::token::any;
use winnow::{ModalResult, Parser, dispatch};

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

pub(crate) fn statement(i: &mut Tokens<'_>) -> ModalResult<Expr> {
    let expr = dispatch! { peek(any).map(|t: &Token| t.kind);
        TokenKind::Keyword(Keyword::Var) => declaration.context(StrContext::Label("declaration")),
        _ => expression,
    }
    .parse_next(i)?;

    alt((TokenKind::Semicolon, TokenKind::Newline))
        .context(StrContext::Label("end of line"))
        .parse_next(i)?;
    opt(TokenKind::Newline).parse_next(i)?;

    Ok(expr)
}

pub(crate) fn expression(i: &mut Tokens<'_>) -> ModalResult<Expr> {
    dispatch! { peek(any).map(|t: &Token| t.kind);
        TokenKind::Identifier => alt(
            (
                skip_newline(call).context(StrContext::Label("function call")),
                skip_newline(identifier).context(StrContext::Label("identifier")).map(Expr::Identifier))
            ),
        TokenKind::String => skip_newline(string)
            .context(StrContext::Label("string"))
            .map(Expr::String),
        _ => fail.context(StrContext::Label("expression (unexpected token)")),
    }
    .parse_next(i)
}

pub(crate) fn string(i: &mut Tokens<'_>) -> ModalResult<String> {
    Ok(TokenKind::String.parse_next(i)?.raw.to_string())
}

pub(crate) fn identifier(i: &mut Tokens<'_>) -> ModalResult<String> {
    Ok(TokenKind::Identifier.parse_next(i)?.raw.to_string())
}

pub(crate) fn declaration(i: &mut Tokens<'_>) -> ModalResult<Expr> {
    let _ = skip_newline(TokenKind::Keyword(Keyword::Var)).parse_next(i)?;
    let name = cut_err(skip_newline(identifier))
        .context(StrContext::Label("variable name"))
        .parse_next(i)?;
    let equals = cut_err(opt(skip_newline(TokenKind::Equals)))
        .parse_next(i)?
        .is_some();
    let value = cut_err(cond(equals, skip_newline(expression))).parse_next(i)?;

    Ok(Expr::Declare {
        name,
        value: value.map(Box::new),
    })
}

pub(crate) fn call(i: &mut Tokens<'_>) -> ModalResult<Expr> {
    (
        skip_newline(identifier),
        skip_newline(TokenKind::OpenParen),
        cut_err(skip_newline(expression)),
        cut_err(skip_newline(TokenKind::CloseParen)),
    )
        .parse_next(i)
        .map(|(ident, _, arg, _)| Expr::Call {
            name: ident,
            args: vec![arg],
        })
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
