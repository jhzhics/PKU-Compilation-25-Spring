//! Lexer module
use logos::Logos;
use logos::Lexer;

#[derive(Logos, Debug, Clone)]
#[logos(skip r#"\s|(//.*\n)|(/\*([^*]|(\*+[^*/]))*\*+/)"#)]
pub enum Token{
    #[token("const")]
    CONST,
    #[token("if")]
    IF,
    #[token("else")]
    ELSE,
    #[token("while")]
    WHILE,
    #[token("break")]
    BREAK,
    #[token("continue")]
    CONTINUE,
    #[token("return")]
    RETURN,
    #[token("int")]
    INT,
    #[token("void")]
    VOID,
    #[token(",")]
    COMMA,
    #[token(";")]
    SEMI,
    #[token("(")]
    LPAREN,
    #[token(")")]
    RPAREN,
    #[token("[")]
    LBRAKET,
    #[token("]")]
    RBRAKET,
    #[token("{")]
    LCURLY,
    #[token("}")]
    RCURLY,
    #[token("+")]
    PLUS,
    #[token("-")]
    MINUS,
    #[token("*")]
    MUL,
    #[token("/")]
    DIV,
    #[token("%")]
    MOD,
    #[token("<")]
    LT,
    #[token(">")]
    GT,
    #[token("<=")]
    LE,
    #[token(">=")]
    GE,
    #[token("==")]
    EQ,
    #[token("!=")]
    NE,
    #[token("&&")]
    AND,
    #[token("||")]
    OR,
    #[token("!")]
    NOT,
    #[token("=")]
    ASSIGN,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", ident_callback)]
    IDENT(String),
    #[regex(r"[+-]?([0-9])+", num_callback)]
    NUM(i32),
}


fn ident_callback(lex: &mut Lexer<Token>) -> Option<String> {
    lex.slice()
        .chars()
        .collect::<String>()
        .into()
}

fn num_callback(lex: &mut Lexer<Token>) -> Option<i32> {
    lex.slice()
        .parse::<i32>()
        .ok()
}