//! Abstract Syntax Tree (AST) module

/// Number    ::= INT_CONST;
#[derive(Debug)]
pub struct Number {
    pub value: i32,
}

/// UnaryOp     ::= "+" | "-" | "!";
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

/// BinaryOp    ::= "+" | "-" | "*" | "/" | "%"
#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}

/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum Exp {
    Number {
        value: Number,
    },
    UnaryExp {
        unary_op: UnaryOp,
        exp: Box<Exp>,
    },
    BinaryExp{
        binary_op: BinaryOp,
        lhs: Box<Exp>,
        rhs: Box<Exp>
    }
}

/// Stmt        ::= "return" Exp ";"
#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp,
}

/// Ident
#[derive(Debug)]
pub struct Ident {
    pub name: String,
}

/// Block     ::= "{" Stmt "}";
#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

/// FuncType  ::= "int";
#[derive(Debug, Clone, Copy)]
pub enum FuncType {
    Int,
}

/// FuncDef   ::= FuncType IDENT "(" ")" Block;
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: Ident,
    pub block: Block,
}

/// CompUnit  ::= FuncDef;
#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}
