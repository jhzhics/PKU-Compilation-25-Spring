//! Abstract Syntax Tree (AST) module
//! 
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
    Mod,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or
}

/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum Exp {
    Number {
        value: Number,
    },
    Ident {
        ident: Ident,
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

///BlockItem     ::= Decl | Stmt;
#[derive(Debug)]
pub enum BlockItem
{
    Return{exp: Exp},
    ConstDecl{btype: ValType, const_defs:Vec<(Ident, Exp)>}
}

/// Ident
#[derive(Debug)]
pub struct Ident {
    pub name: String,
}

/// Block     ::= "{" Stmt "}";
#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>
}

/// FuncType  ::= "int";
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValType {
    Int,
}

/// FuncDef   ::= FuncType IDENT "(" ")" Block;
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: ValType,
    pub ident: Ident,
    pub block: Block,
}

/// CompUnit  ::= FuncDef;
#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}
