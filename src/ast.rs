//! Abstract Syntax Tree (AST) module


/// Number    ::= INT_CONST;
#[derive(Debug)]
pub struct Number
{
    pub value: i32,
}

/// Stmt      ::= "return" Number ";";
#[derive(Debug)]
pub struct Stmt
{
    pub number: Number,
}

/// Ident
#[derive(Debug)]
pub struct Ident
{
    pub name: String,
}

/// Block     ::= "{" Stmt "}";
#[derive(Debug)]
pub struct Block
{
    pub stmt: Stmt
}

/// FuncType  ::= "int";
#[derive(Debug)]
pub enum FuncType
{
    Int
}

/// FuncDef   ::= FuncType IDENT "(" ")" Block;
#[derive(Debug)]
pub struct FuncDef
{
    pub func_type: FuncType,
    pub ident: Ident,
    pub block: Block,
}

/// CompUnit  ::= FuncDef;
#[derive(Debug)]
pub struct CompUnit
{
    pub func_def: FuncDef,
}


