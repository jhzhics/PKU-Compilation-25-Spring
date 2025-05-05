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
    LogicAnd,
    LogicOr
}

/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum Exp {
    Number {
        value: Number,
    },
    Lval {
        lval: Lval,
    },
    UnaryExp {
        unary_op: UnaryOp,
        exp: Box<Exp>,
    },
    BinaryExp{
        binary_op: BinaryOp,
        lhs: Box<Exp>,
        rhs: Box<Exp>
    },
    Call
    {
        ident: Ident,
        args: Vec<Exp>
    }
}

#[derive(Debug)]
pub enum InitVal
{
    Scalar {
        init_val: Option<Exp>
    },
    Array {
        init_vals: Vec<Box<InitVal>>
    }
}

#[derive(Debug)]
pub enum DeclEntry {
    Scalar {
        ident: Ident,
        init_val: InitVal,
    },
    Array {
        ident: Ident,
        shape: Vec<Exp>,
        init_val: InitVal,
    }
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl{btype: ValType, const_defs: Vec<DeclEntry>},
    VarDecl{btype: ValType, var_defs: Vec<DeclEntry>}
}

#[derive(Debug)]
pub enum Lval
{
    Ident {
        ident: Ident,
    },
    Array {
        ident: Ident,
        index: Vec<Exp>,
    }
}

#[derive(Debug)]
pub enum Stmt {
    Return{exp: Option<Exp>},
    Assign{lval: Lval, exp: Exp},
    Exp{exp: Option<Exp>},
    Block{block: Block},
    If{cond: Exp, then_block: Block, else_block: Option<Block>},
    While{cond: Exp, block: Block},
    Break,
    Continue
} 

///BlockItem     ::= Decl | Stmt;
#[derive(Debug)]
pub enum BlockItem
{
    Stmt{stmt: Stmt},
    Decl{decl: Decl}
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValType {
    Int,
    Void
}

#[derive(Debug)]
pub struct FuncParam {
    pub btype: ValType,
    pub ident: Ident,
}

/// FuncDef   ::= FuncType IDENT "(" ")" Block;
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: ValType,
    pub ident: Ident,
    pub params: Vec<FuncParam>,
    pub block: Block,
}

/// CompUnit  ::= FuncDef;
#[derive(Debug)]
pub struct CompUnit {
    pub func_defs: Vec<FuncDef>,
    pub decls: Vec<Decl>,
}
