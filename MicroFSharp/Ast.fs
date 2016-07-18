module Ast

type TypeDef =
   | FuncType of TypeDef * TypeDef
   | IntType
   | BoolType
   | ListType
   | TreeType

type MatchExpr =
  | NilMatch
  | ConsMatch of string * string  
  | LeafMatch
  | NodeMatch of string * string * string

type Expr =
  | TypedExpr of Expr * TypeDef
  | ConstInt of int
  | ConstBool of bool
  | Var of string
  | Let of string * Expr * Expr
  | PrimOp of string * Expr * Expr
  | If of Expr * Expr * Expr
  | Match of Expr * (MatchExpr * Expr) list 
  | FunCall of Expr * Expr
  | TailFunCall of Expr * Expr
  | Constructor of Constructor

and Constructor =
  | Nil
  | Cons of Expr * Expr
  | Leaf
  | Node of Expr * Expr * Expr 
   
type FuncDef = FuncDef of string * string list * TypeDef * Expr

type Program = Program of FuncDef list * Expr