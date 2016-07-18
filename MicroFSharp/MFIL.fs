module MFIL
(* Micro-F# Intermediate Language abstract syntax tree *)

type ILType = 
    | ILInt
    | ILBool
    | ILList
    | ILTree
    | ILFunc of ILType * ILType

type Variant = 
    | ILNil
    | ILCons
    | ILLeaf
    | ILNode

type VariantValue = 
    | ListValue
    | TreeValue
    | Next
    | Left
    | Right

type Instruction = 
    | PushInt of int
    | PushBool of bool
    | Add
    | Mul 
    | Mod 
    | Div 

    | Sub 
    | Gt  // >
    | Lt  // <
    | Eq  // =
    | Neq // <>
    | Ge  // >=
    | Le  // <=
    | Print of ILType
    | PrintAscii
    | Load of string
    | StoreLocal of string * ILType
    | Branch of Instruction list * Instruction list
    | CallInvoke of string * bool * Instruction list
    | CreateObject of string
    | StoreField of string * string
    | CallConstructVariant of Variant
    | CallCheckVariant of Variant
    | ExtractVariantVal of VariantValue

type Field = 
    | Field of string * ILType

type Class = 
    | FunClass of string * string * (ILType * ILType) * Field list * Instruction list
    | EntryClass of string * Instruction list