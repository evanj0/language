module UntypedIr

open Ir

open Range
open Identifier

[<RequireQualifiedAccess>]
module Expr =
    type Expr =
        | Tagged of expr: Expr * range: Range
        | Ident of ident: Ident
        | Literal of value: Literal.Literal
        | Tuple of elements: Expr list
        | Record of elements: (string * Expr) list
        | Block of expr: Expr * next: Expr
        | Extern of name: string * argument: Expr
        | NoRet
        | Type of expr: Expr * t: Type.Type
        | Update of expr: Expr * fields: (string * Expr) list
        | App of f: Expr * x: Expr
        | Let of name: string * expr: Expr * body: Expr
        | Cond of guard: Expr * th: Expr * el: Expr
        | Ref of expr: Expr
        | Mut of expr: Expr * value: Expr
        | Func of p: string * body: Expr
        | Match of expr: Expr * case: Case

    and Case =
        | Case of pat: Pattern.Pattern * expr: Expr * next: Case
        | Default of expr: Expr
