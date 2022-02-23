module Ir

open Identifier

[<RequireQualifiedAccess>]
module Literal =
    type Literal =
        | Str of string
        | Int of int64
        | Real of float
        | Char of char
        | Bool of bool

[<RequireQualifiedAccess>]
module Type =
    type Type =
        | Opaque of name: Ident * parameters: Type list
        | Primitive of Primitive
        | Unknown of id: int

    and Primitive =
        | Str
        | Int
        | Real
        | Char
        | Bool

[<RequireQualifiedAccess>]
module Pattern =
    type Pattern =
        | Ident of name: string