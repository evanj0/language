module Ir

open Identifier

type Uid =
    {
        index: int
    }

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
        | Opaque of name: Ident * inner: Type
        | Application of t: Type * arguments: Type List
        | Constructor of parameters: string list * bounds: Bound list * body: Type
        | Primitive of Primitive
        | Unknown of id: int
        | Uninstantiated of name: string
        | Any
        | Tuple of elements: Type list
        | Overloaded of overloads: (Uid * Type) list

    and Primitive =
        | Str
        | Int
        | Real
        | Char
        | Bool

    and Bound = 
        {
            parameters: string list
            functionName: string
            t: Type
        }

type Type = Type.Type

[<RequireQualifiedAccess>]
module Pattern =
    type Pattern =
        | Ident of name: string