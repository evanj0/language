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

    type Unknown = int

    type Variable = int

    type Type =
        | Contained of Contained
        | Separable of Separable
    
    and Contained =
        | Unknown of id: Unknown  
        | Variable of id: Variable
        | Overloaded of overloads: (Uid * Type) list
        | Constructor of args: Variable list * bounds: Bound list * env: (Ident * Type) list * body: Type
        | Opaque of name: Ident * inner: Type
        | Primitive of Primitive
        | Conforming

    and Separable =
        | Function of left: Type * right: Type
        | Tuple of elements: Type list
        | Record of elements: (string * Type) list
        | Union of elements: Type list

    and Primitive =
        | Str
        | Int
        | Real
        | Char
        | Bool

    and Bound = 
        {
            parameters: Variable list
            functionName: string
            t: Type
        }

    let print (this: Type) = ""

type Type = Type.Type

[<RequireQualifiedAccess>]
module Pattern =
    type Pattern =
        | Ident of name: string