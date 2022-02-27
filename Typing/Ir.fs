module Ir

open Identifier

open ListExtensions
open ResultExtensions
open Range

type Uid = { index: int }

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

    type TVariable = { id: int }

    [<RequireQualifiedAccess>]
    module TVariable =
        let equals a b = a.id = b.id

        let print (var: TVariable) = "" // TODO implement

    type Type =
        | Unknown of id: Unknown
        | Variable of id: TVariable
        | Overloaded of overloads: (Uid * Type) list
        | Constructor of args: TVariable list * bounds: Bound list * env: (Ident * Type) list * body: Type
        | Opaque of name: Ident * inner: Type
        | Unsafe of inner: Type
        | Reference of inner: Type
        | Primitive of Primitive
        | Conforming
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
        { parameters: TVariable list
          functionName: string
          t: Type }

    let print (this: Type) = ""

    type Error =
        { message: string
          note: string
          range: Range }

    [<RequireQualifiedAccess>]
    module Error =
        let create message range =
            { Error.message = message
              note = ""
              range = range }

        let createWithNote (message, note) range =
            { Error.message = message
              note = note
              range = range }

        // TODO improve the language and consistency of the error messages.
        // TODO rename all of these

        let typeMismatch t1 t2 =
            sprintf "Couldn't match type `%s` with type `%s`." (print t1) (print t2)
            |> create

        let ``value not found`` ident =
            sprintf "The value `%s` is not defined in the local or module scope." (Ident.print ident)
            |> create

        let ``overloaded value not found`` ident =
            sprintf
                "The value `%s` is not defined in the local scope, or in module scope as a single or overloaded value."
                (Ident.print ident)
            |> create

        let ``couldn't match tuples`` t1 t2 =
            sprintf
                "Couldn't match tuple `%s` with tuple `%s` because the numbers of elements are not equal."
                (print t1)
                (print t2)
            |> create
        
        let ``couldn't match records`` t1 t2 =
            sprintf
                "Couldn't match record `%s` with record `%s` because the numbers of elements are not equal."
                (print t1)
                (print t2)
            |> create

        let ``couldn't match records because field`` field record other =
            sprintf "Couldn't match record `%s` with record `%s` because the latter does not have a field named `%s`."
                (print record)
                (print other)
                field
            |> create

        let ``couldn't match the type variable with`` var1 t =
            (sprintf
                "Couldn't match the type variable `%s` with the type `%s`."
                (TVariable.print var1)
                (TVariable.print t),
             sprintf
                 "Restricting type variables is not allowed, since this causes types to become more general than intended.")
            |> createWithNote

        let ``insufficient type arguments`` parameter =
            sprintf
                "Unable to match the constructor parameter `%s` with a type to be used as the argument."
                (TVariable.print parameter)
            |> create

        let ``cannot resolve overloaded type`` =
            "Unable to determine the correct overload to use. The type arguements are either too general or do not refer to an existing overload."
            |> create

        let ``couldn't match variable`` var1 var2 =
            sprintf "Couldn't match variable `%s` with variable `%s`." (TVariable.print var1) (TVariable.print var2)
            |> create

        let ``couldn't match name`` n1 n2 =
            sprintf "Couldn't match the name `%s` with `%s`." (Ident.print n1) (Ident.print n2)
            |> create

    /// Directionally compares types; `other` can be more general than `this`.
    /// Runs `f` on `(this, other)` and returns all results.
    /// ```
    /// _ matches Unknown -> Ok
    /// _ matches Conforming -> Ok
    /// Union a1, a2, an matches Union a1, a2, an-1 -> Ok
    /// ```
    let rec tryMatch (f: (Type * Type) -> 'a list) (other: Type) (this: Type) : Result<'a list, Range -> Error> =
        let tryMatch other this = tryMatch f other this
        let res = f (this, other)
        let failStr msg = Error(msg |> Error.create)
        let fail e = Error (e this other)

        match this, other with
        | _, Unknown _ -> Ok res
        | _, Conforming -> Ok res
        | Variable a, Variable b ->
            if a = b then
                Ok res
            else
                Error(Error.``couldn't match variable`` a b)
        | Opaque (thisName, thisInner), Opaque (otherName, otherInner) ->
            if Ident.equals thisName otherName then
                thisInner |> tryMatch otherInner
            else
                Error(Error.``couldn't match name`` thisName otherName)
        | Primitive Str, Primitive Str -> Ok res
        | Primitive Int, Primitive Int -> Ok res
        | Primitive Real, Primitive Real -> Ok res
        | Primitive Char, Primitive Char -> Ok res
        | Primitive Bool, Primitive Bool -> Ok res
        // TODO Test for these cases to determine if they are actually unreachable
        | Overloaded _, Overloaded _ -> failStr "matches Overloaded Overloaded is unreachable"
        | Constructor _, Constructor _ -> failStr "matches Constructor Constructor is unreachable"
        | Function (thisL, thisR), Function (otherL, otherR) ->
            result {
                let! res1 = thisL |> tryMatch otherL
                let! res2 = thisR |> tryMatch otherR
                return res1 @ res2
            }
        | Tuple this, Tuple other ->
            if this.Length = other.Length then
                List.zip this other
                |> List.map (fun (t, o) -> t |> tryMatch o)
                |> Result.collect
                |> Result.map (List.concat)
            else fail Error.``couldn't match tuples``
        | Record this, Record other ->
            if this.Length = other.Length then    
                this
                |> List.map (fun (thisName, thisType) ->
                    other
                    |> List.map (fun (otherName, otherType) ->
                        if thisName = otherName then
                            thisType |> tryMatch otherType
                        else failStr "")
                    |> List.filter Result.isOk
                    |> Result.collect
                    |> Result.bind (fun results ->
                        results
                        |> List.concat
                        |> List.tryHead
                        |> Result.fromOption (Error.``couldn't match records because field`` thisName (Record this) (Record other))))
                |> Result.collect
            else fail Error.``couldn't match records``
        | Union this, Union other ->
            other // TODO correct this
            |> List.map (fun ot -> this |> List.filter (fun tt -> tt |> tryMatch ot))
            |> List.isNotEmpty
        | _ -> Error(Error.typeMismatch this other)

type Type = Type.Type

[<RequireQualifiedAccess>]
module Pattern =
    type Pattern = Ident of name: string
