module Ir

open Identifier

open ListExtensions
open ResultExtensions
open Range

type Uid = 
    { index: int
      label: string }
[<RequireQualifiedAccess>]
module Uid =
    let create id = { Uid.index = id; label = "" }

    let print uid = sprintf "%s`%d" uid.label uid.index

    /// Ignores the label.
    let equals a b = a.index = b.index

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

    type TUnknown = { id: int }

    module TUnknown =
        let create id = { TUnknown.id = id }

        let equals (a: TUnknown) b = a.id = b.id

    type TVariable = { id: int }

    [<RequireQualifiedAccess>]
    module TVariable =
        let create id = { TVariable.id = id }

        let equals (a: TVariable) b = a.id = b.id

        let print (var: TVariable) = "" // TODO implement

    type Type =
        /// Type inference variable. 
        | Unknown of id: TUnknown
        /// Type variable in a type constructor body. This being separate from inference variables means that
        /// instantiation is not necessary.
        | Variable of id: TVariable
        /// Named abstraction. Needed for recursive types.
        | Named of name: Ident * env: (Ident * Type) list
        /// This is meant for overloaded types. Having this info later might be useful.
        | Tagged of uid: Uid * inner: Type
        /// Overloads of a function are put into this to prevent shadowing.
        /// `name` is only for error reporting.
        | Unspecified of overloads: Type list * name: string
        /// Type constructor that lists dependent variables, bounds, and holds originating environment.
        | Constructor of args: TVariable list * bounds: Bound list * env: (Ident * Type) list * body: Type
        /// Construct the type with the arguments.
        | Construct of args: (TVariable * Type) list * ctor: Type
        /// This is probably needed for having named abstractions that cannot be casted back to the structural type.
        | Opaque of id: Uid * inner: Type
        | Unsafe of inner: Type
        | Reference of inner: Type
        | Primitive of Primitive
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

    let rec print (this: Type) = 
        match this with
        | Primitive Str -> "Str"
        | Primitive Int -> "Int"
        | Primitive Real -> "Real"
        | Primitive Char -> "Char"
        | Primitive Bool -> "Bool"
        | Unknown x -> sprintf "<unknown`%d>" x.id
        | Opaque (id, t) -> sprintf "[%s: %s]" (Uid.print id) (print t)
        | Function (Function (leftl, leftr), right) -> sprintf "(%s -> %s) -> %s" (print leftl) (print leftr) (print right)
        | Function (left, right) -> sprintf "%s -> %s" (print left) (print right)
        | Tagged (uid, inner) -> sprintf "%s#%s" (print inner) (Uid.print uid)
        | Reference inner -> sprintf "%s&" (print inner)
        | Unsafe inner -> sprintf "%s!" (print inner)
        | Tuple xs -> 
            let inner = xs |> List.map print |> List.intercalate ", "
            sprintf "(%s)" inner
        | Unspecified (ts, name) ->
            let ts = ts |> List.map print |> List.intercalate "; "
            sprintf "[%s: %s]" name ts

    type Error =
        { message: string
          note: string
          range: Range
          /// Messages originating from the outside scope appear first.
          trace: string list }

    [<RequireQualifiedAccess>]
    module Error =
        let create message range =
            { Error.message = message
              note = ""
              range = range
              trace = [] }

        let createWithNote (message, note) range =
            { Error.message = message
              note = note
              range = range
              trace = []  }

        /// Prepends a trace message. Messages originating from the outside scope appear first.
        let addTraceMessage message e =
            { e with Error.trace = message :: e.trace }

        // TODO improve the language and consistency of the error messages.
        // TODO rename all of these

        let typeMismatch t1 t2 =
            sprintf "Couldn't match type `%s` with type `%s`." (print t1) (print t2)
            |> create

        let insufficientInformation =
            ("Couldn't infer the type based on provided information.",
             "The type of: each expression in a block, the value in a binding expression, and the value in an unsafe binding expression need to be fully inferrable.")
            |> createWithNote

        let expectedTypeMismatch actual expected =
            sprintf "Couldn't match type `%s` with the expected type `%s`." (print actual) (print expected)
            |> create

        let namedTypeNotFound name =
            sprintf "Couldn't find a definition for named type abstraction `%s` in the current scope." (Ident.print name)
            |> create

        let notATypeConstructor t =
            sprintf "Type `%s` is not a type constructor." (print t)
            |> create

        let insufficientTypeArgs arg ctor =
            sprintf "Couldn't construct type `%s` because one or more type arguments are missing, namely `%s`." (print ctor) (TVariable.print arg)
            |> create

        // Separation errors:
        let tupleElementCountMismatch outer inner =
            sprintf "Couldn't match tuple `%s` with tuple `%s` because the numbers of elements are not equal." (print outer) (print inner) |> create

        let opaqueTypeMismatch outer inner =
            sprintf "Couldn't match opaque type `%s` with opaque type `%s` because the IDs are not equal." (print outer) (print inner) |> create
        // --

        // Inference errors:
        let nameNotFound name =
            sprintf "Couldn't find a value named `%s` in the local or module scope." (Ident.print name) |> create

        let overloadNotFound (name, t) =
            sprintf "Couldn't find an overload of `%s` that matches type `%s`." name (print t) |> create

        // TODO remove below

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
            sprintf
                "Couldn't match record `%s` with record `%s` because the latter does not have a field named `%s`."
                (print record)
                (print other)
                field
            |> create

        let ``couldn't match unions`` variant union other =
            sprintf
                "Couldn't match the union `%s` with the union `%s` because the former does not have a variant of type `%s`"
                (print union)
                (print other)
                (print variant)
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


    /// runs `mapping` on all contained types (non separable) and carries state.
    /// This can have the effect of replacing all of one type with another; this
    /// is probably not desired since instantiation only entails replacing variables
    /// in a type constructor.
    let rec mapContained (mapping: 'state -> Type -> 'state * Type) (state: 'state) (t: Type) : 'state * Type =

        let rec substElements lmapping rmapping state ts =
            match ts with
            | t :: ts ->
                let state, newT = mapContained mapping state (lmapping t)
                let state, ts = substElements lmapping rmapping state ts
                state, (rmapping t newT) :: ts
            | [] -> state, []

        match t with
        | Function (left, right) ->
            let state, left = mapContained mapping state left
            let state, right = mapContained mapping state right
            state, Function(left, right)
        | Tuple elements ->
            let state, ts = substElements id (fun _ -> id) state elements
            state, Tuple ts
        | Record elements ->
            let state, ts =
                substElements (fun (_, t) -> t) (fun (n, _) t -> n, t) state elements

            state, Record ts
        | Union variants ->
            let state, ts = substElements id (fun _ -> id) state variants
            state, Union ts
        | Unspecified (ts, n) ->
            substElements id (fun _ t -> t) state ts
            |> fun (state, ts) -> state, Unspecified (ts ,n)
        | Constructor (_args, _bounds, _env, _body) ->
            failwith "Not Implemented"
        // TODO check for other cases where the type can contain other types
        | t -> mapping state t


    // TODO Update which types get traversed
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
        let fail e = Error(e this other)

        match this, other with
        | _, Unknown _ -> Ok res
        | Variable a, Variable b ->
            if a = b then
                Ok res
            else
                Error(Error.``couldn't match variable`` a b)
        | Opaque (_, this), Opaque (_, other) -> this |> tryMatch other // TODO (type-subst) compare ids
        | Primitive Str, Primitive Str -> Ok res
        | Primitive Int, Primitive Int -> Ok res
        | Primitive Real, Primitive Real -> Ok res
        | Primitive Char, Primitive Char -> Ok res
        | Primitive Bool, Primitive Bool -> Ok res
        // TODO Test for these cases to determine if they are actually unreachable
        | Unspecified _, Unspecified _ -> failStr "tryMatch Overloaded Overloaded is unreachable"
        | Constructor _, Constructor _ -> failStr "tryMatch Constructor Constructor is unreachable"
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
            else
                fail Error.``couldn't match tuples``
        | Record this, Record other ->
            if this.Length = other.Length then
                this
                |> List.map (fun (thisName, thisType) ->
                    other
                    |> List.map (fun (otherName, otherType) ->
                        if thisName = otherName then
                            thisType |> tryMatch otherType
                        else
                            failStr "")
                    |> List.filter Result.isOk
                    |> Result.collect
                    |> Result.bind (fun results ->
                        results
                        |> List.concat
                        |> List.tryHead
                        |> Result.fromOption (
                            Error.``couldn't match records because field`` thisName (Record this) (Record other)
                        )))
                |> Result.collect
            else
                fail Error.``couldn't match records``
        | Union this, Union other -> // this :> other
            other
            |> List.map (fun otherType ->
                this
                |> List.map (fun thisType -> thisType |> tryMatch otherType)
                |> List.filter Result.isOk
                |> Result.collect
                |> Result.bind (fun results ->
                    results
                    |> List.concat
                    |> List.tryHead
                    |> Result.fromOption (Error.``couldn't match unions`` otherType (Union this) (Union other))))
            |> Result.collect
        | _ -> Error(Error.typeMismatch this other)

    let rec isMoreGeneralThan (inner: Type) (outer: Type) : bool =
        match outer, inner with
        | Primitive Int, Primitive Int -> true
        | Primitive Str, Primitive Str -> true
        | Primitive Real, Primitive Real -> true
        | Primitive Char, Primitive Char -> true
        | Primitive Bool, Primitive Bool -> true
        | Unknown(id1), Unknown(id2) -> TUnknown.equals id1 id2
        | Variable(id1),Variable(id2) -> TVariable.equals id1 id2
        | Named(name1, _),Named(name2, _) -> Ident.equals name1 name2
        | Tagged(_, inner1), Tagged(_, inner2) -> isMoreGeneralThan inner1 inner2 // uid can be different
        | Unspecified(overloads1, _), Unspecified(overloads2, _) -> List.unorderedCmp isMoreGeneralThan overloads1 overloads2
        | Constructor(_, _, _, _), Constructor(_, _, _, _) -> failwith "unreachable"
        | Construct(_, _), Construct(_, _) -> failwith "unreachable"
        | Opaque(id1, inner1), Opaque(id2, inner2) -> Uid.equals id1 id2 
        | Unsafe(inner1), Unsafe(inner2) -> isMoreGeneralThan inner1 inner2
        | Reference(inner1), Reference(inner2) -> isMoreGeneralThan inner1 inner2
        | Function(left1, right1), Function(left2, right2) -> isMoreGeneralThan left1 left2 && isMoreGeneralThan right1 right2
        | Tuple(elements1), Tuple(elements2) -> elements1.Length = elements2.Length && List.zip elements1 elements2 |> List.map (fun (a, b) -> isMoreGeneralThan a b) |> List.forall id
        | Record(elements1), Record(elements2) -> failwith "Not Implemented"
        | Union(elements1), Union(elements2) -> failwith "Not Implemented"
        | _ -> false


    let subst var newType t =
        t 
        |> mapContained
            (fun state t1 ->
                match t1 with
                | Type.Variable v1 -> 
                    if TVariable.equals var v1 then
                        state, newType
                    else
                        state, t1
                | _ -> state, t1)
            ()
        |> fun (_, t) -> t

    let substUnknown x newType t =
        t
        |> mapContained
            (fun state t1 ->
                match t1 with
                | Type.Unknown x1 -> 
                    if TUnknown.equals x x1 then
                        state, newType
                    else
                        state, t1
                | _ -> state, t1)
            ()
        |> fun (_, t) -> t

    let gen t env =
        t
        |> mapContained
            (fun state t1 ->
                match t1 with
                | Unknown var -> 
                    if state |> List.contains var then
                        state, t1
                    else var :: state, t1
                | _ -> state, t1)
            []
        |> fun (state, t) ->
            let substitutions =
                state
                |> List.indexed
                |> List.map (fun (index, x) -> x, TVariable.create index)
            let newT =
                substitutions
                |> List.map (fun (x, var) -> x, Variable var)
                |> List.fold (fun acc (x, t) -> acc |> substUnknown x t) t
            let args = substitutions |> List.map (fun (_x, var) -> var)
            Constructor(args, [], env, newT)

    let contains pred t =
        t 
        |> mapContained 
            (fun state t ->
                match state, t with
                | true, t -> true, t
                | _, t -> pred t, t)
            false
        |> fun (x, _t) -> x

type Type = Type.Type

[<RequireQualifiedAccess>]
module Pattern =
    type Pattern = Ident of name: string
