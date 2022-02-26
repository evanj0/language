module Inference

open Ir
open UntypedIr

open Identifier
open ResultExtensions
open Range

type Error =
    {
        message: string
        range: Range
    }

[<RequireQualifiedAccess>]
module Error =
    let create message range = { Error.message = message; range = range }

    let ``value not found`` ident = 
        sprintf "The value `%s` is not defined in the local or module scope." (Ident.print ident) |> create

    let ``overloaded value not found`` ident = 
        sprintf "The value `%s` is not defined in the local scope, or in module scope as a single or overloaded value." (Ident.print ident) |> create

    let ``tuples not same length`` t1 t2 = 
        sprintf "The tuples of type `%s` and `%s` do not have the same number of elements." (Type.print t1) (Type.print t2) |> create

    let ``cannot constrain type variable`` var t =
        sprintf "Cannot match type variable `%s` with type `%s`. Constraining type variables is not allowed, as this causes their type to become more specific than intended." (Type.print var) (Type.print t) |> create

[<RequireQualifiedAccess>]
module Constraint =
    type Constraint =
        { 
            left: Type
            right: Type
            range: Range 
        }

    let copyRange left right this = { this with left = left; right = right}

type Constraint = Constraint.Constraint

type Constraints = Constraint list

type Env =
    {
        globals: (Ident * Uid * Type) list
        locals: (Ident * Type) list
        opaqueTypes: (Ident * Type) list
        currentRange: Range
        unknownIndex: int
    }

[<RequireQualifiedAccess>]
module Env =
    let setCurrentRange range this =
        { this with Env.currentRange = range }

    let currentRange this = this.currentRange

    let extend name t this =
        { this with Env.locals = (Ident.fromList [name], t) :: this.locals }

    let getOverloads ident this =
        let local =
            this.locals
            |> List.filter (fun (x, _) -> x |> Ident.contains ident)
            |> List.map (fun (_, x) -> x)
            |> List.tryHead
            |> Result.fromOption (Error.``overloaded value not found`` ident (this |> currentRange))
        if local |> Result.isOk then
            local
        else
            let globals =
                this.globals
                |> List.filter (fun (x, _, _) -> x |> Ident.contains ident)
                |> List.map (fun (_, id, x) -> id, x)
            if globals.Length >= 1 then
                Ok (Type.Contained (Type.Overloaded globals))
            else local

type State =
    {
        index: int
    }

module State = 
    let createUnknown this = { this with State.index = this.index + 1 }, this.index |> (fun x -> Type.Contained (Type.Unknown x))

type IResult<'a> =
    | IOk of state: State * t: 'a * c: Constraints
    | IErr of Error

[<RequireQualifiedAccess>]
module IResult =
    let fromResult state c result =
        match result with
        | Ok t -> IOk (state, t, c)
        | Error e -> IErr e

    let bind (f: State -> _ -> _ IResult) (result: _ IResult): _ IResult =
        match result with
        | IOk (state, t, c) ->
            match f state t with
            | IOk (state, t, c1) -> IOk (state, t, c @ c1)
            | IErr e -> IErr e
        | IErr e -> IErr e

    let map (f: State -> _ -> State * _ * Constraints) (result: _ IResult): _ IResult =
        match result with
        | IOk (state, t, c) ->
            match f state t with
            | (state, t, c1) -> IOk (state, t, c @ c1)
        | IErr e -> IErr e

    let mapType mapping result = 
        result
        |> map (fun state t -> state, mapping t, [])

    let ret state t = IOk (state, t, [])

[<RequireQualifiedAccess>]
module private I =
    let getType ident state this =
        this 
        |> Env.getOverloads ident
        |> IResult.fromResult state []
        
    let noConstraints _ _ = []
    
    let createUnknown state =
        let state, t = state |> State.createUnknown
        IResult.ret state t
        
    [<RequireQualifiedAccess>]
    module Constrain =
        let equals left right this: Constraints =
            [{Constraint.left = left; right = right; range = this |> Env.currentRange}]

[<RequireQualifiedAccess>]
module Inference =
    let rec infer (expr: Expr) (state: State) (constraintFn: Type -> Env -> Constraints) (env: Env): Type IResult =
        match expr with
        | Expr.Ident ident -> 
            env
            |> I.getType ident state
            // TODO Instantiate types
        | Expr.Literal (Literal.Str _) -> IResult.ret state (Type.Contained (Type.Primitive Type.Str))

        | Expr.Cond (guard, th, el) ->
            env
            |> infer guard state (I.Constrain.equals (Type.Contained (Type.Primitive Type.Bool)))
            |> IResult.bind (fun state _ ->
                env
                |> infer th state I.noConstraints
                |> IResult.bind (fun state thenType ->
                    env
                    |> infer el state (I.Constrain.equals thenType)
                    |> IResult.mapType (fun _ -> thenType)))

        | Expr.Tuple xs ->
            let rec inferXs state xs: (Type list) IResult =
                match xs with
                | [] -> IResult.ret state []
                | x::xs -> 
                    env
                    |> infer x state I.noConstraints
                    |> IResult.bind (fun state t ->
                        inferXs state xs
                        |> IResult.mapType (fun ts -> t :: ts))
            inferXs state xs
            |> IResult.mapType (fun xs -> Type.Separable (Type.Tuple xs))

        | Expr.Func (p, expr) ->
            state
            |> I.createUnknown
            |> IResult.bind (fun state paramType ->
                state
                |> I.createUnknown
                |> IResult.bind (fun state returnType ->
                    env
                    |> Env.extend p paramType
                    |> infer expr state (I.Constrain.equals returnType)
                    |> IResult.mapType (fun _ -> returnType)))

        | Expr.Type (expr, t) ->
            env
            |> infer expr state (I.Constrain.equals t)
            // TODO Needs to extend environment with universally quantified vars

        | Expr.Tagged (expr, range) ->
            env
            |> Env.setCurrentRange range
            |> infer expr state constraintFn
        // TODO Implement
        | Expr.Literal(value) -> failwith "Not Implemented"
        | Expr.Record(elements) -> failwith "Not Implemented"
        | Expr.Block(expr, next) -> failwith "Not Implemented"
        | Expr.Extern(name, argument) -> failwith "Not Implemented"
        | Expr.NoRet -> failwith "Not Implemented"
        | Expr.Update(expr, fields) -> failwith "Not Implemented"
        | Expr.App(f, x) -> failwith "Not Implemented"
        | Expr.Let(name, expr, body) -> failwith "Not Implemented"
        | Expr.Ref(expr) -> failwith "Not Implemented"
        | Expr.Mut(expr, value) -> failwith "Not Implemented"
        | Expr.Match(expr, case) -> failwith "Not Implemented"

        |> IResult.map (fun state t -> state, t, constraintFn t env)

module Solver =
    type TypeExpression =
        | Type of t: Type
        | OverloadFilterMap of overloads: (Uid * TypeExpression list) list * filter: TypeExpression list * map: int

    type TypeStatement = 
        | UnknownEquals of unknown: Type.Unknown * value: TypeExpression

    let rec solveTypeExpression expr =
        match expr with
        | OverloadFilterMap (overloads, filter, map) ->
            let typeMatchesFilter (filter: TypeExpression list) (t: TypeExpression list) =
                if filter.Length = t.Length then
                    List.zip filter t
                    |> List.map (fun (filter, t) ->
                        ) // TODO t equals filter or filter is unknown

            overloads
            |> List.filter (fun (_, overload) ->
                )

    let rec createTypeStatements (c: Constraint): Result<TypeStatement list, Error> =
        let fail e = Error (e c.left c.right c.range)
        match c.left, c.right with
        | Type.Separable l, Type.Separable r -> 
            match l, r with
            | Type.Function (l1, l2), Type.Function (r1, r2) ->
                [
                    createTypeStatements (c |> Constraint.copyRange l1 r1);
                    createTypeStatements (c |> Constraint.copyRange l2 r2);
                ]
                |> Result.collect
                |> Result.map (fun xs -> List.concat xs)
            | Type.Tuple l, Type.Tuple r ->
                if l.Length = r.Length then
                    List.zip l r
                    |> List.map (fun (l, r) -> createTypeStatements (c |> Constraint.copyRange l r))
                    |> Result.collect
                    |> Result.map (fun xs -> List.concat xs)
                else fail Error.``tuples not same length``
        | Type.Contained (Type.Unknown id), a -> 
            Ok [UnknownEquals (id, Type a)]
        | Type.Contained (Type.Variable _), _ -> 
            fail Error.``cannot constrain type variable``
