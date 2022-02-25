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

    let ``value not found`` ident = sprintf "The value `%s` is not defined in the local or module scope." (Ident.print ident) |> create

    let ``overloaded value not found`` ident = sprintf "The value `%s` is not defined in the local scope, or in module scope as a single or overloaded value." (Ident.print ident) |> create

[<RequireQualifiedAccess>]
module Constraint =
    type Constraint =
        | Equals of left: Type * right: Type * range: Range
        | Bound of name: string * t: Type

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
                Ok (Type.Overloaded globals)
            else local

type State =
    {
        index: int
    }

module State = 
    let createUnknown this = { this with State.index = this.index + 1 }, this.index |> Type.Unknown

type IResult<'a> =
    | IOk of state: State * t: 'a * c: Constraints
    | IErr of Error

[<RequireQualifiedAccess>]
module IResult =
    let fromResult state c result =
        match result with
        | Result.Ok t -> IOk (state, t, c)
        | Result.Error e -> IErr e

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
            [Constraint.Equals (left, right, this |> Env.currentRange)]

[<RequireQualifiedAccess>]
module Inference =
    let rec infer (expr: Expr) (state: State) (constraintFn: Type -> Env -> Constraints) (env: Env): Type IResult =
        match expr with
        | Expr.Ident ident -> 
            env
            |> I.getType ident state
            // |> I.inst
        | Expr.Literal (Literal.Str _) -> IResult.ret state (Type.Primitive Type.Str)

        | Expr.Cond (guard, th, el) ->
            env
            |> infer guard state (I.Constrain.equals (Type.Primitive Type.Bool))
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
            |> IResult.mapType Type.Tuple

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

        | Expr.Tagged (expr, range) ->
            env
            |> Env.setCurrentRange range
            |> infer expr state constraintFn

        |> IResult.map (fun state t -> state, t, constraintFn t env)

    let refine (t: Type) (other: Type): Result<Type, Error> =
        match t, other with
        | Type.Unknown _, other -> Ok other
        | t, Type.Opaque (name, inner) -> // determine if t matches inner