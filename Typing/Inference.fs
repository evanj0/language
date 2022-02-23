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

[<RequireQualifiedAccess>]
module Constraint =
    type Constraint =
        | Equals of left: Type.Type * right: Type.Type * range: Range

type Env =
    {
        globals: (Ident * Type.Type) list
        locals: (Ident * Type.Type) list
        opaqueTypes: (Ident * Type.Type) list
        currentRange: Range
        unknownIndex: int
    }

type InferenceResult = Result<Env * Type.Type * Constraint.Constraint list, Error>

[<RequireQualifiedAccess>]
module Env =
    let setCurrentRange range this =
        { this with Env.currentRange = range }

    let currentRange this = this.currentRange

    let extend name t this =
        { this with Env.locals = (Ident.fromList [name], t) :: this.locals }

    let names this =
        this.locals @ this.globals

    let getType ident this =
        this
        |> names
        |> List.filter(fun (x, _) -> x |> Ident.contains ident)
        |> List.tryHead
        |> Result.fromOption (Error.``value not found`` ident (this |> currentRange))

    
    
    let bind (f: Type.Type -> Env -> InferenceResult) (result: InferenceResult): InferenceResult =
        match result with
        | Ok (env, t, c) ->
            env
            |> f t
            |> Result.map (fun (env, t, c1) -> env, t, c @ c1)
        | Error e -> Error e

    let map (f: Type.Type -> Env -> Env * Type.Type * Constraint.Constraint list) (result: InferenceResult): InferenceResult =
        result
        |> Result.map (fun (env, t, c) ->
            env
            |> f t)

    let generateUnknown this =
        { this with Env.unknownIndex = this.unknownIndex + 1 }, Type.Unknown this.unknownIndex, []


    [<RequireQualifiedAccess>]
    module Constrain =
        let equals left right this =
            this, right, [Constraint.Equals (left, right, this |> currentRange)]

[<RequireQualifiedAccess>]
module Infer =

    let rec infer (expr: Expr.Expr) (env: Env): InferenceResult =
        match expr with
        | Expr.Tagged (expr, range) ->
            env
            |> Env.setCurrentRange range
            |> infer expr

        | Expr.Cond (guard, th, el) ->
            env
            |> infer guard
            |> Env.map (Env.Constrain.equals (Type.Primitive Type.Bool))
            |> Env.bind (fun _ env ->
                env
                |> infer th
                |> Env.bind (fun thenType env ->
                    env
                    |> infer el
                    |> Env.map (Env.Constrain.equals thenType)))

        | Expr.Literal (Literal.Str _) -> Ok (env, Type.Primitive Type.Str, [])