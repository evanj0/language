module Inference

open Ir
open UntypedIr

open Identifier
open ResultExtensions
open ListExtensions
open Range


[<RequireQualifiedAccess>]
module Constraint =
    type Constraint =
        { outer: Type
          inner: Type
          range: Range }

    let copyRange outer inner this =
        { this with
            outer = outer
            inner = inner }

type Constraint = Constraint.Constraint

type Constraints = Constraint list

type Env =
    { globals: (Ident * Uid * Type) list
      locals: (Ident * Type) list
      currentRange: Range }

[<RequireQualifiedAccess>]
module Env =
    let setCurrentRange range this = { this with Env.currentRange = range }

    let currentRange this = this.currentRange

    let extend name t this =
        { this with Env.locals = (Ident.fromList [ name ], t) :: this.locals }

    let getOverloads ident this =
        let local =
            this.locals
            |> List.filter (fun (x, _) -> x |> Ident.contains ident)
            |> List.map (fun (_, x) -> x)
            |> List.tryHead
            |> Result.fromOption (Type.Error.``overloaded value not found`` ident (this |> currentRange))

        if local |> Result.isOk then
            local
        else
            let globals =
                this.globals
                |> List.filter (fun (x, _, _) -> x |> Ident.contains ident)
                |> List.map (fun (_, id, x) -> id, x)

            if globals.Length >= 1 then
                Ok(Type.Overloaded globals)
            else
                local

type State = { index: int }

module State =
    let createUnknown this =
        { this with State.index = this.index + 1 },
        this.index
        |> (fun x -> Type.Unknown { Type.TUnknown.id = x })

type IResult<'a> =
    | IOk of state: State * t: 'a * c: Constraints
    | IErr of Type.Error

[<RequireQualifiedAccess>]
module IResult =
    let fromResult state c result =
        match result with
        | Ok t -> IOk(state, t, c)
        | Error e -> IErr e

    let toResult iresult =
        match iresult with
        | IOk (state, t, c) -> Ok(state, t, c)
        | IErr e -> Error e

    let bind (f: State -> _ -> _ IResult) (result: _ IResult) : _ IResult =
        match result with
        | IOk (state, t, c) ->
            match f state t with
            | IOk (state, t, c1) -> IOk(state, t, c @ c1)
            | IErr e -> IErr e
        | IErr e -> IErr e

    let map (f: State -> _ -> State * _ * Constraints) (result: _ IResult) : _ IResult =
        match result with
        | IOk (state, t, c) ->
            match f state t with
            | (state, t, c1) -> IOk(state, t, c @ c1)
        | IErr e -> IErr e

    let mapType mapping result =
        result
        |> map (fun state t -> state, mapping t, [])

    let ret state t = IOk(state, t, [])

[<RequireQualifiedAccess>]
module private I =
    let getType ident state env =
        env
        |> Env.getOverloads ident
        |> IResult.fromResult state []

    let noConstraints _ _ = []

    let createUnknown state =
        let state, t = state |> State.createUnknown
        IResult.ret state t

    [<RequireQualifiedAccess>]
    module Constrain =
        /// Refines `inner` with `outer`. `inner` is more general than `outer`.
        /// For example, `refineTo (Unsafe _) x` and `refineTo x (Unknown _)` will typecheck.
        /// `refineTo (Function _ _) (Unknown _)` will not.
        let refineTo outer inner env : Constraints =
            [ { Constraint.outer = outer
                inner = inner
                range = env |> Env.currentRange } ]

        /// Same as `refineTo`, but with arguments reversed.
        let refineFrom inner outer this = refineTo outer inner this

module Solver =
    type TypeExpression =
        | Type of t: Type
        | OverloadFilterMap of overloads: (Uid * Type) list * filter: TypeExpression * map: FunctionMapping
        /// `env` needs to be set since the default value when pulling this from the AST is `[]`.
        | ConstructorApp of
            args: (Type.TVariable * Type) list *
            bounds: Type.Bound list *
            env: (Ident * Type) list *
            inner: Type *
            map: FunctionMapping

    and TypeStatement =
        | UnknownEquals of unknown: Type.TUnknown * value: TypeExpression
        | TypeEquals of t: Type * value: TypeExpression

    and FunctionMapping =
        | KeepLeft of inner: FunctionMapping
        | KeepRight of inner: FunctionMapping
        | KeepValue

    let rec createTypeStatements (c: Constraint) : Result<TypeStatement list, Type.Error> =
        let fail e = Error(e c.outer c.inner c.range)
        let fail2 e x1 x2 = Error(e x1 x2 c.range)

        match c.outer, c.inner with
        | Type.Function (outerL, outerR), Type.Function (innerL, innerR) ->
            [ createTypeStatements (c |> Constraint.copyRange outerL innerL)
              createTypeStatements (c |> Constraint.copyRange outerR innerR) ]
            |> Result.collect
            |> Result.map (fun xs -> List.concat xs)
        | Type.Tuple outer, Type.Tuple inner ->
            if outer.Length = inner.Length then
                List.zip outer inner
                |> List.map (fun (outer, inner) -> createTypeStatements (c |> Constraint.copyRange outer inner))
                |> Result.collect
                |> Result.map (fun xs -> List.concat xs)
            else
                fail Type.Error.``couldn't match tuples``
        | outer, Type.Unknown id -> Ok [ UnknownEquals(id, Type outer) ]
        | Type.Variable var1, Type.Variable var2 ->
            if Type.TVariable.equals var1 var2 then
                Ok []
            else
                fail2 Type.Error.``couldn't match the type variable with`` var1 var2
        | Type.Constructor (args, bounds, env, body), x ->
            body
            |> Type.tryMatch
                (fun (bodyT, t) ->
                    match bodyT, t with
                    | Type.Variable var, t -> [ var, t ]
                    | _ -> [])
                x
            |> Result.mapError (fun e -> e c.range)
            |> Result.bind (fun foundArgs ->
                args
                |> List.map (fun t ->
                    foundArgs
                    |> List.filter (fun (t1, _) -> Type.TVariable.equals t t1)
                    |> List.tryHead
                    |> Result.fromOption t)
                |> Result.collect
                |> Result.mapError (fun t -> Type.Error.``insufficient type arguments`` t c.range)
                |> Result.map (fun args ->
                    createFunctionMappings x
                    |> List.map (fun (t, map) ->
                        let expr = ConstructorApp(args, bounds, env, body, map)

                        match t with
                        | Type.Unknown x -> UnknownEquals(x, expr)
                        | t -> TypeEquals(t, expr)))) // TODO consider splitting this into two lists, one has unknown equality, one has type equality to check

    // TODO not sure if this works
    and private createFunctionMappings' nextMapping x =
        match x with
        | Type.Function (left, right) ->
            createFunctionMappings' (fun next -> KeepLeft next) left
            @ createFunctionMappings' (fun next -> KeepRight next) right
        | t -> [ t, nextMapping KeepValue ]

    and private createFunctionMappings x : (Type * FunctionMapping) list = createFunctionMappings' id x

    let rec private applyFunctionMapping mapping t =
        match t, mapping with
        | Type.Function (left, _), KeepLeft inner -> applyFunctionMapping inner left
        | Type.Function (_, right), KeepRight inner -> applyFunctionMapping inner right
        | t, KeepValue -> t
        | _ -> failwith "Unreachable" // TODO test if this is actually unreachable

    let rec solveTypeExpression (statements: (int * TypeStatement) list) expr : Result<Type, Range -> Type.Error> = // TODO where does range get added to Type.Error messages?
        let solveTypeExpression = solveTypeExpression statements

        match expr with
        | OverloadFilterMap (overloads, filter, mapping) ->
            let typeMatchesFilter filter t =
                t
                |> Type.tryMatch (fun _ -> []) filter
                |> Result.isOk

            result {
                let! filter = solveTypeExpression filter

                return!
                    overloads
                    |> List.filter (fun (_, t) -> typeMatchesFilter t filter)
                    |> List.tryHead
                    |> Result.fromOption (Type.Error.``cannot resolve overloaded type``)
                    |> Result.map (fun (_uid, t) -> applyFunctionMapping mapping t) // TODO what to do with uid?
            }

        | ConstructorApp (args, bounds, env, inner, map) -> failwith ""

    and solve (statements: (int * TypeStatement) list) (visited: int list) (t: Type) : Result<Type, Range -> Type.Error> =
        let mapping = // returns any errors that happen in a list
            fun state tOriginal ->
                match tOriginal with
                | Type.Unknown x ->
                    statements
                    |> List.filterMap (fun (index, stmt) ->
                        match stmt with
                        | UnknownEquals (x1, expr) ->
                            if visited |> List.contains index |> not
                               && Type.TUnknown.equals x x1 then
                                Some expr
                            else
                                None
                        | _ -> None)
                    |> List.tryHead
                    |> Result.fromOption (Type.Error.insufficientInformation)
                    |> Result.bind (fun expr -> solveTypeExpression statements expr)
                    |> fun result ->
                        match result with
                        | Ok t -> state, t
                        | Error e -> e :: state, tOriginal
                | t -> state, t

        t
        |> Type.mapContained mapping ([])
        |> fun (errors, t) ->
            if errors.Length = 0 then
                Ok t
            else
                errors |> List.head |> Error

[<RequireQualifiedAccess>]
module Inference =
    /// Gets the unsolved type of an expressions, along with the constraints needed to solve the type.
    let rec inferUnsolvedType (expr: Expr) (state: State) (constraintFn: Type -> Env -> Constraints) (env: Env) : Type IResult =

        match expr with
        | Expr.Ident ident ->
            env
            // TODO this needs to instantiate types
            // TODO also decide if env will be set in constructors here or somewhere else
            |> I.getType ident state
        | Expr.Literal (Literal.Str _) -> IResult.ret state (Type.Primitive Type.Str)

        | Expr.Cond (guard, th, el) ->
            env
            |> inferUnsolvedType guard state (I.Constrain.refineTo (Type.Primitive Type.Bool))
            |> IResult.bind (fun state _ ->
                env
                |> inferUnsolvedType th state I.noConstraints
                |> IResult.bind (fun state thenType ->
                    env
                    |> inferUnsolvedType el state (I.Constrain.refineTo thenType) // thenType :> type(else)
                    |> IResult.mapType (fun _ -> thenType)))

        | Expr.Tuple xs ->
            let rec inferXs state xs : (Type list) IResult =
                match xs with
                | [] -> IResult.ret state []
                | x :: xs ->
                    env
                    |> inferUnsolvedType x state I.noConstraints
                    |> IResult.bind (fun state t ->
                        inferXs state xs
                        |> IResult.mapType (fun ts -> t :: ts))

            inferXs state xs
            |> IResult.mapType (fun xs -> Type.Tuple xs)

        | Expr.Func (p, expr) ->
            state
            |> I.createUnknown
            |> IResult.bind (fun state pType ->
                env
                |> Env.extend p pType
                |> inferUnsolvedType expr state I.noConstraints
                |> IResult.mapType (fun rType -> Type.Function(pType, rType)))

        | Expr.Type (expr, t) ->
            env
            // TODO Needs to extend environment with universally quantified vars
            |> inferUnsolvedType expr state (I.Constrain.refineTo t) // t :> type(expr)

        | Expr.Tagged (expr, range) ->
            env
            |> Env.setCurrentRange range
            |> inferUnsolvedType expr state constraintFn
        | Expr.Ref (expr) ->
            env
            |> inferUnsolvedType expr state I.noConstraints
            |> IResult.mapType (fun t -> Type.Reference t)

        | Expr.App (f, x) ->
            state
            |> I.createUnknown
            |> IResult.bind (fun state rType ->
                env
                |> inferUnsolvedType x state I.noConstraints
                |> IResult.bind (fun state xType ->
                    let iType = Type.Function(xType, rType)

                    env
                    |> inferUnsolvedType f state (I.Constrain.refineFrom iType) // type(f) :> iType
                    |> IResult.mapType (fun _ -> rType)))

        | Expr.Let (name, expr, body) -> failwith "Not Implemented" // this finishes type inference

        | Expr.UnsafeLet (name, expr, body) -> failwith ""


        // TODO Implement
        | Expr.Literal (value) -> failwith "Not Implemented"
        | Expr.Record (elements) -> failwith "Not Implemented"
        | Expr.Block (expr, next) -> failwith "Not Implemented" // this finishes type inference for each line
        | Expr.Extern (name, argument) -> failwith "Not Implemented"
        | Expr.NoRet -> failwith "Not Implemented"
        | Expr.Update (expr, fields) -> failwith "Not Implemented"
        | Expr.Mut (expr, value) -> failwith "Not Implemented"
        | Expr.Match (expr, case) -> failwith "Not Implemented"

        |> IResult.map (fun state t -> state, t, constraintFn t env)

    and infer (expr: Expr) (state: State) (constrainer: Type -> Env -> Constraints) (env: Env) : Type IResult =
        inferUnsolvedType expr state constrainer env
        |> IResult.toResult
        |> Result.bind (fun (state, t, cs) ->
            result {
                let! statements =
                    cs
                    |> List.map (fun c -> Solver.createTypeStatements c)
                    |> Result.collect
                    |> Result.map (fun rs -> rs |> List.concat |> List.indexed)

                let! solvedType =
                    Solver.solve statements [] t
                    |> Result.mapError (fun e -> e env.currentRange) // TODO is this where range comes from?

                return solvedType, state
            })
        |> fun res ->
            match res with
            | Ok (t, state) -> IOk(state, t, []) // TODO does this return no constraints or does cs also need to get passed
            | Error e -> IErr e
