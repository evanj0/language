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
    { globals: (Ident * Type) list
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
            |> Result.fromOption (Type.Error.``overloaded value not found`` ident (this |> currentRange)) // TODO fix this error handling

        if local |> Result.isOk then
            local
        else
            this.globals
            |> List.filter (fun (i, _t) -> i |> Ident.contains ident)
            |> List.map (fun (_i, t) -> t)

            |> fun list ->
                if list.Length = 1 then
                    list |> List.head |> Ok
                else if list.Length = 0 then 
                    Error (Type.Error.``overloaded value not found`` ident (this |> currentRange))
                else
                    Ok (Type.Overloaded list)

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
        | OverloadFilterMap of overloads: Type list * filter: TypeExpression * map: FunctionMapping
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

    /// Simplifies constraints by separating compound types (functions, tuples, records). 
    /// Does not compare the inner types; errors are produced only if there are conflicts in major types 
    /// (i.e., `(a, b) :> (a, b, c)` or `&a :> a`).
    let rec createTypeStatements (c: Constraint) : Result<TypeStatement list, Type.Error> =
        let fail e = Error(e c.outer c.inner c.range)
        let fail2 e x1 x2 = Error(e x1 x2 c.range)

        let createNext outer inner = createTypeStatements (c |> Constraint.copyRange outer inner)

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
        
        // a! :> b!
        | Type.Unsafe outer, Type.Unsafe inner -> createNext outer inner

        // a& :> b&
        | Type.Reference outer, Type.Reference inner -> createNext outer inner
        
        | Type.Constructor (args, bounds, env, body), x ->
            body
            // Compare `x` to `body` to get a list of variables and their pairings.
            |> Type.tryMatch
                (fun (bodyT, t) ->
                    match bodyT, t with
                    | Type.Variable var, t -> [ var, t ]
                    | _ -> [])
                x
            |> Result.mapError (fun e -> e c.range)
            |> Result.bind (fun foundArgs ->
                // Find args specified in constructor.
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
                        // Generate the correct type of statement.
                        match t with
                        | Type.Unknown x -> UnknownEquals(x, expr)
                        | t -> TypeEquals(t, expr)))) // TODO consider splitting this into two lists, one has unknown equality, one has type equality to check
        // TODO add other cases
        | t, t2 -> Ok ([ TypeEquals(t, Type t2) ])

    // TODO not sure if this works
    and private createFunctionMappings' nextMapping x =
        match x with
        | Type.Function (left, right) ->
            createFunctionMappings' (fun next -> KeepLeft next) left
            @ createFunctionMappings' (fun next -> KeepRight next) right
        | t -> [ t, nextMapping KeepValue ]

    /// Creates a list of types found in x and the corresponding positions in the function.
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
                    |> List.filter (fun (t) -> typeMatchesFilter t filter)
                    |> List.tryHead
                    |> Result.fromOption (Type.Error.``cannot resolve overloaded type``)
                    |> Result.map (fun (t) -> applyFunctionMapping mapping t) // TODO what to do with uid?
            }

        | ConstructorApp (args, bounds, env, inner, map) -> failwith ""
        | Type (t) -> solve statements [] t // Passthrough

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
                        | Ok t -> 
                            let errors = verify statements
                            if errors |> List.length = 1 then
                                (errors |> List.head) :: state, t // TODO figure out a better way to do this than an if statement
                            else state, t
                        | Error e -> e :: state, tOriginal
                | t -> state, t

        t
        |> Type.mapContained mapping ([])
        |> fun (errors, t) ->
            if errors.Length = 0 then
                Ok t
            else
                errors |> List.head |> Error

    and verifyStatement statements expected expr : Result<unit, Range -> Type.Error> =
        result {
            let! actual = solveTypeExpression statements expr
            if expected |> Type.equals actual then
                return ()
            else
                return! Error(Type.Error.expectedTypeMismatch actual expected)
        }

    and verify statements =
        statements
        |> List.map (fun (_index, stmt) ->
            match stmt with
            | TypeEquals (expected, expr) ->
                verifyStatement statements expected expr
            | _ -> Ok ())
        |> List.filterMap Result.getErrorValue

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
        
        // str
        | Expr.Literal (Literal.Str _) -> IResult.ret state (Type.Primitive Type.Str)
        
        // int
        | Expr.Literal (Literal.Int _) -> IResult.ret state (Type.Primitive Type.Int)

        // type(th); bool :> type(guard); type(th) :> type(el)
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

        // (type(x1), type(x2), ..., type(xn))
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

        // new(1) -> type(expr, extend(env, p, new(1)))
        | Expr.Func (p, expr) ->
            state
            |> I.createUnknown
            |> IResult.bind (fun state pType ->
                env
                |> Env.extend p pType
                |> inferUnsolvedType expr state I.noConstraints
                |> IResult.mapType (fun rType -> Type.Function(pType, rType)))

        // type(expr); t :> type(expr)
        | Expr.Type (expr, t) ->
            env
            // TODO Needs to extend environment with universally quantified vars
            |> inferUnsolvedType expr state (I.Constrain.refineTo t) // t :> type(expr); type(expr)
        
        // type(expr)
        | Expr.Tagged (expr, range) ->
            env
            |> Env.setCurrentRange range
            |> inferUnsolvedType expr state I.noConstraints // TODO determine if constraining function needs to be passed
        
        // type(expr)&
        | Expr.Ref expr ->
            env
            |> inferUnsolvedType expr state I.noConstraints
            |> IResult.mapType (fun t -> Type.Reference t) 
        
        // new(1); type(expr) :> new(1)&
        | Expr.Deref expr ->
            state 
            |> I.createUnknown
            |> IResult.bind (fun state innerType ->
                inferUnsolvedType expr state (I.Constrain.refineFrom (Type.Reference innerType)) env
                |> IResult.mapType (fun _ -> innerType)) 
        
        // new(1); type(f) :> type(x) -> new(1)
        | Expr.App (f, x) ->
            state
            |> I.createUnknown
            |> IResult.bind (fun state rType ->
                env
                |> inferUnsolvedType x state I.noConstraints
                |> IResult.bind (fun state xType ->
                    let iType = Type.Function(xType, rType)
                    env
                    |> inferUnsolvedType f state (I.Constrain.refineFrom iType)
                    |> IResult.mapType (fun _ -> rType)))

        | Expr.Let (name, expr, body) -> failwith "Not Implemented" // this finishes type inference

        | Expr.UnsafeLet (name, expr, body) ->
            state
            |> I.createUnknown
            |> IResult.bind (fun state eType ->
                inferUnsolvedType expr state (I.Constrain.refineFrom (Type.Unsafe eType)) env
                ) |> ignore // TODO this needs to finish inference
            failwith "Not Implemented"

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
