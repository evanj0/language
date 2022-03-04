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
                    Error(Type.Error.``overloaded value not found`` ident (this |> currentRange))
                else
                    Ok(Type.Unspecified list)

    let types (env: Env) : (Ident * Type) list = failwith "Not Implemented"

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
    let fromResultT state c result =
        match result with
        | Ok t -> IOk(state, t, c)
        | Error e -> IErr e

    let fromResult result =
        match result with
        | Ok x -> IOk x
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
        |> IResult.fromResultT state []

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

[<RequireQualifiedAccess>]
module Solving2 =
    /// Constructs the type constructor `ctor` with `args` by substituting the variable for the type of each argument in the body.
    let constructType args ctor : Result<Type, Range -> Type.Error> =
        match ctor with
        | Type.Constructor (eArgs, bounds, env, body) -> 
            eArgs
            |> List.map (fun eName ->
                args
                |> List.tryFind (fun (name, _t) -> Type.TVariable.equals name eName)
                |> Result.fromOption (Type.Error.insufficientTypeArgs eName ctor))
            |> Result.collect
            // TODO check that bounds are met here
            |> Result.map (fun args ->
                args
                |> List.fold (fun acc (var, t) -> acc |> Type.subst var t) body)
        | _ -> Error (Type.Error.notATypeConstructor ctor)

    let findSubstitutions (cs: Constraints) : (Type.TUnknown * Type) list =
        cs
        |> List.flatMap
            (fun { outer = outer
                   inner = inner
                   range = _ } ->
                match outer, inner with
                | Type.Unknown x, t -> [ x, t ]
                | t, Type.Unknown x -> [ x, t ]
                | _ -> [])

    let substituteInto (cs: Constraints) (t: Type) (subs: (Type.TUnknown * Type) list) : Constraints * Type =
        subs
        |> List.fold
            (fun (cs, t0) (x, t) ->
                cs
                |> List.map (fun c ->
                    { c with
                        Constraint.outer = c.outer |> Type.substUnknown x t
                        Constraint.inner = c.inner |> Type.substUnknown x t }),
                t0 |> Type.substUnknown x t)
            (cs, t)

    let rec separateConstraint (c: Constraint) : Result<Constraints, Type.Error> =
        let rangeErr e x = e x c.range

        let fail e = Error(e c.outer c.inner c.range)

        let newConstraint outer inner = c |> Constraint.copyRange outer inner

        let separateNewConstraint outer inner = newConstraint outer inner |> separateConstraint

        let addRange result = result |> Result.mapError (fun e -> e c.range)

        // a -> b :> c -> d
        // a :> d
        // b :> d
        match c.outer, c.inner with
        | Type.Function (outer1, outer2), Type.Function (inner1, inner2) ->
            [ separateNewConstraint outer1 inner1
              separateNewConstraint outer2 inner2 ]
            |> Result.collect
            |> Result.map List.concat

        // (a1, a2, ..., an) :> (b1, b2, ..., bn)
        // a1 :> b1
        // a2 :> b2
        // ...
        // an :> bn
        | Type.Tuple outer, Type.Tuple inner ->
            if outer.Length = inner.Length then
                List.zip outer inner
                |> List.map (fun (outer, inner) -> separateNewConstraint outer inner)
                |> Result.collectConcatenated
            else
                fail Type.Error.tupleElementCountMismatch

        // TODO Record should probably support row polymorphism so that update expression works

        // T :> a
        // res(T) :> a
        | Type.Named (name, env), inner ->
            env
            |> List.tryFind (fun (n, _) -> name |> Ident.equals n)
            |> Option.map (fun (_, t) -> t)
            |> Result.fromOption (rangeErr Type.Error.namedTypeNotFound name)
            |> Result.bind (fun outer -> separateNewConstraint outer inner)

        // a :> T
        // a :> res(T)
        | outer, Type.Named (name, env) ->
            env
            |> List.tryFind (fun (n, _) -> name |> Ident.equals n)
            |> Option.map (fun (_, t) -> t)
            |> Result.fromOption (rangeErr Type.Error.namedTypeNotFound name)
            |> Result.bind (fun inner -> separateNewConstraint outer inner)

        // T<a> :> b
        // cons(T, a) :> b
        | Type.Construct (args, ctor), inner ->
            constructType args ctor
            |> addRange
            |> Result.bind (fun outer -> separateNewConstraint outer inner)

        // a :> T<b>
        // a :> cons(T, b)
        | outer, Type.Construct (args, ctor) ->
            constructType args ctor
            |> addRange
            |> Result.bind (fun inner -> separateNewConstraint outer inner)

        // [id: T] :> [id: U]
        // T :> U
        | Type.Opaque (outerId, outer), Type.Opaque (innerId, inner) ->
            if Uid.equals outerId innerId then
                separateNewConstraint outer inner
            else
                fail Type.Error.opaqueTypeMismatch

        // [id: T] :> U
        // T :> U
        | Type.Opaque (_, outer), inner -> separateNewConstraint outer inner

        // TODO add a failure case for
        // T :> [id: U]
        // T :/> U

        // a! :> b!
        // a :> b
        | Type.Unsafe outer, Type.Unsafe inner -> separateNewConstraint outer inner

        // a& :> b&
        // a :> b
        | Type.Reference outer, Type.Reference inner -> separateNewConstraint outer inner

        // a => T(a) :> U(a)
        // (a => T(a))<args(T(a), U(a))> :> U(a)
        | Type.Constructor (_args, _bounds, _env, body) as ctor, inner ->
            let getArgs body t =
                body
                |> Type.tryMatch
                    (fun (tBody, t) ->
                        match tBody, t with
                        | Type.Variable var, t -> [ var, t ]
                        | _ -> [])
                    t
                |> addRange
            
            result {
                let! args = getArgs body inner
                return! separateNewConstraint (Type.Construct(args, ctor)) (inner)
            }

        // TODO Implement overload separation and rest of cases
        // TODO Decide on specific error messages to add here

        // <unknown> :> a
        // -- 
        // a :> <unknown>
        | _ -> Ok [ c ]

    let separateConstraints cs =
        cs
        |> List.map separateConstraint
        |> Result.collect
        |> Result.map List.concat

    let rec solveConstraints (cs: Constraints) (t: Type) : Result<Constraints * Type, Type.Error> =
        result {
            let! cs = separateConstraints cs
            let subs = findSubstitutions cs

            if subs.Length = 0 then
                return cs, t
            else
                let cs, t = subs |> substituteInto cs t
                return! solveConstraints cs t
        }

    let verifyConstraints (cs: Constraints) =
        cs
        |> List.map (fun c ->
            if c.outer |> Type.equals c.inner then
                Ok c
            else
                Error(Type.Error.expectedTypeMismatch c.inner c.outer c.range)) // actual, expected
        |> Result.collect


[<RequireQualifiedAccess>]
module Inference =

    /// Gets the unsolved type of an expressions, along with the constraints needed to solve the type.
    let rec inferUnsolvedType
        (solver: (Type -> Constraints) -> Type IResult -> Type IResult)
        (expr: Expr)
        (state: State)
        (constrainer: Type -> Env -> Constraints)
        (env: Env)
        : Type IResult =
        let constrainer t = constrainer t env
        let infer = inferUnsolvedType solver

        match expr with
        | Expr.Ident ident ->
            env
            // TODO also decide if env will be set in constructors here or somewhere else
            |> I.getType ident state

        // str
        | Expr.Literal (Literal.Str _) -> IResult.ret state (Type.Primitive Type.Str)

        // int
        | Expr.Literal (Literal.Int _) -> IResult.ret state (Type.Primitive Type.Int)

        // real
        | Expr.Literal (Literal.Real _) -> IResult.ret state (Type.Primitive Type.Real)

        // char
        | Expr.Literal (Literal.Char _) -> IResult.ret state (Type.Primitive Type.Char)

        // bool
        | Expr.Literal (Literal.Bool _) -> IResult.ret state (Type.Primitive Type.Bool)

        // type(th); bool :> type(guard); type(th) :> type(el)
        | Expr.Cond (guard, th, el) ->
            env
            |> infer guard state (I.Constrain.refineTo (Type.Primitive Type.Bool))
            |> IResult.bind (fun state _ ->
                env
                |> infer th state I.noConstraints
                |> IResult.bind (fun state thenType ->
                    env
                    |> infer el state (I.Constrain.refineTo thenType) // thenType :> type(else)
                    |> IResult.mapType (fun _ -> thenType)))

        // (type(x1), type(x2), ..., type(xn))
        | Expr.Tuple xs ->
            let rec inferXs state xs : (Type list) IResult =
                match xs with
                | [] -> IResult.ret state []
                | x :: xs ->
                    env
                    |> infer x state I.noConstraints
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
                |> infer expr state I.noConstraints
                |> IResult.mapType (fun rType -> Type.Function(pType, rType)))

        // type(expr); t :> type(expr)
        | Expr.Type (expr, t) -> env |> infer expr state (I.Constrain.refineTo t) // t :> type(expr); type(expr)

        // type(expr)
        | Expr.Tagged (expr, range) ->
            env
            |> Env.setCurrentRange range
            |> infer expr state I.noConstraints // TODO determine if constraining function needs to be passed

        // type(expr)&
        | Expr.Ref expr ->
            env
            |> infer expr state I.noConstraints
            |> IResult.mapType (fun t -> Type.Reference t)

        // new(1); type(expr) :> new(1)&
        | Expr.Deref expr ->
            state
            |> I.createUnknown
            |> IResult.bind (fun state innerType ->
                infer expr state (I.Constrain.refineFrom (Type.Reference innerType)) env
                |> IResult.mapType (fun _ -> innerType))

        // new(1); type(f) :> type(x) -> new(1)
        | Expr.App (f, x) ->
            state
            |> I.createUnknown
            |> IResult.bind (fun state rType ->
                env
                |> infer x state I.noConstraints
                |> IResult.bind (fun state xType ->
                    let iType = Type.Function(xType, rType)

                    env
                    |> infer f state (I.Constrain.refineFrom iType)
                    |> IResult.mapType (fun _ -> rType)))

        | Expr.Let (name, expr, body) ->
            infer expr state I.noConstraints env
            |> solver constrainer
            |> IResult.bind (fun state t ->
                let t = Type.gen t (env |> Env.types)

                env
                |> Env.extend name t
                |> infer body state I.noConstraints)

        | Expr.UnsafeLet (name, expr, body) ->
            // FIXME (type-subst) This is no longer correct.
            state
            |> I.createUnknown
            |> IResult.bind (fun state eType ->
                infer expr state (I.Constrain.refineFrom (Type.Unsafe eType)) env
                |> solver constrainer
                |> IResult.bind (fun state _t ->
                    Env.extend name eType env
                    |> infer body state I.noConstraints))

        | Expr.Block (expr, next) -> failwith "Not Implemented" // this finishes type inference for each line

        // TODO Implement
        | Expr.Record (elements) -> failwith "Not Implemented"
        | Expr.Extern (name, argument) -> failwith "Not Implemented"
        | Expr.NoRet -> failwith "Not Implemented"
        | Expr.Update (expr, fields) -> failwith "Not Implemented"
        | Expr.Mut (expr, value) -> failwith "Not Implemented"
        | Expr.Match (expr, case) -> failwith "Not Implemented"

        |> IResult.map (fun state t -> state, t, constrainer t)

    let defaultSolver constrainer iResult =
        result {
            let! state, t, cs = iResult |> IResult.toResult
            let! cs, t = Solving2.solveConstraints cs t
            let! _ = Solving2.verifyConstraints cs
            return state, t, cs
        }
        |> IResult.fromResult
        |> IResult.map (fun state t -> state, t, constrainer t)
