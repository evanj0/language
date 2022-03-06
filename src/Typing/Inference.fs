module Inference

open Ir
open UntypedIr

open Identifier
open ResultExtensions
open ListExtensions
open Range


type Constraint =
    { outer: Type
      inner: Type
      range: Range
      message: string }

[<RequireQualifiedAccess>]
module Constraint =

    let copyRange outer inner this =
        { this with
            outer = outer
            inner = inner }

    let setMessage message c = { c with Constraint.message = message }

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

    /// Retrieves types of values that match `pred` from the local scope.
    /// If a local value is found, this shadows any values in the global scope.
    /// If none are found in the local scope, the types of values that match `pred` are returned.
    let getTypesOfValues pred env =
        let pred = fun (ident, t_) -> pred ident
        let locals = env.locals |> List.filter pred

        if locals.Length >= 1 then
            [ locals |> List.head ]
        else
            env.globals |> List.filter pred
        |> List.map (fun (_ident, t) -> t)

    let types env = []


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

    let mapError mapping result =
        match result with
        | IOk (state, t, c) -> IOk(state, t, c)
        | IErr e -> IErr(mapping e)

[<RequireQualifiedAccess>]
module private I =

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
                range = env |> Env.currentRange
                message = "" } ]

        /// Same as `refineTo`, but with arguments reversed.
        let refineFrom inner outer this = refineTo outer inner this

[<RequireQualifiedAccess>]
module Solving =
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
        | _ -> Error(Type.Error.notATypeConstructor ctor)

    let findSubstitutions (cs: Constraints) : (Type.TUnknown * Type) list =
        cs
        |> List.flatMap
            (fun { outer = outer
                   inner = inner
                   range = _ } ->
                match outer, inner with
                | Type.Unknown x1, (Type.Unknown x2 as var) ->
                    // Check for production of a self substitution that would cause infinite recursion.
                    if Type.TUnknown.equals x1 x2 then
                        []
                    else
                        [ x1, var ]
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
                if (match t with
                    | Type.Unspecified _ -> false // don't want to do substitutions if t is unspecified
                    | _ -> true) then
                    t0 |> Type.substUnknown x t
                else
                    t0)
            (cs, t)


    /// Separates a single constraint into multiple by separating types when possible.
    /// Produces errors.
    let rec separateConstraint (c: Constraint) : Result<Constraints, Type.Error> =

        // This should always be used in this since it adds the trace message.
        let error e =
            e c.range |> Type.Error.addTraceMessage c.message

        let fail e = Error(error (e c.outer c.inner))

        let newConstraint outer inner = c |> Constraint.copyRange outer inner

        let separateNewConstraint outer inner =
            newConstraint outer inner |> separateConstraint

        let addRange result =
            result |> Result.mapError (fun e -> e c.range)

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
            |> Result.fromOption (Type.Error.namedTypeNotFound name |> error)
            |> Result.bind (fun outer -> separateNewConstraint outer inner)

        // a :> T
        // a :> res(T)
        | outer, Type.Named (name, env) ->
            env
            |> List.tryFind (fun (n, _) -> name |> Ident.equals n)
            |> Option.map (fun (_, t) -> t)
            |> Result.fromOption (Type.Error.namedTypeNotFound name |> error)
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
                return! separateNewConstraint (Type.Construct(args, ctor)) inner
            }

        | Type.Unspecified _, Type.Unspecified _ -> Ok [ c ]

        | Type.Unspecified (tOuters, name), inner ->
            if inner
               |> Type.contains (function
                   | Type.Unknown _ -> true
                   | _ -> false) then
                Ok [ c ]
            else
                tOuters
                |> List.tryFind (fun t -> t |> Type.isMoreGeneralThan inner)
                |> Result.fromOption (Type.Error.overloadNotFound (name, inner) c.range)
                |> Result.bind (fun t -> separateNewConstraint t inner)

        | outer, Type.Unspecified (tInners, name) ->
            if outer
               |> Type.contains (function
                   | Type.Unknown _ -> true
                   | _ -> false) then
                Ok [ c ]
            else
                tInners
                |> List.tryFind (fun t -> outer |> Type.isMoreGeneralThan t)
                |> Result.fromOption (Type.Error.overloadNotFound (name, outer) c.range)
                |> Result.bind (fun t -> separateNewConstraint outer t)

        // TODO Decide on specific error messages to add here

        // <unknown> :> a
        // --
        // a :> <unknown>
        | _ -> Ok [ c ]

    let separateConstraints cs : Result<Constraint list, Type.Error> =
        cs
        |> List.map separateConstraint
        |> Result.collect
        |> Result.map List.concat

    let rec solveConstraints (cs: Constraints) (t: Type) : Result<Constraints * Type, Type.Error> =
        
        let _ = 1
        result {
            let! cs = separateConstraints cs
            let subs = findSubstitutions cs

            match subs.Length with
            | 0 -> return cs, t
            | _ ->
                let cs, t = subs |> substituteInto cs t
                return! solveConstraints cs t
        }

    /// Verifies constraints that do not contribute to the final type.
    /// Produces errors.
    let verifyConstraints (cs: Constraints) : Result<Constraint list, Type.Error> =
        cs
        |> List.map (fun c ->
            if c.outer |> Type.isMoreGeneralThan c.inner then
                Ok c
            else
                Error(
                    Type.Error.expectedTypeMismatch c.inner c.outer c.range
                    |> Type.Error.addTraceMessage c.message
                )) // actual, expected
        |> Result.collect


    /// Checks for remaining unknowns types in `cs` and `t`.
    /// Produces errors.
    let checkForUnknowns cs t = failwith "Not Implemented" // TODO this should check for any remaining unknowns.

[<RequireQualifiedAccess>]
module Inference =

    /// Produces a potentially unsolved type and constraints used to solve the type.
    /// Produces errors.
    let rec infer
        (solver: (Type -> Constraints) -> Type IResult -> Type IResult)
        (expr: Expr)
        (state: State)
        (constrainer: Type -> Env -> Constraints)
        (env: Env)
        : Type IResult =

        let constrainer t = constrainer t env

        let infer = infer solver

        let fail e args = IErr(e args env.currentRange)

        match expr with

        // ident
        | Expr.Ident ident ->
            env
            |> Env.getTypesOfValues (fun id -> id |> Ident.contains ident)
            |> IResult.ret state
            |> IResult.bind (fun state list ->
                match list.Length with
                | 0 -> fail Type.Error.nameNotFound ident
                | 1 -> IResult.ret state list.Head
                | _ ->
                    IResult.ret state (Type.Unspecified(list, Ident.print ident))
                    |> IResult.bind (fun state unspecifiedT ->
                        state
                        |> I.createUnknown
                        |> IResult.map (fun state unknownT -> state, unknownT, I.Constrain.refineTo unspecifiedT unknownT env)))

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

        |> IResult.map (fun state t ->
            state,
            t,
            constrainer t
            |> List.map (fun c ->
                c
                |> Constraint.setMessage "TODO Add trace messages")) // TODO Add trace messages
        |> IResult.mapError (fun e ->
            e
            |> Type.Error.addTraceMessage "TODO Add trace messages") // TODO Add trace messages

    // TODO Other functions that produce errors need to insert the message

    let defaultSolver constrainer iResult =
        result {
            let! state, t, cs = iResult |> IResult.toResult
            let! cs, t = Solving.solveConstraints cs t
            let! _ = Solving.verifyConstraints cs
            return state, t, cs
        }
        |> IResult.fromResult
        |> IResult.map (fun state t -> state, t, constrainer t)
