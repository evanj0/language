module TypedIr

open Ir

open Range
open Identifier

[<RequireQualifiedAccess>]
module Expr =
    type Expr =
        | Tagged of expr: Expr * range: Range
        | Ident of ident: Ident * t: Type
        | Literal of value: Literal.Literal * t: Type
        | Tuple of elements: Expr list * t: Type
        | Record of elements: (string * Expr) list * t: Type
        | Block of expr: Expr * next: Expr * t: Type
        | Extern of name: string * argument: Expr * t: Type
        | Type of expr: Expr * t: Type
        | Update of expr: Expr * fields: (string * Expr) list * t: Type
        | App of f: Expr * x: Expr * t: Type
        | Let of name: string * expr: Expr * body: Expr * t: Type
        | UnsafeLet of name: string * expr: Expr * body: Expr * t: Type
        | Cond of guard: Expr * th: Expr * el: Expr
        | Ref of expr: Expr * t: Type
        | Deref of expr: Expr * t: Type
        | Mut of expr: Expr * value: Expr * t: Type
        | Func of p: string * body: Expr * pt: Type
        | Match of expr: Expr * cases: (Pattern * Expr) list * t: Type
        member this.t =
            match this with
            | Tagged (expr, _tag) -> expr.t
            | Func (pt = pt; body = body) -> Type.Function(pt, body.t)
            | Cond (th = th) -> th.t
            | Ident (t = t)
            | Literal (t = t)
            | Tuple (t = t)
            | Record (t = t)
            | Block (t = t)
            | Extern (t = t)
            | Type (t = t)
            | Update (t = t)
            | App (t = t)
            | Let (t = t)
            | UnsafeLet (t = t)
            | Ref (t = t)
            | Deref (t = t)
            | Mut (t = t)
            | Match (t = t) -> t

type Expr = Expr.Expr

[<RequireQualifiedAccess>]
module TypedIr =

    module UExpr = UntypedIr.Expr

    type State = { index: int }

    [<RequireQualifiedAccess>]
    module State =
        let init = { State.index = 0 }

        let unknown state =
            { state with State.index = state.index + 1 }, Type.Unknown(Type.TUnknown.create state.index)

        let bind f (state, x) = f state x

        let rec bindMap f (state, list) =
            match list with
            | x :: xs ->
                let state, x = f state x
                let state, xs = bindMap f (state, xs)
                state, x :: xs
            | [] -> state, []

    let rec fromUntyped (state: State) (expr: UntypedIr.Expr) : State * Expr =
        match expr with
        | UExpr.Tagged (expr, tag) ->
            let state, expr = expr |> fromUntyped state
            state, Expr.Tagged(expr, tag)
        | UExpr.Ident (ident) ->
            let state, t = state |> State.unknown
            state, Expr.Ident(ident, t)
        | UExpr.Literal (literal) ->
            let state, t = state |> State.unknown
            state, Expr.Literal(literal, t)
        | UExpr.Tuple (elements) ->
            let state, elements = (state, elements) |> State.bindMap fromUntyped
            let t = elements |> List.map (fun x -> x.t) |> Type.Tuple
            state, Expr.Tuple(elements, t)
        | UExpr.Record (elements) ->
            let state, elements =
                (state, elements)
                |> State.bindMap (fun state (name, expr) -> 
                    let (state, expr) = expr |> fromUntyped state in state, (name, expr))

            let t =
                elements
                |> List.map (fun (n, x) -> n, x.t)
                |> Type.Record

            state, Expr.Record(elements, t)
        | UExpr.Block(expr, next) ->
            let state, expr = expr |> fromUntyped state
            let state, next = next |> fromUntyped state
            let t = expr.t
            state, Expr.Block(expr, next, t)
        | UExpr.Extern(name, argument) ->
            let state, t = state |> State.unknown
            let state, argument = argument |> fromUntyped state
            state, Expr.Extern(name, argument, t)
        | UExpr.Type(expr, t) ->
            let state, expr = expr |> fromUntyped state
            state, Expr.Type(expr, t)
        | UExpr.Update(expr, fields) ->
            let state, expr = expr |> fromUntyped state
            let state, fields =
                (state, fields)
                |> State.bindMap (fun state (name, expr) -> 
                    let (state, expr) = expr |> fromUntyped state in state, (name, expr))
            state, Expr.Update(expr, fields, expr.t)
        | UExpr.App(f, x) ->
            let state, f = f |> fromUntyped state
            let state, x = x |> fromUntyped state
            let state, t = state |> State.unknown
            state, Expr.App(f, x, t)
        | UExpr.Let(name, expr, body) ->
            let state, expr = expr |> fromUntyped state
            let state, body = body |> fromUntyped state
            let t = body.t
            state, Expr.Let(name, expr, body, t)
        | UExpr.UnsafeLet(name, expr, body) ->
            let state, expr = expr |> fromUntyped state
            let state, body = body |> fromUntyped state
            let t = body.t
            state, Expr.UnsafeLet(name, expr, body, t)
        | UExpr.Cond(guard, th, el) ->
            let state, guard = guard |> fromUntyped state
            let state, th = th |> fromUntyped state
            let state, el = el |> fromUntyped state
            state, Expr.Cond(guard, th, el)
        
        
        | UExpr.Func(p, body) ->
            let state, pt = state |> State.unknown
            let state, body = body |> fromUntyped state
            state, Expr.Func(p, body, pt)
