module Lowering

[<RequireQualifiedAccess>]
module Lower =

    open Identifier

    let lIdent (Ast.ResolvedIdentifier (final, modules)) = Ident.create (final, modules)

    let lShortIdent (Ast.Ident.Identifier (string)) = string

    let rec lType (t: Ast.Type.Type) =
        match t.ty with
        | Ast.Type.Named ident -> Ir.Type.Named(lIdent ident)
        | Ast.Type.Primitive Ast.Type.String -> Ir.Type.Primitive Ir.Type.Str
        | Ast.Type.Primitive Ast.Type.Integer -> Ir.Type.Primitive Ir.Type.Int
        | Ast.Type.Primitive Ast.Type.Real -> Ir.Type.Primitive Ir.Type.Real
        | Ast.Type.Primitive Ast.Type.Character -> Ir.Type.Primitive Ir.Type.Char
        | Ast.Type.Primitive Ast.Type.Boolean -> Ir.Type.Primitive Ir.Type.Bool
        | Ast.Type.Function (left, right) -> Ir.Type.Function(lType left, lType right)
        | Ast.Type.UnsafeFunction (left, right) -> Ir.Type.Function(lType left, Ir.Type.Unsafe(lType right))
        | Ast.Type.Reference inner -> Ir.Type.Reference(lType inner)
        | Ast.Type.Tuple ts -> ts |> List.map lType |> Ir.Type.Tuple

    let lQuantifiedType (Ast.Type.QuantifiedType (t, _vars, _bounds)) = lType t

    let rec lPattern (p: Ast.Pattern.Pattern) : Ir.Pattern = 
        match p.pat with
        | Ast.Pattern.Identifier id -> lShortIdent id |> Ir.Pattern.Ident

    let rec lExpr (e: Ast.Expression.Expression) =
        match e.expr with
        | Ast.Expression.Identifier ident -> UntypedIr.Expr.Ident(lIdent ident)
        | Ast.Expression.Literal (Ast.Literal.String x) -> UntypedIr.Expr.Literal(Ir.Literal.Str x)
        | Ast.Expression.Literal (Ast.Literal.Integer x) -> UntypedIr.Expr.Literal(Ir.Literal.Int x)
        | Ast.Expression.Literal (Ast.Literal.Real x) -> UntypedIr.Expr.Literal(Ir.Literal.Real x)
        | Ast.Expression.Literal (Ast.Literal.Character x) -> UntypedIr.Expr.Literal(Ir.Literal.Char x)
        | Ast.Expression.Literal (Ast.Literal.Boolean x) -> UntypedIr.Expr.Literal(Ir.Literal.Bool x)
        | Ast.Expression.Application (f, x) -> UntypedIr.Expr.App(lExpr f, lExpr x)
        | Ast.Expression.TupleConstructor xs -> xs |> List.map lExpr |> UntypedIr.Expr.Tuple
        | Ast.Expression.ExplicitType (expr, t) -> UntypedIr.Expr.Type(lExpr expr, lQuantifiedType t)
        | Ast.Expression.Conditional (guard, t, e) -> UntypedIr.Expr.Cond(lExpr guard, lExpr t, lExpr e)
        | Ast.Expression.Lambda cases ->

            let createParameters count =
                [ 0 .. count - 1 ]
                |> List.map (fun i -> NameGeneration.generateParamName (string i))

            let rec createFunctions ps inner =
                match ps with
                | [] -> inner
                | p :: ps -> UntypedIr.Expr.Func(p, createFunctions ps inner)

            let parameters = createParameters (cases |> List.head |> fun (ps, _) -> ps.Length)

            let paramsExpr =
                parameters
                |> List.map Ident.fromString
                |> List.map UntypedIr.Expr.Ident
                |> UntypedIr.Expr.Tuple

            let matchCases =
                cases
                |> List.map (fun (ps, e) -> ps |> List.map lPattern |> Ir.Pattern.Tuple, lExpr e)

            let matchExpr = UntypedIr.Expr.Match(paramsExpr, matchCases)

            let lambdaExpr = createFunctions parameters matchExpr

            match cases with
            | (pats, expr) :: [] ->
                let rec tryCreateNested (pats: Ast.Pattern.Pattern list) =
                    match pats with
                    | [] -> Some(lExpr expr)
                    | p :: ps ->
                        match p.pat with
                        | Ast.Pattern.Identifier id ->
                            tryCreateNested ps
                            |> Option.bind (fun body -> Some(UntypedIr.Expr.Func(lShortIdent id, body)))
                        | _ -> None

                match tryCreateNested pats with
                | Some expr -> expr
                | None -> lambdaExpr
            | _ -> lambdaExpr

        | Ast.Expression.Block items ->
            let rec lExprs xs =
                match xs with
                | { Ast.Expression.expr = Ast.Expression.Let(name, value) }::xs ->
                    UntypedIr.Expr.Let(lShortIdent name, lExpr value, lExprs xs)
                | { Ast.Expression.expr = Ast.Expression.UnsafeLet(name, value) }::xs ->
                    UntypedIr.Expr.UnsafeLet(lShortIdent name, lExpr value, lExprs xs)
                | { Ast.Expression.expr = Ast.Expression.UnsafeDo(value) }::xs ->
                    UntypedIr.Expr.UnsafeLet(NameGeneration.generateDiscard, lExpr value, lExprs xs)
                | x::[] -> lExpr x
                | x::xs -> 
                    UntypedIr.Expr.Block(lExpr x, lExprs xs)
                | [] -> failwith "Unreachable"

            lExprs items