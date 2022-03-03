module Lowering

[<RequireQualifiedAccess>]
module Lower =

    open Identifier

    let lIdent (Ast.ResolvedIdentifier (final, modules)) = Ident.create (final, modules)

    let rec lType (t: Ast.Type.Type) =
        match t.ty with
        | Ast.Type.Named ident -> Ir.Type.Named (lIdent ident, []) // TODO need to put environment into here at some point
        | Ast.Type.Primitive Ast.Type.String -> Ir.Type.Primitive Ir.Type.Str
        | Ast.Type.Primitive Ast.Type.Integer -> Ir.Type.Primitive Ir.Type.Int
        | Ast.Type.Primitive Ast.Type.Real -> Ir.Type.Primitive Ir.Type.Real
        | Ast.Type.Primitive Ast.Type.Character -> Ir.Type.Primitive Ir.Type.Char
        | Ast.Type.Primitive Ast.Type.Boolean -> Ir.Type.Primitive Ir.Type.Bool
        | Ast.Type.Function (left, right) -> Ir.Type.Function (lType left, lType right)
        | Ast.Type.UnsafeFunction (left, right) -> Ir.Type.Function (lType left, Ir.Type.Unsafe (lType right))
        | Ast.Type.Reference inner -> Ir.Type.Reference (lType inner)


    let rec lExpr (e: Ast.Expression.Expression) =
        match e.expr with
        | Ast.Expression.Identifier ident -> UntypedIr.Expr.Ident (lIdent ident)
        | Ast.Expression.Literal (Ast.Literal.String x) -> UntypedIr.Expr.Literal (Ir.Literal.Str x)
        | Ast.Expression.Literal (Ast.Literal.Integer x) -> UntypedIr.Expr.Literal (Ir.Literal.Int x)
        | Ast.Expression.Literal (Ast.Literal.Real x) -> UntypedIr.Expr.Literal (Ir.Literal.Real x)
        | Ast.Expression.Literal (Ast.Literal.Character x) -> UntypedIr.Expr.Literal (Ir.Literal.Char x)
        | Ast.Expression.Literal (Ast.Literal.Boolean x) -> UntypedIr.Expr.Literal (Ir.Literal.Bool x)
        | Ast.Expression.Application (f, x) -> UntypedIr.Expr.App (lExpr f, lExpr x)