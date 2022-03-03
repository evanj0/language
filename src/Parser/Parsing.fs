module Parsing

[<RequireQualifiedAccess>]
module Parse =
    open FParsec

    let ``type`` string =
        match run CstTypeParsing.ty string with
        | Success (node, _, _) -> Result.Ok(Cst2Ast.ty node)
        | Failure (message, _, _) -> Result.Error(message)

    let expr string =
        match run CstExprParsing.expr string with
        | Success (node, _, _) -> Result.Ok(Cst2Ast.expr node)
        | Failure (message, _, _) -> Result.Error(message)