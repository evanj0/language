module Cst2Ast

open Cst
open Range

let ident (id: ident) = Ast.Identifier id.inner

let resolvedIdent ((list, id): resolvedIdent) = 
    let reversed = list |> List.rev |> List.map (fun x -> x.inner)
    Ast.ResolvedIdentifier (id.inner, reversed), 
    Range.create 
        (if list.Length >= 1 then (list |> List.head).start else id.start) 
        (id.stop)

let escapedCharacter (c: escapedCharacter) =
    match c with
    | DoubleQuote -> '\"'
    | Newline -> '\n'
    | Tab -> '\t'
    | Backslash -> '\\'
    | Unicode (c1, c2, c3, c4) -> 
        System.Int32.Parse($"{c1}{c2}{c3}{c4}", System.Globalization.NumberStyles.HexNumber) 
        |> char
    | AnyCharacter c -> c

let stringLiteral ((l, cs, r): stringLiteral) =
    let string = 
        cs 
        |> List.map escapedCharacter
        |> System.String.Concat
    string, Range.create l.start r.stop

let literalDigits (list: literalDigit list) =
    list
    |> List.map (fun x ->
        match x with
        | D0 -> '0'
        | D1 -> '1'
        | D2 -> '2'
        | D3 -> '3'
        | D4 -> '4'
        | D5 -> '5'
        | D6 -> '6'
        | D7 -> '7'
        | D8 -> '8'
        | D9 -> '9'
        | Underscore -> '_')
    |> List.filter (fun x -> x <> '_')
    |> System.String.Concat

let integerLiteral (x: integerLiteral) =
    let (digits, suffix) = x.inner
    System.Int64.Parse(literalDigits digits) * (if suffix.IsSome then -1L else 1L), 
    Range.create x.start x.stop

let realLiteral (x: realLiteral) =
    let (integral, fractional, suffix) = x.inner
    let string = $"{literalDigits integral}.{literalDigits fractional}"
    System.Double.Parse(string) * (if suffix.IsSome then -1.0 else 1.0), 
    Range.create x.start x.stop

let rec quantifiedType t = failwith "Not Implemented"

and ty (t: ty) = 
    match t with
    | Type (t, ts) -> 
        let rec funcs t ts =
            match t, ts with
            | t, (_, t1)::(ts) -> 
                match t1 with
                | SafeFunction (_, _, t2) ->
                    let astT = typeU t
                    let astT2 = funcs t2 ts
                    Ast.Type.create (Ast.Type.Function (astT, astT2)) astT.range.start astT2.range.stop
                | UnsafeFunction (_, _, t2) ->
                    let astT = typeU t
                    let astT2 = funcs t2 ts
                    Ast.Type.create (Ast.Type.UnsafeFunction (astT, astT2)) astT.range.start astT2.range.stop
            | t, [] -> typeU t
        funcs t ts
        

and typeU t =
    match t with
    | Union (x, xs) ->
        let types =
            x :: (xs |> List.map (fun (_, x) -> x))
            |> List.map typeR
        if types.Length = 1 then
            types.Head
        else 
            Ast.Type.create (Ast.Type.Union types) types.Head.range.start (types |> List.last).range.stop
        

and typeR t : Ast.Type.Type =
    match t with
    | typeR.Reference (t, rOpt) ->
        match rOpt with
        | Some (_, kw) -> 
            let astType = enclosedType t
            Ast.Type.create (Ast.Type.Reference astType) astType.range.start kw.stop 
        | None -> enclosedType t

and enclosedType (x: enclosedType) =
    match x with
    | Primitive x ->
        Ast.Type.create
            (
                match x.inner with
                | primitive.String -> Ast.Type.Primitive Ast.Type.String
                | primitive.Integer -> Ast.Type.Primitive Ast.Type.Integer
                | primitive.Real -> Ast.Type.Primitive Ast.Type.Real
                | primitive.Character -> Ast.Type.Primitive Ast.Type.Character
                | primitive.Boolean -> Ast.Type.Primitive Ast.Type.Boolean)
            x.start
            x.stop
    | enclosedType.Unit (l, _, r) -> Ast.Type.create (Ast.Type.Tuple []) l.start r.stop
    | enclosedType.Parens (l, _, x, xs, _, r) ->
        if xs.Length = 0
        then ty x
        else 
            xs
            |> List.map (fun (_, x) -> x)
            |> fun xs -> x :: xs
            |> List.map ty
            |> fun xs -> Ast.Type.create (Ast.Type.Tuple xs) l.start r.stop
    | enclosedType.EmptyRecord (l, _, r) -> Ast.Type.create (Ast.Type.Record []) l.start r.stop
    | enclosedType.Record (l, _, x, xs, _, r) ->
        let recordField (id, _, _, t) = ident id, ty t
        xs
        |> List.map (fun (_, x) -> x)
        |> fun xs -> x :: xs
        |> List.map recordField
        |> fun xs -> Ast.Type.create (Ast.Type.Record xs) l.start r.stop
    | Nominal (name, args) ->
        let ident, identRange = resolvedIdent name
        match args with
        | Some (_, (_, _, x, xs, _, r)) ->
            let args = 
                x 
                :: (xs |> List.map (fun (_, x) -> x))
                |> List.map (fun t -> ty t)
            Ast.Type.create 
                (Ast.Type.Application 
                    (( Ast.Type.create (Ast.Type.Named ident) identRange.start identRange.stop),
                       args))
                identRange.start 
                r.stop
        | None -> Ast.Type.create (Ast.Type.Named ident) identRange.start identRange.stop
    | Variable(_, name, args) ->
        let ident, identStart, identStop = name.inner, name.start, name.stop
        match args with
        | Some (_, (_, _, x, xs, _, r)) ->
            let args = 
                x 
                :: (xs |> List.map (fun (_, x) -> x))
                |> List.map (fun t -> ty t)
            Ast.Type.create 
                (Ast.Type.Application 
                    (( Ast.Type.create (Ast.Type.Variable ident) identStart identStop),
                       args))
                identStart 
                r.stop
        | None -> Ast.Type.create (Ast.Type.Variable ident) identStart identStop

let rec pattern (x: pattern): Ast.Pattern.Pattern = 
    match x with
    | Assignment (kw, _, id, _, _, pat) ->
        let astPat = pattern pat
        Ast.Pattern.create
            (Ast.Pattern.Assignment (ident id, astPat))
            kw.start
            astPat.range.stop
    | Variant (pat, ts) ->
        let astPat = enclosedPattern pat
        if ts.Length = 0
        then 
            astPat
        else 
            ts
            |> List.map (fun (_, x) -> x)
            |> List.map ty
            |> List.fold 
                (fun acc x -> Ast.Pattern.create (Ast.Pattern.Variant (x, acc)) astPat.range.start x.range.stop)
                astPat

and enclosedPattern (x: enclosedPattern) =
    match x with 
    | enclosedPattern.Ident id -> 
        Ast.Pattern.create
            (Ast.Pattern.Identifier (ident id))
            id.start
            id.stop
    | Discard kw -> Ast.Pattern.create Ast.Pattern.Discard kw.start kw.stop
    | enclosedPattern.Unit (l, _, r) -> Ast.Pattern.create (Ast.Pattern.Tuple []) l.start r.stop
    | enclosedPattern.Parens (l, _, x, xs, _, r) -> 
        if xs.Length = 0
        then pattern x
        else 
            xs
            |> List.map (fun (_, x) -> x)
            |> fun xs -> x :: xs
            |> List.map pattern
            |> fun xs -> Ast.Pattern.create (Ast.Pattern.Tuple xs) l.start r.stop
    | enclosedPattern.EmptyRecord (l, _, r) -> Ast.Pattern.create (Ast.Pattern.Record []) l.start r.stop
    | enclosedPattern.Record (l, _, x, xs, _, r) ->
        let patternFieldInit (id, _, _, pat) = ident id, pattern pat
        xs
        |> List.map (fun (_, x) -> x)
        |> fun xs -> x :: xs
        |> List.map patternFieldInit
        |> fun xs -> Ast.Pattern.create (Ast.Pattern.Record xs) l.start r.stop
    | enclosedPattern.String x -> 
        let string, range = stringLiteral x
        Ast.Pattern.create (Ast.Pattern.Literal (Ast.String string)) range.start range.stop 
    | enclosedPattern.Integer x ->
        let int, range = integerLiteral x
        Ast.Pattern.create (Ast.Pattern.Literal (Ast.Integer int)) range.start range.stop
    | enclosedPattern.Real x ->
        let real, range = realLiteral x
        Ast.Pattern.create (Ast.Pattern.Literal (Ast.Real real)) range.start range.stop
    | enclosedPattern.Character (l, c, r) ->
        Ast.Pattern.create (Ast.Pattern.Literal (Ast.Character (escapedCharacter c))) l.start r.stop
    | enclosedPattern.Boolean x ->
        Ast.Pattern.create (Ast.Pattern.Literal (Ast.Boolean (x.inner))) x.start x.stop

let rec expr (x: expr): Ast.Expression.Expression =
    match x with
    | SubExpr x -> subExpr x
    | Lambda (branch, branches) ->
        let cases2Ast (xs: lambdaBranch list) = 
            let cases =
                xs
                |> List.map (fun (kw, _, x, xs, _, _, e) ->
                    let patterns = 
                        x :: (xs |> List.map (fun (_, x) -> x))
                        |> List.map pattern
                    let astExpr = subExpr e
                    (
                        if patterns.Length = 1 
                        then patterns |> List.head 
                        else 
                            Ast.Pattern.create 
                                (Ast.Pattern.Tuple patterns) 
                                (patterns |> List.head).range.start 
                                (patterns |> List.last).range.stop),
                    astExpr,
                    Range.create kw.start astExpr.range.stop)
            cases 
            |> List.map (fun (pat, ex, _) -> pat, ex),
            Range.create
                (match cases |> List.head with (_, _, range) -> range.start)
                (match cases |> List.last with (_, _, range) -> range.stop)

        let rec createParameters number =
            assert (number >= 0)
            match number with
            | 0 -> []
            | _ -> 
                let parameter =
                    number
                    |> NameGeneration.generateParamName
                    |> Ast.Ident.create
                parameter :: createParameters (number - 1)

        let cases, range = 
            branch 
            :: (branches |> List.map (fun (_, x) -> x))
            |> cases2Ast

        let rec createFunction matchingExpr (parameters: Ast.Ident list) =
            match parameters with
            | [] -> 
                Ast.Expression.create
                    (Ast.Expression.Match (matchingExpr, cases))
                    range.start
                    range.stop
            | p :: ps ->
                Ast.Expression.create
                    (Ast.Expression.Function (p, createFunction matchingExpr ps))
                    range.start
                    range.stop

        let parameters = createParameters (match branch with (_, _, _, xs, _, _, _) -> 1 + xs.Length)
        let matchingExpr = 
            if parameters.Length = 1
            then 
                parameters 
                |> List.head 
                |> Ast.Ident.toResolvedIdent 
                |> Ast.Expression.Identifier
                |> fun ident -> Ast.Expression.create ident range.start range.stop
            else 
                parameters
                |> List.map Ast.Ident.toResolvedIdent
                |> List.map Ast.Expression.Identifier
                |> List.map (fun e -> Ast.Expression.create e range.start range.stop)
                |> Ast.Expression.TupleConstructor
                |> fun e -> Ast.Expression.create e range.start range.stop

        createFunction matchingExpr parameters
        

and subExpr (x: subExpr): Ast.Expression.Expression =
    match x with
    | Let (kw, _, id, _, _, e) -> 
        let astE = expr e
        Ast.Expression.create (Ast.Expression.Let (ident id, astE)) kw.start astE.range.stop
    | UnsafeLet (kw, _, id, _, _, e) ->
        let astE = expr e
        Ast.Expression.create (Ast.Expression.UnsafeLet (ident id, astE)) kw.start astE.range.stop
    | UnsafeDo (kw, _, e) ->
        let astE = expr e
        Ast.Expression.create (Ast.Expression.UnsafeDo astE) kw.start astE.range.stop
    | Conditional ((kw, _, e1, _), (_, _, e2, _), (_, _, e3)) -> 
        let astE3 = expr e3
        Ast.Expression.create (Ast.Expression.Conditional (expr e1, expr e2, astE3)) kw.start astE3.range.stop
    | Reference (kw, _, e) ->
        let astE = expr e
        Ast.Expression.create (Ast.Expression.Reference astE) kw.start astE.range.stop
    | Dereference (kw, _, e) ->
        let astE = expr e
        Ast.Expression.create (Ast.Expression.Dereference astE) kw.start astE.range.stop
    | Mutate (kw, _, e1, _, _, e2) ->
        let astE2 = expr e2
        Ast.Expression.create (Ast.Expression.Mutate (expr e1, astE2)) kw.start astE2.range.stop
    | Ordered x -> subExprE x

and subExprE (ExplicitType (e, types)) =
    if types.Length = 0
    then subExprU e
    else 
        types 
        |> List.fold
            (fun acc (_, t) -> 
                let astT = quantifiedType t
                Ast.Expression.create 
                    (Ast.Expression.ExplicitType (acc, astT)) 
                    acc.range.start 
                    (match astT with Ast.Type.QuantifiedType (t, _, _) -> t.range.stop))
            (subExprU e)

and subExprU (Update (e, updates)) = 
    if updates.Length = 0
    then subExprA e
    else 
        updates
        |> List.fold
            (fun acc (_, items) ->
                let (astItems, range) = recordBody items
                Ast.Expression.create
                    (Ast.Expression.RecordUpdate (acc, astItems))
                    acc.range.start
                    range.stop)
            (subExprA e)

and recordBody ((l, _, x, xs, _, r): recordBody): (Ast.Ident * Ast.Expression.Expression) list * Range =
    let rest = 
        xs 
        |> List.map (fun (_, x) -> fieldInit x)
    let list = fieldInit x :: rest
    list, Range.create l.start r.stop


and fieldInit ((id, _, _, e): fieldInit) = (ident id, expr e)

and subExprA (Application (e, es)) =
    if es.Length = 0
    then subExprC e
    else 
        es
        |> List.fold
            (fun acc (_, expr) ->
                let astExpr = subExprC expr
                Ast.Expression.create
                    (Ast.Expression.Application (acc, astExpr))
                    acc.range.start
                    astExpr.range.stop)
            (subExprC e)

and subExprC (Chain (e, es)) =
    if es.Length = 0
    then atom e
    else
        es
        |> List.fold
            (fun acc (_, expr) ->
                let astExpr = atom expr
                Ast.Expression.create
                    (Ast.Expression.Application (astExpr, acc))
                    acc.range.start
                    astExpr.range.stop)
            (atom e)

and atom (e: atom) =
    match e with
    | Ident x -> 
        let (ident, range) = resolvedIdent x
        Ast.Expression.create 
            (Ast.Expression.Identifier ident) 
            range.start 
            range.stop
    | Parens (l, _, x, xs, _, r) ->
        if xs.Length = 0
        then expr x
        else
            let items = expr x :: (xs |> List.map (fun (_, x) -> expr x))
            Ast.Expression.create 
                (Ast.Expression.TupleConstructor items) 
                l.start 
                r.stop
    | Unit (l, _, r) -> 
        Ast.Expression.create 
            (Ast.Expression.TupleConstructor []) 
            l.start 
            r.stop
    | Record x ->
        let (items, range) = recordBody x
        Ast.Expression.create
            (Ast.Expression.RecordConstructor items)
            range.start
            range.stop
    | EmptyRecord (l, _, r) ->
        Ast.Expression.create
            (Ast.Expression.RecordConstructor [])
            l.start
            r.stop
    | List (l, _, x, xs, _, r) ->
        let rec createAstList list =
            match list with
            | [] -> 
                Ast.Expression.create
                    (Ast.Expression.RecordConstructor [])
                    l.start
                    r.stop
            | x::xs -> 
                Ast.Expression.create 
                    (Ast.Expression.TupleConstructor [expr x; createAstList xs])
                    l.start
                    r.stop
        createAstList (x :: (xs |> List.map (fun (_, x) -> x)))
    | EmptyList (l, _, r) ->
        Ast.Expression.create
            (Ast.Expression.RecordConstructor [])
            l.start
            r.stop
    | Block (l, _, x, xs, _, r) ->
        let items = 
            x :: (xs |> List.map (fun (_, x) -> x)) 
            |> List.map expr
        Ast.Expression.create
            (Ast.Expression.Block items)
            l.start
            r.stop
    | Extern (l, _, s, _, e, _, r) ->
        let (name, _) = stringLiteral s
        Ast.Expression.create
            (Ast.Expression.External (name, expr e))
            l.start
            r.stop
    | String x ->
        let (string, range) = stringLiteral x
        Ast.Expression.create
            (Ast.Expression.Literal (Ast.String string))
            range.start
            range.stop
    | Integer x ->
        let (value, range) = integerLiteral x
        Ast.Expression.create
            (Ast.Expression.Literal (Ast.Integer value))
            range.start
            range.stop
    | Real x ->
        let (value, range) = realLiteral x
        Ast.Expression.create
            (Ast.Expression.Literal (Ast.Real value))
            range.start
            range.stop
    | Character (l, c, r) ->
        Ast.Expression.create
            (Ast.Expression.Literal (Ast.Character (escapedCharacter c)))
            l.start
            r.stop
    | Boolean x ->
        Ast.Expression.create
            (Ast.Expression.Literal (Ast.Boolean x.inner))
            x.start
            x.stop