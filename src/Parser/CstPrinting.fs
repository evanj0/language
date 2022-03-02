module CstPrinting

open Cst

(* SHARED *)

let private str (list: string list) = System.String.Concat list

let private list delimiterPrinter itemPrinter list = 
    list 
    |> List.map (fun (delimiter, item) -> str [delimiterPrinter delimiter; itemPrinter item]) 
    |> str

let private opt printer option =
    option |> Option.map printer |> Option.defaultValue ""

let private opt2 printer1 printer2 = opt (fun (x1, x2) -> str [printer1 x1; printer2 x2])

let private opt3 printer1 printer2 printer3 = opt (fun (x1, x2, x3) -> str [printer1 x1; printer2 x2; printer3 x3])

(* spacing *)

let private singleSpace x =
    match x with
    | singleSpace.Whitespace -> "\u0020"
    | singleSpace.Newline -> "\n"
    | singleSpace.Tab -> "\t"
    | singleSpace.Comment (_, text, _) -> str ["(*"; text; "*)"]


let space (space: space) = space |> List.map (fun x -> singleSpace x) |> str

(* identifiers *)

let ident (x: ident) = x.inner

let resolvedIdent ((xs, x): resolvedIdent) = 
    [
        xs |> List.map (fun typename -> $"{typename.inner}::") |> str; 
        x.inner
    ] |> str

let resolvedTypename ((xs, x): resolvedTypename) =
    [
        xs |> List.map (fun typename -> $"{typename.inner}::") |> str;
        x.inner
    ] |> str

(* delimiters *)

let private delim string (s1, s2) = str [space s1; string; space s2]

let private commaDelim = delim ","

(* literals *)

let escapedCharacter c =
    match c with
    | escapedCharacter.DoubleQuote -> "\\\""
    | escapedCharacter.Newline -> "\\n"
    | escapedCharacter.Tab -> "\\t"
    | escapedCharacter.Backslash -> "\\\\"
    | escapedCharacter.Unicode (c1, c2, c3, c4) -> $"\\u{c1}{c2}{c3}{c4}"
    | escapedCharacter.AnyCharacter c -> $"{c}"

let characterLiteral((_, c, _): characterLiteral) =
    ["'"; escapedCharacter c; "'"] |> str

let stringLiteral ((_, chars, _): stringLiteral) =
    ["\""; chars |> List.map escapedCharacter |> str; "\""] |> str

let literalDigit x =
    match x with 
    | D0 -> "0" 
    | D1 -> "1"
    | D2 -> "2"
    | D3 -> "3"
    | D4 -> "4"
    | D5 -> "5"
    | D6 -> "6"
    | D7 -> "7"
    | D8 -> "8"
    | D9 -> "9"
    | Underscore -> "_"

let negativeSuffix x =
    match x with 
    | Uppercase -> "N"
    | Lowercase -> "n"

let negativeSuffixOpt x = x |> Option.map negativeSuffix |> Option.defaultValue ""

let digitSequence xs = xs |> List.map literalDigit |> str

let integerLiteral (x: integerLiteral) =
    let (digits, suffix) = x.inner
    [digitSequence digits; negativeSuffixOpt suffix] |> str

let realLiteral (x: realLiteral) =
    let (int, frac, suffix) = x.inner
    [digitSequence int; "."; digitSequence frac; negativeSuffixOpt suffix] |> str

let booleanLiteral (x: booleanLiteral) =
    match x.inner with
    | true -> "true"
    | false -> "false"

(* TYPES *)

let rec quantifiedType (QuantifiedType (x, rest)) = 
    [ty x; opt3 space forall (opt2 space where) rest] |> str

and forall (_, s1, x, xs) = 
    ["forall"; space s1; ident x; list commaDelim ident xs] |> str

and where (_, s1, x, xs) =
    ["where"; space s1; typeConstraint x; list commaDelim typeConstraint xs] |> str

and typeConstraint c =
    match c with 
    | typeConstraint.NamedConstraint (name, s1, _, s2, x, xs, s3, _) -> [resolvedTypename name; space s1; "("; space s2; ident x; list commaDelim ident xs; space s3; ")"]
    | typeConstraint.Constraint (_, s1, x, xs, s2, _) -> ["{"; space s1; constraintItem x; list commaDelim constraintItem xs; space s2; "}"]
    |> str

and constraintItem (id, s1, s2, t) =
    [ident id; space s1; ":"; space s2; quantifiedType t] |> str

and ty (Type (x, fns)) =
    [typeU x; list space func fns] |> str

and func x = 
    match x with
    | SafeFunction (_, s, t) -> ["->"; space s; typeU t]
    | UnsafeFunction (_, s, t) -> ["-!>"; space s; typeU t]
    |> str
    
and typeU (Union (x, xs)) = [typeR x; list (delim "|") typeR xs] |> str

and typeR (typeR.Reference (t, r)) = 
    [
        enclosedType t; 
        r |> Option.map (fun (s, _) -> $"{space s}&") |> Option.defaultValue ""
    ] |> str

and enclosedType t =
    match t with 
    | Primitive p -> [primitive p.inner]
    | Nominal (name, args) -> [resolvedTypename name; opt2 space typeArgs args]
    | Variable (_, name, args) -> ["'"; ident name; opt2 space typeArgs args]
    | enclosedType.Unit (_, s, _) -> ["("; space s; ")"]
    | enclosedType.Parens (_, s1, x, xs, s2, _) -> ["("; space s1; ty x; list commaDelim ty xs; space s2; ")"]
    | enclosedType.EmptyRecord (_, s, _) -> ["{"; space s; "}"]
    | enclosedType.Record (_, s1, x, xs, s2, _) -> ["{"; space s1; recordField x; list commaDelim recordField xs; space s2; "}"]
    |> str

and primitive p =
    match p with 
    | primitive.String -> "String"
    | primitive.Integer -> "Integer"
    | primitive.Real -> "Real"
    | primitive.Character -> "Character"
    | primitive.Boolean -> "Boolean"

and typeArgs (_, s1, x, xs, s2, _) =
    ["<"; space s1; ty x; list commaDelim ty xs; space s2; ">"] |> str

and recordField (id, s1, s2, t) =
    [ident id; space s1; ":"; space s2; ty t] |> str

(* PATTERNS *)

let rec pattern x = 
    match x with 
    | Assignment (_, s1, id, s2, s3, pat) -> ["let"; space s1; ident id; space s2; "="; space s3; pattern pat]
    | Variant (pat, xs) -> [enclosedPattern pat; list (delim ":") ty xs]
    |> str

and enclosedPattern x =
    match x with 
    | enclosedPattern.Ident x -> ident x
    | enclosedPattern.Discard _ -> "_"
    | enclosedPattern.Unit (_, s, _) -> str ["("; space s; ")"]
    | enclosedPattern.Parens (_, s1, x, xs, s2, _) -> str ["("; space s1; pattern x; list commaDelim pattern xs; space s2; ")"]
    | enclosedPattern.EmptyRecord (_, s, _) -> str ["{"; space s; "}"]
    | enclosedPattern.Record (_, s1, x, xs, s2, _) -> str ["{"; space s1; patternFieldInit x; list commaDelim patternFieldInit xs; space s2; "}"]
    | enclosedPattern.String x -> stringLiteral x
    | enclosedPattern.Integer x -> integerLiteral x
    | enclosedPattern.Real x -> realLiteral x
    | enclosedPattern.Character x -> characterLiteral x
    | enclosedPattern.Boolean x -> booleanLiteral x

and patternFieldInit (id, s1, s2, pat) =
    str [ident id; space s1; "="; space s2; pattern pat]

(* EXPRESSIONS *)

let rec expr x =
    match x with
    | SubExpr x -> [subExpr x]
    | Lambda (x, xs) -> [lambdaBranch x; list space lambdaBranch xs]
    |> str

and lambdaBranch (_, s1, p, ps, s2, s3, e) = 
    ["|"; space s1; pattern p; list space pattern ps; space s2; "->"; space s3; subExpr e] |> str

and subExpr this =
    match this with
    | Let (_, s1, id, s2, s3, e) -> 
        ["let"; space s1; ident id; space s2; "="; space s3; expr e]
    | UnsafeLet (_, s1, id, s2, s3, e) ->
        [sprintf "let!%s%s%s=%s%s" (space s1) (ident id) (space s2) (space s3) (expr e)]
    | UnsafeDo (_, s, e) -> [sprintf "do!%s%s" (space s) (expr e)]
    | Conditional ((_, s1, e1, s2), (_, s3, e2, s4), (_, s5, e3)) -> 
        ["if"; space s1; expr e1; space s2; "then"; space s3; expr e2; space s4; "else"; space s5; expr e3]
    | Reference (_, s, e) -> 
        ["ref"; space s; expr e]
    | Dereference (_, s, e) -> [sprintf "deref%s%s" (space s) (expr e)]
    | Mutate (_, s1, e1, s2, s3, e2) ->
        ["mut"; space s1; expr e1; space s2; "<-"; space s3; expr e2]
    | Ordered x -> [subExprE x]
    |> str

and subExprE (ExplicitType (e, types)) = 
    str [subExprU e; list (delim ":") quantifiedType types]

and subExprU (Update (e, updates)) = 
    str [subExprA e; list (delim "with") recordBody updates]

and subExprA (Application (e, rest)) = 
    str [subExprC e; list space subExprC rest]

and subExprC (Chain (e, rest)) =
    str [atom e; list (delim ".") atom rest]

and atom x =
    match x with
    | atom.Ident x -> [resolvedIdent x]
    | atom.Parens (_, s1, x, xs, s2, _) -> ["("; space s1; expr x; list commaDelim expr xs; space s2; ")"]
    | atom.Unit (_, s, _) -> ["("; space s; ")"]
    | atom.Record x -> [recordBody x]
    | atom.EmptyRecord (_, s, _) -> ["{"; space s; "}"]
    | atom.List (_, s1, x, xs, s2, _) -> ["["; space s1; expr x; list commaDelim expr xs; space s2; "]"]
    | atom.EmptyList (_, s, _) -> ["["; space s; "]"]
    | atom.Block (_, s1, x, xs, s2, _) -> ["@{"; space s1; expr x; list (delim ";") expr xs; space s2; "}"]
    | atom.Extern (_, s1, x, s2, e, s3, _) -> ["[<"; space s1; stringLiteral x; space s2; expr e; space s3; ">]"]
    | atom.String s -> [stringLiteral s]
    | atom.Integer x -> [integerLiteral x]
    | atom.Real x -> [realLiteral x]
    | atom.Character x -> [characterLiteral x]
    | atom.Boolean x -> [booleanLiteral x]
    |> str

and recordBody (_, s1, x, xs, s2, _) = 
    ["{"; space s1; fieldInit x; list commaDelim fieldInit xs; space s2; "}"] |> str
and fieldInit (id, s1, s2, e) =
    [ident id; space s1; "="; space s2; expr e] |> str

(* MODULE *)

let rec file (File (s1, x, xs, s2)) =
    str [space s1; topLevel x; list space topLevel xs; space s2]

and topLevel (Module (_, s1, name, exports, s2, s3, x, xs)) =
    str ["module"; space s1; resolvedTypename name; opt2 space exportList exports; space s2; "="; space s3; declaration x; list (delim ";") declaration xs]

and exportList (_, s1, x, xs, s2, _) =
    str ["("; space s1; ident x; list commaDelim ident xs; space s2; ")"]

and declaration x =
    match x with 
    | Import (_, s, id, filter) -> ["import"; space s; resolvedTypename id; opt2 space importFilter filter]
    | Definition (_, s1, id, s2, s3, t) -> ["def"; space s1; ident id; space s2; ":"; space s3; quantifiedType t]
    | Binding (_, s1, id, s2, s3, e) -> ["let"; space s1; ident id; space s2; "="; space s3; expr e]
    | Newtype (_, s1, name, s2, s3, t) -> ["type"; space s1; ident name; space s2; "="; space s3; quantifiedType t]
    | Constraint (_, s1, name, s2, ps, s3, s4, c, cs) -> ["bound"; space s1; ident name; space s2; constraintParams ps; space s3; "="; space s4; typeConstraint c; list commaDelim typeConstraint cs]
    |> str

and importFilter (_, s1, x, xs, s2, _) = 
    str ["("; space s1; ident x; list commaDelim ident xs; space s2; ")"]

and constraintParams (_, s1, x, xs, s2, _) =
    str ["("; space s1; ident x; list commaDelim ident xs; space s2; ")"]