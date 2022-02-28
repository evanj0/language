module CstSharedParsing

open FParsec
open Cst

(* tuple flattening *)

let tup3 ((x1, x2), x3) = (x1, x2, x3)
let tup4 (((x1, x2), x3), x4) = (x1, x2, x3, x4)
let tup5 ((((x1, x2), x3), x4), x5) = (x1, x2, x3, x4, x5)
let tup6 (((((x1, x2), x3), x4), x5), x6) = (x1, x2, x3, x4, x5, x6)
let tup7 ((((((x1, x2), x3), x4), x5), x6), x7) = (x1, x2, x3, x4, x5, x6, x7)
let tup8 (((((((x1, x2), x3), x4), x5), x6), x7), x8) = (x1, x2, x3, x4, x5, x6, x7, x8)
let tup9 ((((((((x1, x2), x3), x4), x5), x6), x7), x8), x9) = (x1, x2, x3, x4, x5, x6, x7, x8, x9)
let tup10 (((((((((x1, x2), x3), x4), x5), x6), x7), x8), x9), x10) = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

(* reserved keywords *)

let isReserved string =
    match string with
    | "if" | "then" | "else" | "ref" | "mut" | "let" | "true" | "false" | "with" | "->" | "<-" | "module" -> true
    | _ -> false

let ensureNotReserved separator parser =
    parser >>=? fun x -> if separator x |> isReserved then fail $"Cannot use reserved keyword ‘{separator x}’ in identifier." else preturn x

(* position *)

let pos parser = 
    getPosition .>>.? parser .>>. getPosition 
    |>> fun ((p1, x), p2) -> Pos (x, 
        { index = p1.Index; line = p1.Line; column = p1.Column }, 
        { index = p2.Index; line = p2.Line; column = p2.Column })

let pcharpos c: Parser<ckw, _> = pos (pchar c)

let pstringpos s: Parser<skw, _> = pos (pstring s)

(* spacing *)

let pWhitespace = pchar '\u0020' >>% Whitespace
let pNewline = newline >>% singleSpace.Newline
let pTab = tab >>% singleSpace.Tab
let pComment: Parser<_, unit> = 
    pstringpos "(*" .>>. manyCharsTill anyChar (lookAhead (pstring "*)")) .>>. pstringpos "*)" 
    |>> tup3 |>> Comment
let singleSpace = pWhitespace <|> pNewline <|> pTab <|> pComment
let space: Parser<space, _> = many1 singleSpace
let optSpace: Parser<space, _> = many singleSpace

(* literals *)

let pEscapedDoubleQuote: Parser<_, unit> = pstring "\\\"" >>% escapedCharacter.DoubleQuote
let pEscapedNewline = pstring "\\n" >>% escapedCharacter.Newline
let pEscapedTab = pstring "\\t" >>% escapedCharacter.Tab
let pEscapedBackslash = pstring "\\\\" >>% escapedCharacter.Backslash
let pUnicode = pstring "\\u" >>. hex .>>. hex .>>. hex .>>. hex |>> tup4 |>> escapedCharacter.Unicode
let pAnyCharacter = noneOf "\\" |>> escapedCharacter.AnyCharacter
let escapedCharacter =
    pEscapedDoubleQuote <|> pEscapedNewline <|> pEscapedTab <|> pEscapedBackslash <|> pUnicode <|> pAnyCharacter

let stringLiteral: Parser<stringLiteral, _> = pcharpos '"' .>>. manyTill escapedCharacter (lookAhead (pchar '"')) .>>. pcharpos '"' |>> tup3

let characterLiteral: Parser<characterLiteral, _> = pcharpos '\'' .>>. escapedCharacter .>>. pcharpos '\'' |>> tup3

let pDigit: Parser<_, unit> = 
    (pchar '0' >>% D0) 
    <|> (pchar '1' >>% D1)
    <|> (pchar '2' >>% D2)
    <|> (pchar '3' >>% D3)
    <|> (pchar '4' >>% D4)
    <|> (pchar '5' >>% D5)
    <|> (pchar '6' >>% D6)
    <|> (pchar '7' >>% D7)
    <|> (pchar '8' >>% D8)
    <|> (pchar '9' >>% D9)
    <|> (pchar '_' >>% Underscore)
let pUppercaseNegativeSuffix = pchar 'N' >>% negativeSuffix.Uppercase
let pLowercaseNegativeSuffix = pchar 'n' >>% negativeSuffix.Lowercase
let negativeSuffix = pUppercaseNegativeSuffix <|> pLowercaseNegativeSuffix

let integerLiteral: Parser<integerLiteral, _> = many1 pDigit .>>.? opt negativeSuffix |> pos

let realLiteral: Parser<realLiteral, _> = many1 pDigit .>>? pchar '.' .>>. many1 pDigit .>>. opt negativeSuffix |>> tup3 |> pos

let booleanLiteral: Parser<booleanLiteral, unit> = (pstringpos "true" >>% true) <|> (pstringpos "false" >>% false) |> pos

(* identifiers *)

[<Literal>]
let identChar = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!$%^&*-_=+<>?/1234567890'"

[<Literal>]
let identStartChar = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!$%^&*-_=+<>?/"

[<Literal>]
let typenameChar = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890'"

[<Literal>]
let typenameStartChar = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

let ident: Parser<ident, unit> = 
    anyOf identStartChar .>>. many (anyOf identChar) 
    |>> List.Cons |>> System.String.Concat |> pos
    |> ensureNotReserved (fun x -> x.inner)

let typename: Parser<typename, unit> =
    anyOf typenameStartChar .>>. many (anyOf typenameChar)
    |>> List.Cons |>> System.String.Concat |> pos
    |> ensureNotReserved (fun x -> x.inner)

let resolvedIdent: Parser<resolvedIdent, _> = 
    many (typename .>>? pstring "::") .>>.? ident

let resolvedTypename: Parser<resolvedTypename, _> = many (typename .>>? pstring "::") .>>. typename

(* delimiters *)

let delim parser: Parser<_, unit> = optSpace .>>? parser .>>. optSpace

let commaDelim: Parser<commaDelim, _> = delim (pchar ',')
let semicolonDelim: Parser<semicolonDelim, _> = delim (pchar ';')