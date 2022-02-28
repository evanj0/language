module CstPatternParsing

open FParsec
open Cst
open CstSharedParsing
open CstTypeParsing

let (pattern: Parser<pattern, unit>, patternRef) = createParserForwardedToRef()

let pIdent = ident |>> enclosedPattern.Ident
let pDiscard = pcharpos '_' |>> enclosedPattern.Discard
let pUnit = pcharpos '(' .>>.? optSpace .>>.? pcharpos ')' |>> tup3 |>> enclosedPattern.Unit
let pParens = 
    pcharpos '(' .>>.? optSpace .>>.? pattern .>>. many (commaDelim .>>. pattern) .>>. optSpace .>>. pcharpos ')'
    |>> tup6 |>> enclosedPattern.Parens
let pEmptyRecord = pcharpos '{' .>>.? optSpace .>>.? pcharpos '}' |>> tup3 |>> enclosedPattern.EmptyRecord
let patternFieldInit = ident .>>. space .>> pchar '=' .>>. optSpace .>>. pattern |>> tup4
let pRecord = 
    pcharpos '{' .>>.? optSpace .>>.? patternFieldInit .>>. many (commaDelim .>>. patternFieldInit) .>>. optSpace .>>. pcharpos '}'
    |>> tup6 |>> enclosedPattern.Record
let pString = stringLiteral |>> enclosedPattern.String
let pInteger = integerLiteral |>> enclosedPattern.Integer
let pReal = realLiteral |>> enclosedPattern.Real
let pCharacter = characterLiteral |>> enclosedPattern.Character
let pBoolean = booleanLiteral |>> enclosedPattern.Boolean
let enclosedPattern =
    pIdent 
    <|> pDiscard 
    <|> pUnit 
    <|> pParens 
    <|> pEmptyRecord 
    <|> pRecord 
    <|> pString 
    <|> pReal 
    <|> pInteger 
    <|> pCharacter 
    <|> pBoolean

let pVariant = enclosedPattern .>>. many (delim (pchar ':') .>>. ty) |>> Variant
let pAssignment = 
    pstringpos "let" .>>. space .>>. ident .>>. space .>> pchar '=' .>>. optSpace .>>. pattern 
    |>> tup6 |>> Assignment
let patternImpl = pVariant <|> pAssignment
do patternRef.Value <- patternImpl