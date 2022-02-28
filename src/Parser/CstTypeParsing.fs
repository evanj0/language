module CstTypeParsing

open FParsec
open Cst
open CstSharedParsing

let (ty: Parser<ty, unit>, tyRef) = createParserForwardedToRef()

let pPrimitive = 
    (pstring "String" >>% primitive.String)
    <|> (pstring "Integer" >>% primitive.Integer)
    <|> (pstring "Real" >>% primitive.Real)
    <|> (pstring "Character" >>% primitive.Character)
    <|> (pstring "Boolean" >>% primitive.Boolean) 
    |> pos |>> enclosedType.Primitive
let arguments = 
    pcharpos '<' .>>. optSpace .>>. ty .>>. many (commaDelim .>>. ty) .>>. optSpace .>>. pcharpos '>'
    |>> tup6
let pNominal = resolvedTypename .>>. opt (optSpace .>>.? arguments) |>> enclosedType.Nominal
let pVariable = pcharpos ''' .>>. typename .>>. opt (optSpace .>>.? arguments) |>> tup3 |>> enclosedType.Variable
let pUnit = pcharpos '(' .>>.? optSpace .>>.? pcharpos ')' |>> tup3 |>> enclosedType.Unit
let pParens = 
    pcharpos '(' .>>.? optSpace .>>.? ty .>>. many (commaDelim .>>. ty) .>>. optSpace .>>. pcharpos ')'
    |>> tup6 |>> enclosedType.Parens
let pEmptyRecord = pcharpos '{' .>>.? optSpace .>>.? pcharpos '}' |>> tup3 |>> enclosedType.EmptyRecord
let recordField = ident .>>. optSpace .>> pchar ':' .>>. optSpace .>>. ty |>> tup4
let pRecord = 
    pcharpos '{' .>>.? optSpace .>>.? recordField .>>. many (commaDelim .>>. recordField) .>>. optSpace .>>. pcharpos '}' 
    |>> tup6 |>> enclosedType.Record
let enclosedType = 
    pPrimitive 
    <|> pNominal 
    <|> pVariable 
    <|> pUnit 
    <|> pParens 
    <|> pEmptyRecord 
    <|> pRecord

let pReference = enclosedType .>>. opt (optSpace .>>.? pcharpos '&') |>> typeR.Reference
let typeR = pReference

let pUnion = typeR .>>. many (delim (pchar '|') .>>. typeR) |>> typeU.Union
let typeU = pUnion

let pUnsafeFunction = pstringpos "-!>" .>>.? optSpace .>>. typeU |>> tup3 |>> UnsafeFunction
let pSafeFunction = pstringpos "->" .>>.? optSpace .>>. typeU |>> tup3 |>> SafeFunction
let func = pUnsafeFunction <|> pSafeFunction
let tyImpl = typeU .>>. many (optSpace .>>.? func) |>> Type
do tyRef.Value <- tyImpl

let (quantifiedType: Parser<quantifiedType, unit>, quantifiedTypeRef) = createParserForwardedToRef()

let constraintItem = ident .>>. optSpace .>> pchar ':' .>>. optSpace .>>. quantifiedType |>> tup4
let pConstraint = 
    pcharpos '{' .>>. optSpace .>>. constraintItem .>>. many (commaDelim .>>. constraintItem) .>>. optSpace .>>. pcharpos '}'
    |>> tup6 |>> typeConstraint.Constraint
let pNamedConstraint = 
    resolvedTypename .>>. optSpace .>>. 
    pcharpos '(' .>>. optSpace .>>. ident .>>. many (commaDelim .>>. ident) .>>. optSpace .>>. pcharpos ')'
    |>> tup8 |>> typeConstraint.NamedConstraint
let typeConstraint = pNamedConstraint <|> pConstraint

let where: Parser<where, _> = 
    pstringpos "where" .>>. optSpace .>>. typeConstraint .>>. many (commaDelim .>>. typeConstraint) |>> tup4
let forall: Parser<forall, _> = 
    pstringpos "forall" .>>. space .>>. ident .>>. many (commaDelim .>>. ident) |>> tup4
let pQuantifiedType = 
    ty .>>. opt (optSpace .>>.? forall .>>. opt (optSpace .>>.? where) |>> tup3) |>> QuantifiedType
do quantifiedTypeRef.Value <- pQuantifiedType