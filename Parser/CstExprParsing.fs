module CstExprParsing

open FParsec
open Cst
open CstSharedParsing
open CstTypeParsing
open CstPatternParsing

let (expr: Parser<expr, unit>, exprRef) = createParserForwardedToRef()

let pIdent = resolvedIdent |>> atom.Ident
let pParens = 
    pcharpos '(' .>>.? optSpace .>>.? expr .>>. many (commaDelim .>>. expr) .>>. optSpace .>>. pcharpos ')'
    |>> tup6 |>> atom.Parens
let pUnit = pcharpos '(' .>>.? optSpace .>>.? pcharpos ')' |>> tup3 |>> atom.Unit
let fieldInit = ident .>>. space .>> pchar '=' .>>. optSpace .>>. expr |>> tup4
let recordBody = 
    pcharpos '{' .>>.? optSpace .>>.? fieldInit .>>. many (commaDelim .>>. fieldInit) .>>. optSpace .>>. pcharpos '}'
    |>> tup6
let pRecord = recordBody |>> atom.Record
let pEmptyRecord = pcharpos '{' .>>.? optSpace .>>.? pcharpos '}' |>> tup3 |>> atom.EmptyRecord
let pList = 
    pcharpos '[' .>>.? optSpace .>>.? expr .>>. many (commaDelim .>>. expr) .>>. optSpace .>>. pcharpos ']'
    |>> tup6 |>> atom.List
let pEmptyList = pcharpos '[' .>>.? optSpace .>>.? pcharpos ']' |>> tup3 |>> atom.EmptyList
let pBlock = 
    pstringpos "@{" .>>. optSpace .>>. expr .>>. many (semicolonDelim .>>. expr) .>>. optSpace .>>. pcharpos '}' 
    |>> tup6 |>> atom.Block
let pExtern = 
    pstringpos "[<" .>>. optSpace .>>. stringLiteral .>>. optSpace .>>. expr .>>. optSpace .>>. pstringpos ">]" 
    |>> tup7 |>> atom.Extern
let pString = stringLiteral |>> atom.String
let pInteger = integerLiteral |>> atom.Integer
let pReal = realLiteral |>> atom.Real
let pCharacter = characterLiteral |>> atom.Character
let pBoolean = booleanLiteral |>> atom.Boolean
let atom = 
    pIdent 
    <|> pParens 
    <|> pUnit 
    <|> pRecord 
    <|> pEmptyRecord 
    <|> pList 
    <|> pEmptyList 
    <|> pBlock 
    <|> pExtern 
    <|> pString 
    <|> pReal 
    <|> pInteger 
    <|> pCharacter 
    <|> pBoolean

let pChain = atom .>>. many (delim (pchar '.') .>>. atom) |>> Chain
let subExprC = pChain

let pApplication = subExprC .>>. many (optSpace .>>.? subExprC) |>> Application
let subExprA = pApplication

let pUpdate = subExprA .>>. many (delim (pstring "with") .>>. recordBody) |>> Update
let subExprU = pUpdate

let pExplicitType = subExprU .>>. many (delim (pchar ':') .>>. quantifiedType) |>> ExplicitType
let subExprE = pExplicitType

let pLet = pstringpos "let" .>>. space .>>. ident .>>. space .>> pchar '=' .>>. optSpace .>>. expr |>> tup6 |>> subExpr.Let
let guardBranch = pstringpos "if" .>>. optSpace .>>. expr .>>. optSpace |>> tup4
let thenBranch = pstringpos "then" .>>. optSpace .>>. expr .>>. optSpace |>> tup4
let elseBranch = pstringpos "else" .>>. optSpace .>>. expr |>> tup3
let pConditional = guardBranch .>>. thenBranch .>>. elseBranch |>> tup3 |>> subExpr.Conditional
let pReference = pstringpos "ref" .>>. optSpace .>>. expr |>> tup3 |>> subExpr.Reference
let pMutate = 
    pstringpos "mut" .>>. optSpace .>>. expr .>>. optSpace .>> pstring "<-" .>>. optSpace .>>. expr 
    |>> tup6 |>> subExpr.Mutate
let pOrdered = subExprE |>> subExpr.Ordered
let subExpr = 
    pLet 
    <|> pConditional 
    <|> pReference 
    <|> pMutate 
    <|> pOrdered

let pSubExpr = subExpr |>> SubExpr
let lambdaBranch = 
    pcharpos '|' .>>. optSpace .>>. pattern .>>. many (optSpace .>>.? pattern) .>>. optSpace .>> 
    pstring "->" .>>. optSpace .>>. subExpr
    |>> tup7
let pLambda = lambdaBranch .>>. many (optSpace .>>. lambdaBranch) |>> Lambda
let exprImpl = pSubExpr <|> pLambda
do exprRef.Value <- exprImpl