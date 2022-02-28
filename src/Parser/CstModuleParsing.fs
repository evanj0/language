module CstModuleParsing

open FParsec
open Cst
open CstSharedParsing
open CstTypeParsing
open CstPatternParsing
open CstExprParsing

let importFilter = 
    pcharpos '(' .>>. optSpace .>>. ident .>>. many (commaDelim .>>. ident) .>>. optSpace .>>. pcharpos ')' 
    |>> tup6
let pImport = 
    pstringpos "import" .>>. space .>>. resolvedTypename .>>. opt (optSpace .>>. importFilter) 
    |>> tup4 |>> Import
let pDefinition = 
    pstringpos "def" .>>. space .>>. ident .>>. optSpace .>> pchar ':' .>>. optSpace .>>. quantifiedType
    |>> tup6 |>> Definition
let pBinding = 
    pstringpos "let" .>>. space .>>. ident .>>. space .>> pchar '=' .>>. optSpace .>>. expr
    |>> tup6 |>> Binding
let pNewtype = 
    pstringpos "type" .>>. space .>>. typename .>>. space .>> pchar '=' .>>. optSpace .>>. quantifiedType
    |>> tup6 |>> Newtype
let constraintParams = pcharpos '(' .>>. optSpace .>>. typename .>>. many (commaDelim .>>. typename) .>>. optSpace .>>. pcharpos ')' |>> tup6
let pConstraint = 
    pstringpos "bound" .>>. space .>>. typename .>>. optSpace .>>. constraintParams .>>. optSpace .>>
    pchar '=' .>>. optSpace .>>. typeConstraint .>>. many (commaDelim .>>. typeConstraint)
    |>> tup9 |>> declaration.Constraint
let declaration = 
    pImport 
    <|> pDefinition 
    <|> pBinding 
    <|> pNewtype 
    <|> pConstraint

let exportList = pcharpos '(' .>>. optSpace .>>. ident .>>. many (commaDelim .>>. ident) .>>. optSpace .>>. pcharpos ')' |>> tup6
let pModule = 
    pstringpos "module" .>>. optSpace .>>. resolvedTypename .>>. opt (optSpace .>>. exportList) .>>. optSpace .>>
    pchar '=' .>>. optSpace .>>. declaration .>>. many (semicolonDelim .>>. declaration)
    |>> tup8 |>> Module
let topLevel = pModule

let pFile = optSpace .>>. topLevel .>>. many (optSpace .>>. topLevel) .>>. optSpace .>> eof |>> tup4 |>> File
let file = pFile