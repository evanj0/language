module Cst

open Range

(* Non-idiomatic capitalization in this file matches convention in grammar definition. *)

type pos<'a> = Pos of 'a * startPos: Position * endPos: Position
with
    member self.inner with get() = match self with Pos(inner, _, _) -> inner
    member self.start with get() = match self with Pos(_, start, _) -> start
    member self.stop with get() = match self with Pos(_, _, stop) -> stop

let unwrapPos f (Pos (inner, _, _)) = f inner

(* SHARED *)

(* keyword *)

type ckw = char pos
type skw = string pos

(* spacing *)

type singleSpace =
    | Whitespace
    | Newline
    | Tab
    | Comment of skw * text: string * skw

type space = singleSpace list
    
(* literals *)

type escapedCharacter =
    | DoubleQuote
    | Newline
    | Tab
    | Backslash
    | Unicode of sequence: (char * char * char * char)
    | AnyCharacter of char

type literalDigit = 
    | D0
    | D1
    | D2
    | D3
    | D4
    | D5
    | D6
    | D7
    | D8
    | D9
    | Underscore

type negativeSuffix = 
    | Uppercase
    | Lowercase

type integerLiteral = (literalDigit list * negativeSuffix option) pos
type realLiteral = (literalDigit list * literalDigit list * negativeSuffix option) pos
type stringLiteral = ckw * escapedCharacter list * ckw
type characterLiteral = ckw * escapedCharacter * ckw
type booleanLiteral = bool pos

(* identifiers *)

type ident = string pos
type typename = string pos
type resolvedIdent = typename list * ident
type resolvedTypename = typename list * typename

(* delimiters *)

type commaDelim = space * space
type colonDelim = space * space
type vbarDelim = space * space
type arrowDelim = space * space
type unsafeDelim = space * space
type semicolonDelim = space * space
type dotDelim = space * space
type withDelim = space * space

(* TYPES *)

type quantifiedType = QuantifiedType of ty * (space * forall * (space * where) option) option
and forall = skw * space * ident * (commaDelim * ident) list
and where = skw * space * typeConstraint * (commaDelim * typeConstraint) list

and typeConstraint = 
    | NamedConstraint of name: resolvedTypename * space * ckw * space * first: ident * rest: (commaDelim * ident) list * space * ckw
    | Constraint of ckw * space * first: constraintItem * rest: (commaDelim * constraintItem) list * space * ckw
and constraintItem = ident * space * space * quantifiedType

and ty = Type of typeU * (space * func) list
and func = 
        | SafeFunction of skw * space * typeU
        | UnsafeFunction of skw * space * typeU
and typeU = Union of first: typeR * rest: (vbarDelim * typeR) list
and typeR = Reference of ty: enclosedType * (space * ckw) option

and enclosedType =
    | Primitive of primitive pos
    | Nominal of name: resolvedTypename * args: (space * arguments) option
    | Variable of ckw * name: typename * args: (space * arguments) option
    | Unit of ckw * space * ckw
    | Parens of ckw * space * first: ty * rest: (commaDelim * ty) list * space * ckw
    | EmptyRecord of ckw * space * ckw
    | Record of ckw * space * first: recordField * rest: (commaDelim * recordField) list * space * ckw
and primitive = String | Integer | Real | Character | Boolean
and arguments = ckw * space * ty * (commaDelim * ty) list * space * ckw
and recordField = ident * space * space * ty

(* PATTERNS *)

type pattern =
    | Assignment of skw * space * ident: ident * space * space * pattern: pattern
    | Variant of first: enclosedPattern * rest: (colonDelim * ty) list

and enclosedPattern =
    | Ident of ident
    | Discard of ckw
    | Unit of ckw * space * ckw
    | Parens of ckw * space * first: pattern * rest: (commaDelim * pattern) list * space * ckw
    | EmptyRecord of ckw * space * ckw
    | Record of ckw * space * first: patternFieldInit * rest: (commaDelim * patternFieldInit) list * space * ckw
    | String of stringLiteral
    | Integer of integerLiteral
    | Real of realLiteral
    | Character of characterLiteral
    | Boolean of booleanLiteral
and patternFieldInit = ident * space * space * pattern

(* EXPRESSIONS *)

type expr = 
    | SubExpr of subExpr
    | Lambda of first: lambdaBranch * rest: (space * lambdaBranch) list

and lambdaBranch = ckw * space * pattern * (space * pattern) list * space * space * subExpr

and subExpr = 
    | Let of skw * space * ident: ident * space * space * expr: expr
    | Conditional of guardBranch * thenBranch * elseBranch
    | Reference of skw * space * expr: expr
    | Mutate of skw * space * expr1: expr * space * space * expr2: expr
    | Ordered of subExprE
and guardBranch = skw * space * expr * space
and thenBranch = skw * space * expr * space
and elseBranch = skw * space * expr

and subExprE = ExplicitType of subExprU * (colonDelim * quantifiedType) list
and subExprU = Update of subExprA * (withDelim * recordBody) list
and subExprA = Application of first: subExprC * rest: (space * subExprC) list
and subExprC = Chain of first: atom * rest: (dotDelim * atom) list

and atom =
    | Ident of resolvedIdent
    | Parens of ckw * space * first: expr * rest: (commaDelim * expr) list * space * ckw
    | Unit of ckw * space * ckw
    | Record of recordBody
    | EmptyRecord of ckw * space * ckw
    | List of ckw * space * first: expr * rest: (commaDelim * expr) list * space * ckw
    | EmptyList of ckw * space * ckw
    | Block of skw * space * first: expr * rest: (semicolonDelim * expr) list * space * ckw
    | Extern of skw * space * name: stringLiteral * space * expr: expr * space * skw
    | String of stringLiteral
    | Integer of integerLiteral
    | Real of realLiteral
    | Character of characterLiteral
    | Boolean of booleanLiteral
and recordBody = ckw * space * fieldInit * (commaDelim * fieldInit) list * space * ckw
and fieldInit = ident * space * space * expr

(* MODULE *)

type file = File of space * topLevel * (space * topLevel) list * space

and topLevel = 
    Module of 
        skw * space * resolvedTypename * (space * exportList) option * space *
        space * declaration * (semicolonDelim * declaration) list
and exportList = ckw * space * ident * (commaDelim * ident) list * space * ckw

and declaration = 
    | Import of skw * space * resolvedTypename * (space * importFilter) option
    | Definition of skw * space * ident * space * space * quantifiedType
    | Binding of skw * space * ident * space * space * expr
    | Newtype of skw * space * typename * space * space * quantifiedType
    | Constraint of 
        skw * space * typename * space * constraintParams * space * 
        space * typeConstraint * (commaDelim * typeConstraint) list
and importFilter = ckw * space * ident * (commaDelim * ident) list * space * ckw
and constraintParams = ckw * space * typename * (commaDelim * typename) list * space * ckw 