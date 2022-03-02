module Ast

open Range

/// Module names are stored in reverse order. `A::B::C::x` becomes `(x, [C; B; A])` or `(x, (cons C (cons B (cons C nil))))`
type ResolvedIdent = ResolvedIdentifier of ident: string * modules: string list

type Ident = Identifier of value: string

module Ident =

    let create string = Identifier string

    let toResolvedIdent ident = match ident with Identifier s -> ResolvedIdentifier (s, [])

type Literal =
    | String of string
    | Integer of int64
    | Real of float
    | Character of char
    | Boolean of bool

module Type =

    type Primitive = 
        | String
        | Integer
        | Real
        | Character
        | Boolean

    type Type = 
        { ty: Type'
          range: Range }

    and Type' = 
        | Primitive of Primitive
    
        | Tuple of items: Type list
        | Record of fields: (Ident * Type) list

        | Union of variants: Type list
        | Function of left: Type * right: Type
        | UnsafeFunction of left: Type * right: Type
    
    type TypeConstraint = 
        | Dependency of name: Ident * ty: Type
        | NamedConstraint of name: ResolvedIdent * arguments: Ident list

    and QuantifiedType = QuantifiedType of ty: Type * variables: Ident list *  constraints: TypeConstraint list
    
    let create ty start stop = { Type.ty = ty; Type.range = Range.create start stop }

module Pattern =

    open Type

    type Pattern = 
        { pat: Pattern'
          range: Range }

    and Pattern' =
        | Identifier of value: Ident
        | Discard
        | Tuple of patterns: Pattern list
        | Record of fields: (Ident * Pattern) list
        | Literal of value: Literal

        | Assignment of name: Ident * pattern: Pattern
        | Variant of variant: Type * pattern: Pattern
        
    let create pattern start stop = { Pattern.pat = pattern; Pattern.range = Range.create start stop }

    let rec signature x =
        match x.pat with
        | Identifier _ -> "ident"
        | Discard -> "discard"
        | Tuple xs -> 
            xs
            |> List.map signature
            |> List.fold (fun acc x -> sprintf "%s %s" acc x) "tuple"
            |> sprintf "(%s)"
        // TODO Implement
        | Record(fields) -> failwith "Not Implemented"
        | Literal(value) -> failwith "Not Implemented"
        | Assignment(name, pattern) -> failwith "Not Implemented"
        | Variant(variant, pattern) -> failwith "Not Implemented"

module Expression =

    open Type
    open Pattern

    type Expression = 
        { expr: Expression'
          range: Range }

    and Expression' =
        | Identifier of value: ResolvedIdent
        | Literal of value: Literal
        | TupleConstructor of items: Expression list
        | RecordConstructor of items: (Ident * Expression) list
        | Block of items: Expression list
        | External of name: string * argument: Expression

        | ExplicitType of expr: Expression * ty: QuantifiedType
        | RecordUpdate of expr: Expression * items: (Ident * Expression) list
        | Application of left: Expression * right: Expression

        | Let of name: Ident * value: Expression
        | UnsafeLet of name: Ident * value: Expression
        | UnsafeDo of proc: Expression
        | Conditional of guard: Expression * thenBranch: Expression * elseBranch: Expression
        | Reference of expr: Expression
        | Dereference of expr: Expression
        | Mutate of expr: Expression * value: Expression

        // TODO move to the higher level function generator notion of pattern matching/lambda
        | Lambda of branches: (Pattern * Expression) list

        // TODO get rid of these
        | Function of parameter: Ident * body: Expression
        | Match of expr: Expression * cases: (Pattern * Expression) list

    let create expr start stop = { Expression.expr = expr; Expression.range = Range.create start stop }

    let rec signature expr =
        match expr.expr with 
        | Identifier _ -> "ident"
        | Literal x -> 
            match x with
            | Literal.String _ -> "string"
            | Literal.Integer _ -> "int"
            | Literal.Real _ -> "real"
            | Literal.Character _ -> "char"
            | Literal.Boolean _ -> "bool"
        | TupleConstructor xs -> 
            xs 
            |> List.map signature 
            |> List.fold (fun acc x -> sprintf "%s %s" acc x) "tuple" 
            |> sprintf "(%s)"
        | RecordConstructor xs -> 
            xs 
            |> List.map (fun (_, expr) -> sprintf "(field %s)" (signature expr))
            |> List.fold (fun acc x -> sprintf "%s %s" acc x) "record"
            |> sprintf "(%s)"
        | Block xs ->
            xs 
            |> List.map signature 
            |> List.fold (fun acc x -> sprintf "%s %s" acc x) "block" 
            |> sprintf "(%s)"
        | External (name, arg) -> sprintf "(extern '%s' %s)" name (signature arg)

        | Function (_, expr) -> sprintf "(func %s)" (signature expr)
        | Match (expr, cases) -> 
            cases
            |> List.map (fun (pat, expr) -> sprintf "(case [%s] %s)" (Pattern.signature pat) (signature expr))
            |> List.fold (fun acc x -> sprintf "%s %s" acc x) (sprintf "match [%s]" (signature expr))
            |> sprintf "(%s)"
        // TODO Implement
        | ExplicitType(expr, ty) -> failwith "Not Implemented"
        | RecordUpdate(expr, items) -> failwith "Not Implemented"
        | Application(left, right) -> failwith "Not Implemented"
        | Let(name, value) -> failwith "Not Implemented"
        | Conditional(guard, thenBranch, elseBranch) -> failwith "Not Implemented"
        | Reference(expr) -> failwith "Not Implemented"
        | Mutate(expr, value) -> failwith "Not Implemented"

module Program =

    open Type
    open Expression

    type Declaration = 
        | Import of name: ResolvedIdent * filter: Ident list
        | Definition of name: Ident * ty: QuantifiedType
        | Implementation of name: Ident * expr: Expression
        | Newtype of name: Ident * ty: QuantifiedType
        | Constraint of name: Ident * parameters: Ident list * constraints: TypeConstraint list

    and TopLevel = Module of name: ResolvedIdent * exports: Ident list * members: Declaration list

    and Program = Program of items: TopLevel list
