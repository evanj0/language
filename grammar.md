```ebnf
(* NOTATION *)

(* PascalCase names represent a variant and do not appear in other sequences. 
 * 
 * snake_case names represent a sequence that appears in other sequences. 
 * 
 * Since the grammar defined in this document also serves as a model for the concrete 
 * syntax tree, there are many trivial assignments (i.e, `SubExpr = sub_expr`) that 
 * are intended to show what is complete versus what is just a part of another grammar 
 * rule. This has the added benefit of allowing further expansion at a specific 
 * precedence level without requiring too many changes to the grammar. 
 *)

(* SHARED *)

(* reserved *)

reserved_word = "if" | "then" | "else" | "ref" | "mut" | "let" | "true" | "false" | "with" | "->" | "<-" | "module" ;

(* spacing *)

space = single_space, { single_space } ;
opt_space = { single_space } ;

single_space = 
    | Whitespace
    | Newline
    | Tab
    | Comment ;
Whitespace = '\u0020' ;
Newline = '\n' | '\r' | '\r\n' ;
Tab = '\t' ;
Comment = '(*', { ? any character ? }, '*)' ;

(* literals *)

string_literal = '"', { escaped_character }, '"' ;

character_literal = "'", escaped_character, "'" ;

escaped_character =
    | '\"'
    | '\n'
    | '\t'
    | '\\'
    | '\u', hex_digit, hex_digit, hex_digit, hex_digit 
    | ? any character \ ? ;

integer_literal = digit_sequence, [ negative_suffix ] ;

real_literal = digit_sequence, '.', digit_sequence, [ negative_suffix ] ;
Digit = ? 0..9 ? ;
literal_digit = ? 0..9 or _ ? ;
digit_sequence = Digit, { literal_digit } ;
negative_suffix = 
    | UppercaseNegativeSuffix
    | LowercaseNegativeSuffix ;
UppercaseNegativeSuffix = 'N' ;
LowercaseNegativeSuffix = 'n' ;

boolean_literal = 'true' | 'false' ;

(* identifiers *)

ident_char = 
    ? any of abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!$%^&*-_=+<>?/1234567890' ? ;
ident_start_char = 
    ? any of abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!$%^&*-_=+<>?/ ? ;
ident = ident_start_char, { ident_char } ;

(* <, >, and & should not appear in type names as they often appear next to types *)
typename_char = 
    ? any of abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890' ? ;
typename_start_char = 
    ? any of abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_ ? ;
typename = typename_start_char, { typename_char } ; 

resolved_ident = { typename, '::' }, ident ;

resolved_typename = { typename, '::' }, typename ;

(* delimiters *)

comma_delim = opt_space, ',', opt_space ;
semicolon_delim = opt_space, ';', opt_space ;

(* TYPES *)

quantified_type = QuantifiedType ;
QuantifiedType = type, [ opt_space, forall, [ opt_space, where ] ] ;
forall = 'forall', space, ident, { comma_delim, ident } ;
where = 'where', opt_space, type_constraint, { comma_delim, type_constraint } ;

type_constraint = 
    | NamedConstraint
    | Constraint ;
NamedConstraint = 
    resolved_typename, opt_space, 
    '(', opt_space, ident, { comma_delim, ident }, opt_space, ')' ;
Constraint = '{', opt_space, constraint_items, opt_space, '}' ;
constraint_items = constraint_item, { comma_delim, constraint_item } ;
constraint_item = ident, opt_space, ':', opt_space, quantified_type ;

type = Type ;
Type = type_u, { opt_space, function } ;

function = 
    | SafeFunction 
    | UnsafeFunction ;
SafeFunction = '->', opt_space, type_u ;
UnsafeFunction = '-!>', opt_space, type_u ;

type_u = Union ;
Union = type_r, { opt_space, '|', opt_space, type_r } ;

type_r = Reference ;
Reference = enclosed_type, opt_space, '&' ;

enclosed_type =
    | Primitive
    | Nominal
    | Variable
    | Unit
    | Parens
    | EmptyRecord
    | Record ;
Primitive = 'String' | 'Integer' | 'Real' | 'Character' | 'Boolean' ;
Nominal = resolved_typename, [ opt_space, arguments ] ;
Variable = "'", typename, [ opt_space, arguments ] ;
arguments = '<', opt_space, type, { comma_delim, type }, opt_space '>' ;
Unit = '(', opt_space, ')' ;
Parens = '(', opt_space, type, { comma_delim, type }, opt_space, ')' ;
EmptyRecord = '{', opt_space, '}' ;
Record = '{', opt_space, record_field, { comma_delim, record_field }, opt_space, '}' ;
record_field = ident, opt_space, ':', opt_space, type ;

(* PATTERNS *)

pattern = 
    | Assignment
    | Variant ;
Assignment = 'let', space, ident, space, '=', opt_space, pattern ;
Variant = enclosed_pattern, { opt_space, ':', opt_space, type } ;

enclosed_pattern = 
    | Ident
    | Discard
    | Unit
    | Parens
    | EmptyRecord
    | Record
    | String
    | Integer
    | Real
    | Character
    | Boolean ;
Ident = ident ;
Discard = '_' ;
Unit = '(', opt_space, ')' ;
Parens = '(', opt_space, pattern, { comma_delim, pattern }, opt_space, ')' ;
EmptyRecord = '{', opt_space, '}' ;
Record = '{', opt_space, field_init, { comma_delim, field_init }, opt_space, '}' ;
field_init = ident, space, '=', opt_space, pattern ;
String = string_literal ;
Integer = integer_literal ;
Real = real_literal ;
Character = character_literal ;
Boolean = boolean_literal ;

(* EXPRESSIONS *)

expr =
    | SubExpr
    | Lambda ;
SubExpr = sub_expr ;
Lambda = lambda_branch, { opt_space, lambda_branch } ;
lambda_branch = 
    '|', opt_space, pattern, { opt_space, pattern }, opt_space 
    '->', opt_space, sub_expr ;

sub_expr = 
    | Let
    | UnsafeLet
    | Conditional
    | Reference
    | Dereference
    | Mutate
    | Ordered ;
Let = 'let', space, ident, space, '=', opt_space, expr ;
UnsafeLet = 'let!', space, ident, space, '=', opt_space, expr ;
Conditional = 
    'if', opt_space, expr, opt_space, 
    'then', opt_space, expr, opt_space, 
    'else', opt_space, expr ;
Reference = 'ref', opt_space, expr ;
Dereference = 'deref' opt_space, expr ;
Mutate = 'mut', opt_space, expr, opt_space, '<-', opt_space, expr ;
Ordered = sub_expr_e;

sub_expr_e = ExplicitType ;
ExplicitType = sub_expr_u, { opt_space, ':', opt_space, quantified_type } ;

sub_expr_u = Update ;
Update = sub_expr_a, { opt_space, 'with', opt_space, record_body } ;

sub_expr_a = Application ;
Application = sub_expr_c, { opt_space, sub_expr_c } ;

sub_expr_c = Chain ;
Chain = atom, { opt_space, '.', opt_space, atom } ;

atom = 
    | Ident
    | Parens
    | Unit
    | Record
    | EmptyRecord
    | List
    | EmptyList
    | Block
    | Extern
    | String
    | Integer
    | Real
    | Character
    | Boolean ;
Ident = resolved_ident ;
Parens = '(', opt_space, expr, { comma_delim, expr }, opt_space, ')' ;
Unit = '(', opt_space, ')' ;
Record = record_body ;
record_body = '{', opt_space, field_init, { comma_delim, field_init }, opt_space, '}' ;
field_init = ident, space, '=', opt_space, expr ;
EmptyRecord = '{', opt_space, '}' ;
List = '[', opt_space, expr, { comma_delim, expr }, opt_space, ']' ;
EmptyList = '[', opt_space, ']' ;
Block = '@{', opt_space, expr, { semicolon_delim, expr }, opt_space, '}' ;
Extern = '[<', opt_space, string_literal, opt_space, expr, opt_space '>]' ;
String = string_literal ;
Integer = integer_literal ;
Real = real_literal ;
Character = character_literal ;
Boolean = boolean_literal ;

(* MODULE *)

file = File ;
File = opt_space, top_level, { opt_space, top_level }, opt_space, ? end of file ? ;

top_level = Module ;
Module = 
    'module', opt_space, resolved_typename, [ opt_space, export_list ], opt_space, 
    '=', opt_space, declaration, { semicolon_delim, declaration } ;
export_list = '(', opt_space, ident, { comma_delim, ident }, opt_space, ')' ;

declaration =
    | Import
    | Definition
    | Binding
    | NewType
    | Constraint ;
Import = 'import', space, resolved_typename, [ opt_space, import_filter ] ;
import_filter = '(', opt_space, ident, { comma_delim, ident }, opt_space, ')' ;
Definition = 'def', space, ident, opt_space, ':', opt_space, quantified_type ;
Binding = 'let', space, ident, space, '=', opt_space, expr ;
NewType = 'type', space, typename, space, '=', opt_space, quantified_type ;
Constraint = 
    'bound', space, typename, opt_space, constraint_params, opt_space, 
    '=', opt_space, type_constraint, { comma_delim, type_constraint } ;
constraint_params = '(', opt_space, typename, { comma_delim, typename }, opt_space, ')' ;
```