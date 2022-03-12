namespace TextFormat

module Parse =
    open Instructions
    open FParsec

    let quotedString: Parser<_, unit> = pchar '"' >>. manyCharsTill anyChar (pchar '"')

    let pInstGen name args : Parser<_, unit> =
        pchar '('
        .>>.? spaces
        .>>.? pstring name
        >>. spaces
        >>. (args)
        .>> spaces
        .>> pchar ')'

    let pInst name inst = pInstGen name spaces >>% inst

    let pInstArg name arg inst = pInstGen name arg |>> inst

    let pInstArgs2 name arg1 arg2 inst =
        pInstGen name (arg1 .>> spaces .>>. arg2) |>> inst

    let pNamedArg name p =
        pchar '('
        >>. spaces
        >>. pstring name
        >>. spaces
        >>. (p)
        .>> spaces
        .>> pchar ')'

    let instruction =
        choice [ pInstArg "exit" pint32 Exit
                 pInstArg "label" quotedString Label
                 pInstArg "ip-set" quotedString IpSet
                 pInstArgs2 "proc" quotedString (pNamedArg "args" puint16) Proc
                 pInstArg "call" quotedString Call
                 pInst "return" Return
                 pInstArg "jump-if-false" puint64 JumpIfFalse
                 pInstArg "jump" puint64 Jump
                 pInst "debug.dump" DebugDump

                 pInstArg "i64.push" pint64 I64Push
                 pInstArg "f64.push" pfloat F64Push
                 pInstArg "char.push" (pchar ''' >>. anyChar .>> pchar ''') CharPush
                 pInstArg "bool.push" (pstring "true" >>% true <|> (pstring "false" >>% false)) BoolPush
                 pInstArg "string.push" quotedString StringPush
                 
                 pInstArg "local.arg.load" puint16 LocalArgLoad
                 pInstArg "local.closure-arg.load" puint16 LocalClosureArgLoad
                 pInstArg "local.load" puint16 LocalLoad
                 
                 pInstArg "record.alloc" puint16 RecordAlloc
                 pInstArg "record.get-field" puint16 RecordGetField
                 pInstArg "record.set-field" puint16 RecordSetField
                 
                 pInstArgs2 "closure.alloc" (pNamedArg "proc" quotedString) (pNamedArg "args" puint16) ClosureAlloc
                 pInstArg "closure.set-arg" puint16 ClosureSetArg
                 pInst "closure.apply" ClosureApply
                 
                 pInst "i64.add" I64Add
                 pInst "i64.cmp-eq" I64CmpEq ]

    let comment: Parser<_, unit> =
        pstring ";;"
        .>>. manyCharsTill anyChar (pchar '\n')
        >>% ()

    let spacesAndComments = many (spaces1 <|> skipMany1 comment)

    let code =
        pchar '('
        >>. spaces
        >>. pstring "code"
        >>. spacesAndComments
        >>. many (instruction .>>? spacesAndComments)
        .>> pstring ")"

    let file = spacesAndComments >>. code .>> spacesAndComments

module Assembler = 
    open FParsec

    open ResultExtensions

    let compile string =
        match run Parse.file string with
        | Success(instructions, _, _) -> 
            Instructions.Compile.toVmInstructions instructions
        | Failure(message, _, _) -> Result.Error(message)

    type TextFormatCompilationException(message: string) = inherit System.Exception(message)

    let Compile string =
        let result =
            compile string
            |> Result.map (fun (program, procTable, strings) -> program |> List.toArray, procTable |> List.toArray, strings |> List.toArray)

        match result with
        | Result.Ok x -> x
        | Result.Error message -> raise (TextFormatCompilationException(message))