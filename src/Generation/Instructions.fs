module Instructions

type Inst =
    | Exit of statusCode: int32
    | Label of name: string
    | IpSet of label: string
    | Proc of name: string * args: uint16
    | Call of proc: string
    | Return
    | JumpIfFalse of number: uint64
    | Jump of number: uint64
    | DebugDump

    | I64Push of int64
    | F64Push of float
    | CharPush of char
    | BoolPush of bool
    | StringPush of string
    
    | LocalArgLoad of index: uint16
    | LocalClosureArgLoad of index: uint16
    | LocalLoad of index: uint16

    | RecordAlloc of numFields: uint16
    | RecordGetField of field: uint16
    | RecordSetField of field: uint16

    | ClosureAlloc of proc: string * numArgs: uint16
    | ClosureSetArg of arg: uint16
    | ClosureApply

    | I64Add
    | I64CmpEq

module Compile =
    open ResultExtensions

    type Op = Interpreter.Op
    type Code = Interpreter.OpCode
    type Word = Interpreter.Word
    type ProcInfo = Interpreter.ProcInfo

    type ProcDescriptor =
        { addr: uint32
          numArgs: uint16 }

    let toVmInstructions (instructions: Inst list) =
        let instructions = List.indexed instructions 
        
        let procs = 
            instructions
            |> List.fold (fun procs (addr, instruction) ->
                match instruction with 
                | Proc(name, args) -> (name, ProcInfo(addr, int args)) :: procs
                | _ -> procs)
                []

        let procMap = 
            procs
            |> List.indexed
            |> List.map (fun (index, (name, _proc)) -> name, index)
            |> Map

        let procTable = 
            procs
            |> List.indexed
            |> List.map (fun (index, (_name, proc)) -> proc)

        let labels = 
            instructions
            |> List.fold (fun state (index, instruction) ->
                match instruction with
                | Label name -> (name, index) :: state
                | _ -> state)
                []
            |> Map

        let strings =
            instructions
            |> List.fold (fun strings (_, instruction) ->
                match instruction with
                | StringPush string -> string :: strings
                | _ -> strings)
                []
        
        let stringMap =
            strings
            |> List.indexed
            |> List.map (fun (index, string) -> string, index)
            |> Map
            
        let tryFindAddr label =
            labels
            |> Map.tryFind label
            |> Result.fromOption (sprintf "Couldn't find label `%s`." label)

        let tryFindProc name =
            procMap
            |> Map.tryFind name
            |> Result.fromOption (sprintf "Couldn't find procedure `%s`." name)

        let mapping instruction =
            match instruction with
            | Exit exitCode -> Op(Code.Exit, Word.FromI32(exitCode)) |> Ok
            | Label _ -> Op(Code.NoOp) |> Ok
            | Proc _ -> Op(Code.NoOp) |> Ok
            | IpSet label -> 
                tryFindAddr label
                |> Result.map (fun index -> Op(Code.IpSet, Word.Ptr(uint64 index)))
            | Call proc -> 
                tryFindProc proc
                |> Result.map (fun index -> Op(Code.Call, Word.Ptr(uint64 index)))
            | Return -> Op(Code.Return) |> Ok
            | JumpIfFalse num -> Op(Code.JumpIfFalse, Word.Ptr(uint64 num)) |> Ok
            | Jump num -> Op(Code.Jump, Word.Ptr(uint64 num)) |> Ok
            | DebugDump -> Op(Code.DebugDump) |> Ok

            | I64Push value -> Op(Code.I64Push, Word.FromI64(value)) |> Ok
            | BoolPush value -> Op(Code.BoolPush, Word.FromBool(value)) |> Ok

            | LocalArgLoad index -> Op(Code.LocalArgLoad, Word.Ptr(uint64 index)) |> Ok
            | LocalClosureArgLoad index -> Op(Code.LocalClosureArgLoad, Word.Ptr(uint64 index)) |> Ok
            | LocalLoad index -> Op(Code.LocalLoad, Word.Ptr(uint64 index)) |> Ok
            
            | RecordAlloc numFields -> Op(Code.RecordAlloc, Word.Ptr(uint64 numFields)) |> Ok
            | RecordGetField index -> Op(Code.RecordGetField, Word.Ptr(uint64 index)) |> Ok
            | RecordSetField index -> Op(Code.RecordSetField, Word.Ptr(uint64 index)) |> Ok

            | ClosureAlloc(proc, args) -> 
                tryFindProc proc
                |> Result.map (fun index -> Op(Code.ClosureAlloc, Word().SetU32(0, uint32 index).SetU16(4, args)))
            | ClosureSetArg arg -> Op(Code.ClosureSetArg, Word.Ptr(uint64 arg)) |> Ok
            | ClosureApply -> Op(Code.ClosureApply) |> Ok

            | I64Add -> Op(Code.I64Add) |> Ok
            | I64CmpEq -> Op(Code.I64CmpEq) |> Ok

        instructions
        |> List.map (fun (_, instruction) -> mapping instruction)
        |> Result.collect
        |> Result.map (fun program -> program, procTable, strings)

