module Instructions

type Inst =
    | Exit of statusCode: int32
    | Label of name: string
    | IpSet of label: string
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

    type ProcDescriptor =
        { label: string
          numArgs: uint16 }

    type Data = 
        { procs: (string * ProcDescriptor) list }

    let toVmInstructions (data: Data) (instructions: Inst list) =
        let procs = 
            data.procs 
            |> List.indexed 
            |> List.map (fun (index, (name, desc)) -> index, name, desc)
        
        let labels = 
            instructions
            |> List.indexed
            |> List.fold (fun state (index, instruction) ->
                match instruction with
                | Label name -> (name, index) :: state
                | _ -> state)
                []
            |> Map
            
        let getIndex name =
            labels
            |> Map.tryFind name
            |> Result.fromOption ()

        let mapping instruction =
            match instruction with
            | Exit exitCode -> Op(Code.Exit, Word.FromI32(exitCode)) |> Ok
            | Label _ -> Op(Code.NoOp) |> Ok
            | IpSet label -> 
                getIndex label
                |> Result.map (fun index -> Op(Code.IpSet, Word.Ptr(getIndex label)))
