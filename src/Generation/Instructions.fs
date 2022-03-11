namespace Instructions

    type Inst =
        | Exit of code: int
        | Block of instructions: Inst list
        
        | Dump
        
        | PushI64
        | PushF64
        | Pop

        | AddI64

        | SkipIfFalse

        | Local of index: uint64

    type OpCode =
        | Exit = 0
        | Dump = 4
        | PushI64 = 16
        | AddI64 = 24
        | Peek = 32

    type Op = struct
        val Code: OpCode
        val Data: byte[]
        new(code: OpCode, data: byte[]) = { Op.Code = code; Op.Data = data }
    end