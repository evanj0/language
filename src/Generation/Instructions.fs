module Instructions

[<RequireQualifiedAccess>]
module Inst =
    type Inst =
        | Exit of code: int
        | Block of instructions: Inst list
        
        | SkipIfFalse


type Inst = Inst.Inst