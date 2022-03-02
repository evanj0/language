module Instructions

[<RequireQualifiedAccess>]
module Inst =
    type Inst =
        | Exit of code: int

type Inst = Inst.Inst