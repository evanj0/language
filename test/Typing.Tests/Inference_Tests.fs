module Inference.Tests

open ResultExtensions
open Range
open Ir

open NUnit.Framework

let tryTest expr t =
    result {
        let pos = { Position.column = 0; line = 0; index = 0 }
        let env = { Env.currentRange = Range.create pos pos; globals = []; locals = []; }
        let! resultT = 
            Inference.infer expr { State.index = 0 } (fun t env -> []) env
            |> IResult.toResult
            |> Result.map (fun (_state, t, _cs) -> t)
        return t |> Type.equals resultT
    }

[<Test>]
let test1() = Assert.Fail()