﻿module ResultExtensions

type ResultBuilder() = class
    member _.Return(x) = Ok x
    member _.Bind(m, f) = Result.bind f m
    member _.Zero() = Ok ()
    member _.ReturnFrom(x: Result<_,_>) = x
end

let result = ResultBuilder()

[<RequireQualifiedAccess>]
module Result =
    let rec private collect' (state: Result<'a list, 'e>) (xs: Result<'a, 'e> list) =
        match xs with
        | [] -> state
        | x::xs ->
            match x with
            | Ok el -> 
                state 
                |> Result.bind 
                    (fun state -> 
                        collect' (Ok (state @ [el])) xs)
            | Error e -> Error e

    let collect xs = collect' (Ok []) xs

    let fromOption errorValue option =
        match option with
        | Some x -> Ok x
        | None -> Error errorValue