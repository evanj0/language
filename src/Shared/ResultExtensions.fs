module ResultExtensions

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

    /// Combines a list of results into a single result. Stops at the first `Error` value.
    /// `Result.collect []` returns `Ok ([])`.
    let collect xs = collect' (Ok []) xs

    /// Same as `xs |> Result.collect |> Result.map List.concat`.
    let collectConcatenated xs = xs |> collect |> Result.map List.concat

    let fromOption errorValue option =
        match option with
        | Some x -> Ok x
        | None -> Error errorValue

    let isOk this = match this with Ok _ -> true | Error _ -> false

    let isError this = not (isOk this)

    let getOkValue result = 
        match result with 
        | Ok x -> Some x 
        | _ -> None

    let getErrorValue result =
        match result with
        | Ok _ -> None
        | Error e -> Some e