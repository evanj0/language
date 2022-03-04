module ListExtensions

[<RequireQualifiedAccess>]
module List =
    let rec containsAtStart list this =
        match (this, list) with
        | ([], []) -> true
        | ([], _::_) -> false // shorter than other list
        | (_::_, []) -> true // longer than other list
        | (x::xs, y::ys) -> x = y && (xs |> containsAtStart ys)

    let isNotEmpty (list: _ list) = not list.IsEmpty

    let rec filterMap mapping list =
        match list with
        | x::xs -> 
            match mapping x with
            | Some x -> x :: filterMap mapping xs
            | _ -> filterMap mapping xs
        | [] -> []

    let rec flatMap mapping list = list |> List.map mapping |> List.concat