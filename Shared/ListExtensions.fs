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