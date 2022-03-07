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

    let intercalate (sep: string) (list: string list) = System.String.Join(sep, list)

    /// Runs `comparer` on an element from `list1` and an element from `list2`.
    /// Returns `false` if the lists are of different sizes. 
    let unorderedCmp (comparer: 'a -> 'b -> bool) (list1: 'a list) (list2: 'b list) : bool =
        if list1.Length = list2.Length then
            list1
            |> List.map (fun a ->
                list2
                |> List.tryFind (fun b -> comparer a b)
                |> Option.isSome)
            |> List.forall id
        else 
            false
    
    /// Runs `comparer` on an element from `list1` and an element from `list2`.
    let containsUnordered (comparer: 'a -> 'b -> bool) (list1: 'a list) (list2: 'b list) : bool =
        list1
            |> List.map (fun a ->
                list2
                |> List.tryFind (fun b -> comparer a b)
                |> Option.isSome)
            |> List.forall id