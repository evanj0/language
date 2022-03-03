module Identifier

open ListExtensions

/// Ident with modules stored in reverse order.
type Ident =
    {
        final: string
        modules: string list
    }

[<RequireQualifiedAccess>]
module Ident =
    let create (final, modules) = { Ident.final = final; modules = modules }

    /// Creates an identifier from a list of names in parsing order.
    /// Expects `list` to contain at least 1 element.
    let fromList list =
        assert (list |> List.length >= 1)
        list
        |> List.rev
        |> (fun xs -> xs |> List.head, xs |> List.tail)
        |> create

    let fromString string = fromList [string]

    let contains other this =
        this.modules 
        |> List.containsAtStart other.modules
        && other.final = this.final

    let equals other this =
        List.zip this.modules other.modules
        |> List.map (fun (x, y) -> x = y)
        |> List.forall id
        && this.final = other.final

    let print this =
        this.modules
        |> List.fold (fun acc x -> sprintf "%s::%s" x acc) this.final