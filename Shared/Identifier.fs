module Identifier

open ListExtensions

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
        other.final = this.final && this.modules |> List.containsAtStart other.modules

    let print this =
        this.modules
        |> List.fold (fun acc x -> sprintf "%s::%s" x acc) this.final