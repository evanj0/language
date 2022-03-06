module NameGeneration

open ListExtensions

// TODO make this use sprintf for type safety
let generateNameWithAttrs attrs unique =
    let attrs =
        attrs
        |> List.map (fun (k, v) -> sprintf "%s: %s" k v)
        |> List.intercalate "; "
    sprintf "@gen[%s]`%s" attrs unique

let generateNameWithUsage usage unique = generateNameWithAttrs [("usage", usage)] unique

let generateParamName unique = generateNameWithUsage "param" unique

