module NameGeneration

// TODO make this use sprintf for type safety
let generateNameWithAttrs attrs unique =
    attrs
    |> List.map (fun (k, v) -> $"{k}:{v}")
    |> (fun xs -> System.String.Join(",", xs))
    |> (fun attrs -> $"@gen`{attrs}`{unique}")

let generateNameWithUsage usage unique = generateNameWithAttrs [("usage", usage)] unique

let generateParamName unique = generateNameWithUsage "param_name" unique

