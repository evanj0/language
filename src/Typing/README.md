# Inference Algorithm

```fsharp
let infer(expr, constrainer, env) - type, constraints =
    let type = 
        match expr with
        | Ident ident -> inst(getType(ident, env)), []
        | Literal String -> String, []
    return type, constrainer(type, env)

let solve(type, )
```