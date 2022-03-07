# language-alpha

## todo
- [ ] Finish implementing `quantifiedType` in Cst2Ast.fs
- [ ] Create lowering module to convert AST to IR
  - [ ] Type compiler (resolves typenames based on imported modues)
  - [ ] Representation of module scope (probably want to flatten module structure, but strip module names from functions when importing so that overloading works)
- [ ] Finish testing of type system
- [ ] Create typed IR generator module (need to figure out how to get type information for function names inside of expressions)
- [ ] Add file information to `Range` so that error messages can show file where the error originates
