# Lambda Calculus

This is a lambda-calculus reducer, based on implementations in the TAPL book.

The commands include:
- `order <order>`, where `<order> is one of `normal`, `cbn`, cbv`.
- `step <n>`, where `<n>` is a number or `*`.
- `trace <switch>`, where `<switch>` is `on` or `off`.
- `<expression>` where `<expression>` is a lambda-calculus expression such as `(lambda x.x) (lambda y.y)`.
- `<name> = <expression>`
