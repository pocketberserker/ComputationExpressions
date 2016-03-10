namespace ComputationExpressions

[<AbstractClass>]
type BaseBuilder() =
  member __.TryWith(f, g) =
    try f () with e -> g e
  member __.TryFinally(f, g) =
    try f () finally g ()
