namespace ComputationExpressions

[<AbstractClass>]
type BaseBuilder() =
  abstract member TryWith: (unit -> 'T) * (exn -> 'T) ->'T
  default __.TryWith(f, g) =
    try f () with e -> g e
  abstract member TryFinally: (unit -> 'T) * (unit -> unit) ->'T
  default __.TryFinally(f, g) =
    try f () finally g ()
