module ComputationExpressions.Helper

module Option =
  let inline point (x: string) = Some x
  let inline bind (f: string -> string option) x = Option.bind f x
  let inline continuable (x: string option) = Option.isSome x
  let zero: string option = None

module List =
  let inline point x = [x]
  let inline bind f xs = List.collect f xs
  let inline continuable xs = not <| List.isEmpty xs
  let zero = []
