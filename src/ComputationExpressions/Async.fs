namespace ComputationExpressions

open System

type AsyncOptionBuilder() =
  member __.ReturnFrom(x: Async<_ option>) =
    async {
      let! x = x
      return fun _ -> x
    }
  member __.Return(x) = async { return fun _ -> Some x }
  member __.Zero() = async { return fun k -> k None }
  member this.Bind(x: Async<_ option>, f) =
    async {
      let! x = x
      return!
        match x with
        | Some x -> f x
        | None -> this.Zero()
    }
  member __.Using(x: #IDisposable, f) = async.Using(x, f)
  member __.Combine(x: Async<(_ option -> _) -> _>, rest: unit -> Async<(_ option -> _) -> _>) =
    async {
      let! f = x
      let! g = rest ()
      return fun k -> f (fun _ -> g k)
    }
  member __.TryWith(f, g) =
    try f () with e -> g e
  member __.TryFinally(f, g) =
    try f () finally g ()
  member this.While(guard, f) =
    if guard () then
      this.Combine(f (), fun () -> this.While(guard, f))
    else this.Zero()
//  member this.For(xs: #seq<_>, f) =
//    this.Using(
//      xs.GetEnumerator(),
//      fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))
  member __.Delay(f) = f
  member __.Run(f: unit -> Async<(_ option -> _) -> _>) =
    async {
      let! g = f ()
      return g id
    }

[<AutoOpen>]
module AsyncSyntax =

  let asyncOption = AsyncOptionBuilder()
