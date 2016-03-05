namespace ComputationExpressions

open System

type AsyncBuilder() =
  member __.ReturnFrom(x) =
    async {
      let! x = x
      return fun _ -> x
    }
  member __.Return(x) = async { return fun _ -> x }
  member this.Bind(x, f) =
    async {
      let! x = x
      return! f x
    }
  member __.Using(x: #IDisposable, f) = async.Using(x, f)
  member __.Combine(x: Async<(_ -> _) -> _>, rest: unit -> Async<(_ -> _) -> _>) =
    async {
      let! f = x
      let! g = rest ()
      return fun k -> f (fun _ -> g k)
    }
  member __.TryWith(f, g) =
    try f () with e -> g e
  member __.TryFinally(f, g) =
    try f () finally g ()
  member __.Delay(f) = f
  member this.Run(f: unit -> Async<(_ -> _) -> _>) =
    async {
      let! f = f ()
      return f id
    }

type AsyncWithZeroBuilder<'T>(zero: 'T) =
  inherit AsyncBuilder()
  member __.Zero() = async { return fun k -> k zero }
  member this.While(guard, f) =
    if guard () then
      this.Combine(f (), fun () -> this.While(guard, f))
    else this.Zero()
//  member this.For(xs: #seq<_>, f) =
//    this.Using(
//      xs.GetEnumerator(),
//      fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))

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

  let async = AsyncBuilder()
  let asyncWithZero zero = AsyncWithZeroBuilder(zero)

  let asyncOption = AsyncOptionBuilder()
