namespace ComputationExpressions

open System
open System.ComponentModel
open System.Threading
open System.Threading.Tasks

type AsyncBuilder() =
  inherit BaseBuilder()
  member __.ReturnFrom(x) =
    async { return fun _ -> x }
  member __.Return(x) = async { return fun _ -> async { return x } }
  [<EditorBrowsable(EditorBrowsableState.Never)>]
  member __.Zero() = async { return fun k -> k Unchecked.defaultof<'T> }
  member __.Bind(x, f) =
    async {
      let! x = x
      return! f x
    }
  member __.Source(f: Async<('T -> Async<'T>) -> Async<'U>> * RequireZero) = async {
    let! f = fst f
    return! f async.Return
  }
  member __.Source(x: Async<'T>) = x
  member __.Source(xs: #seq<_>) = async.Return(xs)
  member __.Using(x: #IDisposable, f) = async.Using(x, f)
  member __.Combine(x: Async<(_ -> _) -> _>, rest: unit -> Async<(_ -> _) -> _>) =
    async {
      let! f = x
      return fun k -> f (fun _ -> async {
        let! g = rest ()
        return! g k
      })
    }
  member this.While(guard, f) =
    if guard () then
      this.Combine(f (), fun () -> this.While(guard, f))
    else this.Zero()
  member this.For(xs: Async<#seq<_>>, f) = async {
    let! xs = xs
    return! this.Using(
      xs.GetEnumerator(),
      fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))
  }
  member __.Delay(f) = f
  member __.Run(f: unit -> Async<('T -> Async<'T>) -> 'U>) =
    (f (), RequireZero)

type AsyncOptionBuilder() =
  inherit BaseBuilder()
  member __.ReturnFrom(x: Async<_ option>) =
    async { return fun _ -> x }
  member __.Return(x) = async { return fun _ -> async { return Some x } }
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
      return fun k -> f (fun _ -> async {
        let! g = rest ()
        return! g k
      })
    }
  member this.While(guard, f) =
    if guard () then
      this.Combine(f (), fun () -> this.While(guard, f))
    else this.Zero()
  member this.For(xs: #seq<_>, f) =
    this.Using(
      xs.GetEnumerator(),
      fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))
  member __.Delay(f) = f
  member __.Run(f: unit -> Async<(_ option -> _) -> _>) =
    async {
      let! f = f ()
      return! f async.Return
    }

[<AutoOpen>]
module AsyncSyntax =

  let private unsafe computation =  async {
    let! computation = computation
    return! computation async.Return
  }

  module Unsafe =

    type Async with
      static member UnsafeRunSynchronously((computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero), ?timeout: int, ?cancellationToken: CancellationToken) =
        Async.RunSynchronously(unsafe computation, ?timeout = timeout, ?cancellationToken = cancellationToken)
      static member UnsafeStart((computation: Async<('T -> Async<'T>) -> Async<unit>>, RequireZero), ?cancellationToken: CancellationToken) =
        Async.Start(unsafe computation, ?cancellationToken = cancellationToken)
      static member UnsafeStartAsTask((computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero), ?taskCreationOptions: TaskCreationOptions, ?cancellationToken: CancellationToken) =
        Async.StartAsTask(unsafe computation, ?taskCreationOptions = taskCreationOptions, ?cancellationToken = cancellationToken)
      static member UnsafeStartImmediate((computation: Async<('T -> Async<'T>) -> Async<unit>>, RequireZero), ?cancellationToken: CancellationToken) =
        Async.StartImmediate(unsafe computation, ?cancellationToken = cancellationToken)
      static member UnsafeStartWithContinuations
        (
          (computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero),
          continuation, exceptionContinuation, cancellationContinuation, ?cancellationToken: CancellationToken) =
            let c = unsafe computation
            Async.StartWithContinuations(c, continuation, exceptionContinuation, cancellationContinuation, ?cancellationToken = cancellationToken)
      static member UnsafeStartChild((computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero), ?millisecondsTimeout: int) =
        Async.StartChild(unsafe computation, ?millisecondsTimeout = millisecondsTimeout)
      static member UnsafeStartChildAsTask((computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero), ?taskCreationOptions: TaskCreationOptions) =
        Async.StartChildAsTask(unsafe computation, ?taskCreationOptions = taskCreationOptions)

  let inline private asyncWithDefault zero computation =
    async {
      let! f = computation
      return! f (fun _ -> async.Return(zero))
    }

  type Async with
    static member ApplyZero(zero: 'T) =
      fun (computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero) ->
        asyncWithDefault zero computation
    static member RunSynchronously(zero: 'T, ?timeout: int, ?cancellationToken: CancellationToken) =
      fun (computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero) ->
        Async.RunSynchronously(asyncWithDefault zero computation, ?timeout = timeout, ?cancellationToken = cancellationToken)
    static member Start(zero: 'T, ?cancellationToken: CancellationToken) =
      fun (computation: Async<('T -> Async<'T>) -> Async<unit>>, RequireZero) ->
        Async.Start(asyncWithDefault zero computation, ?cancellationToken = cancellationToken)
    static member StartAsTask(zero: 'T, ?taskCreationOptions: TaskCreationOptions, ?cancellationToken: CancellationToken) =
      fun (computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero) ->
        Async.StartAsTask(asyncWithDefault zero computation, ?taskCreationOptions = taskCreationOptions, ?cancellationToken = cancellationToken)
    static member StartImmediate(zero: 'T, ?cancellationToken: CancellationToken) =
      fun (computation: Async<('T -> Async<'T>) -> Async<unit>>, RequireZero) ->
        Async.StartImmediate(asyncWithDefault zero computation, ?cancellationToken = cancellationToken)
    static member StartWithContinuations(zero: 'T, continuation, exceptionContinuation, cancellationContinuation, ?cancellationToken: CancellationToken) =
      fun (computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero) ->
        let c = asyncWithDefault zero computation
        Async.StartWithContinuations(c, continuation, exceptionContinuation, cancellationContinuation, ?cancellationToken = cancellationToken)
    static member Start(zero: 'T, ?millisecondsTimeout: int) =
      fun (computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero) ->
        Async.StartChild(asyncWithDefault zero computation, ?millisecondsTimeout = millisecondsTimeout)
    static member StartChildAsTask(zero: 'T, ?taskCreationOptions: TaskCreationOptions) =
      fun (computation: Async<('T -> Async<'T>) -> Async<'U>>, RequireZero) ->
        Async.StartChildAsTask(asyncWithDefault zero computation, ?taskCreationOptions = taskCreationOptions)

  let async = AsyncBuilder()

  let asyncOption = AsyncOptionBuilder()
