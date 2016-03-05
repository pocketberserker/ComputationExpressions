namespace ComputationExpressions.Tests

open Persimmon
open UseTestNameByReflection
open ComputationExpressions

// based on https://github.com/BasisLib/Basis.Core/blob/f48ed463699ae0235aa58623d0f46c754a6f7326/Basis.Core.Tests/OptionTest.ComputationExpr.fs

module AsyncOptionTest =

  let some x = async.Return(Some x)
  let none<'T> = async.Return(None: 'T option)

  let zero = test {
    let res = asyncOption { () }
    do!
      res |> Async.RunSynchronously
      |> assertEquals None
  }

  let ret = test {
    let res = asyncOption { return 0 }
    do!
      res |> Async.RunSynchronously
      |> assertEquals (Some 0)
  }

  let retret = test {
    let res = asyncOption { return 10; return 20; }
    do!
      res |> Async.RunSynchronously
      |> assertEquals (Some 10)
  }

  let retFrom = parameterize {
    source [
      none
      some 10
    ]
    run (fun opt -> test {
      let res = asyncOption { return! opt; return 0 }
      do!
        res |> Async.RunSynchronously
        |> assertEquals (opt |> Async.RunSynchronously)
    })
  }

  let letBinding = parameterize {
    source [
      (some 10, Some "20")
      (none,    None)
    ]
    run (fun (opt, expected) -> test {
      let res = asyncOption {
        let! a = opt
        return a * 2 |> string
      }
      do!
        res |> Async.RunSynchronously
        |> assertEquals expected
    })
  }

  let letBindings = parameterize {
    source [
      (some 10, some 5, Some "15")
      (some 10, none, None)
      (none, some 5, None)
      (none, none, None)
    ]
    run (fun (opt1, opt2, expected) -> test {
      let res = asyncOption {
        let! a = opt1
        let! b = opt2
        return a + b |> string
      }
      do!
        res |> Async.RunSynchronously
        |> assertEquals expected
    })
  }

  let usingBinding = parameterize {
    source [
      (none, false, None)
      (some (new Disposable<Async<int option>>(none)), true, None)
      (some (new Disposable<Async<int option>>(some 10)), true, Some "10")
      (some (new Disposable<Async<int option>>(some 20)), true, Some "20")
    ]
    run (fun (opt, willDisposed, expected) -> test {
      let disposed = ref false
      let res = asyncOption {
        use! a = opt
        a.F <- (fun () -> disposed := true)
        let! b = a.Value
        return b |> string
      }
      do!
       res |> Async.RunSynchronously
       |> assertEquals expected
      do! !disposed |> assertEquals willDisposed
    })
  }

  let combine = parameterize {
    source [
      (none, false, None)
      (some 11, false, Some 11)
      (some 18, true, Some 36)
    ]
    run(fun (opt, willEven, expected) -> test {
      let isEven = ref false
      let res = asyncOption {
        let! a = opt
        if a % 2 = 0 then
          isEven := true
          return a * 2
        return a
      }
      do!
        res |> Async.RunSynchronously
        |> assertEquals expected
      do! !isEven |> assertEquals willEven
    })
  }

  let tryWith = parameterize {
    source [
      ((fun () -> none), None)
      ((fun () -> some 10), Some 10)
      ((fun () -> failwith "oops!": Async<int option>), Some -1)
    ]
    run (fun (f, expected) -> test {
      let res = asyncOption {
        try
          let! a = f ()
          return a
        with
          _ -> return -1
      }
      do!
        res |> Async.RunSynchronously
        |> assertEquals expected
    })
  }

  type TestBuilder with
    member this.TryWith(f, h) = try f () with e -> h e
    member this.TryFinally(f, g) = try f () finally g ()

  let tryFinally = parameterize {
    source [
      ((fun () -> none), None)
      ((fun () -> some 10), Some 10)
      ((fun () -> failwith "oops!": Async<int option>),  None)
    ]
    run (fun (f, expected) -> test {
      let final = ref false
      try
        let res = asyncOption {
          try
            let! a = f ()
            return a
          finally
            final := true
        }
        do!
          res |> Async.RunSynchronously
          |> assertEquals expected
        do! assertPred !final
      with
        e ->
          do! assertPred !final
          do! assertEquals "oops!" e.Message
    })
  }

  let whileLoop = parameterize {
    source [
      (none, 0, None)
      (some 1, 5, Some 1)
      (some 2, 6, Some 2)
      (some 10, 10, Some -1)
    ]
    run(fun (opt, expectedCounter, expected) -> test {
      let counter = ref 0
      let res = asyncOption {
        let! a = opt
        while (!counter < 5) do
          counter := !counter + a
          if !counter = 10 then
            return -1
        return a
      }
      do!
        res |> Async.RunSynchronously
        |> assertEquals expected
      do! !counter |> assertEquals expectedCounter
    })
  }

//  let forLoop = parameterize {
//    source [
//      (none, 0, None)
//      (some 1, 5, Some 1)
//      (some -1, 3, Some 0)
//    ]
//    run (fun (opt, expectedCounter, expected) -> test {
//      let counter = ref 0
//      let res = asyncOption {
//        let! a = opt
//        for i in 1..5 do
//          counter := i
//          if a = -1 && i = 3 then
//            return 0
//        return a
//      }
//      do!
//        res |> Async.RunSynchronously
//        |> assertEquals expected
//      do! !counter |> assertEquals expectedCounter
//    })
//  }
