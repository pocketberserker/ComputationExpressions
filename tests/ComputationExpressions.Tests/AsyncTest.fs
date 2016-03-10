namespace ComputationExpressions.Tests

open Persimmon
open UseTestNameByReflection

module AsyncTest =

  let returnI x = async.Return(x)

  open ComputationExpressions

  let runSync (zero: 'T) computation = Async.RunSynchronously(zero) computation

  let zero = test {
    let res = async { () }
    do!
      res |> runSync 0
      |> assertEquals 0
  }

  let ret = test {
    let res = async { return 0 }
    do!
      res |> runSync 0
      |> assertEquals 0
  }

  let retret = test {
    let res = async { return 10; return 20; }
    do!
      res |> runSync 0
      |> assertEquals 10
  }

  let retFrom = test {
    let opt = returnI 1
    let res = async { return! opt; return 0 }
    do!
      res |> runSync 0
      |> assertEquals (opt |> Async.RunSynchronously)
  }

  let letBinding = test {
    let res = async {
      let! a = returnI 10
      return a * 2 |> string
    }
    do!
      res |> runSync 0
      |> assertEquals "20"
  }

  let letBindings = test {
    let res = async {
      let! a = returnI 10
      let! b = returnI 5
      return a + b |> string
    }
    do!
      res |> runSync 0
      |> assertEquals "15"
  }

  let usingBinding = parameterize {
    source [
      (returnI (new Disposable<Async<int>>(returnI 10)), true, "10")
      (returnI (new Disposable<Async<int>>(returnI 20)), true, "20")
    ]
    run (fun (opt, willDisposed, expected) -> test {
      let disposed = ref false
      let res = async {
        use! a = opt
        a.F <- (fun () -> disposed := true)
        let! b = a.Value
        return b |> string
      }
      do!
       res |> runSync 0
       |> assertEquals expected
      do! !disposed |> assertEquals willDisposed
    })
  }

  let tryWith = parameterize {
    source [
      ((fun () -> returnI 10), 10)
      ((fun () -> failwith "oops!": Async<int>), -1)
    ]
    run (fun (f, expected) -> test {
      let res = async {
        try
          let! a = f ()
          return a
        with
          _ -> return -1
      }
      do!
        res |> runSync 0
        |> assertEquals expected
    })
  }

  let tryFinally = parameterize {
    source [
      ((fun () -> returnI 10), Some 10)
      ((fun () -> failwith "oops!": Async<int>),  None)
    ]
    run (fun (f, expected) -> test {
      let final = ref false
      try
        let res = async {
          try
            let! a = f ()
            return Some a
          finally
            final := true
        }
        do!
          res |> runSync 0
          |> assertEquals expected
        do! assertPred !final
      with
        e ->
          do! assertPred !final
          do! assertEquals "oops!" e.Message
    })
  }

  let combine = parameterize {
    source [
      (returnI 11, false, 11)
      (returnI 18, true, 36)
    ]
    run(fun (opt, willEven, expected) -> test {
      let isEven = ref false
      let res = async {
        let! a = opt
        if a % 2 = 0 then
          isEven := true
          return a * 2
        return a
      }
      do!
        res |> runSync 0
        |> assertEquals expected
      do! !isEven |> assertEquals willEven
    })
  }

  let whileLoop = parameterize {
    source [
      (returnI 1, 5, 1)
      (returnI 2, 6, 2)
      (returnI 10, 10, -1)
    ]
    run(fun (opt, expectedCounter, expected) -> test {
      let counter = ref 0
      let res = async {
        let! a = opt
        while (!counter < 5) do
          counter := !counter + a
          if !counter = 10 then
            return -1
        return a
      }
      do!
        res |> runSync 0
        |> assertEquals expected
      do! !counter |> assertEquals expectedCounter
    })
  }

  let forLoop = parameterize {
    source [
      (returnI 1, 5, 1)
      (returnI -1, 3, 0)
    ]
    run (fun (opt, expectedCounter, expected) -> test {
      let counter = ref 0
      let res = async {
        let! a = opt
        for i in 1..5 do
          counter := i
          if a = -1 && i = 3 then
            return 0
        return a
      }
      do!
        res |> runSync -1
        |> assertEquals expected
      do! !counter |> assertEquals expectedCounter
    })
  }
