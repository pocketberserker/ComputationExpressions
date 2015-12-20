namespace ComputationExpressions.TypeProviders.Tests

open ComputationExpressions.TypeProviders
open ComputationExpressions.Helper
open Persimmon
open UseTestNameByReflection

module FlowControlTest =

  open System.Reflection

#if DEBUG
  [<Literal>]
  let DllPath = @"..\ComputationExpressions.Helper\bin\Debug\ComputationExpressions.Helper.dll"
#else
  [<Literal>]
  let DllPath = @"..\ComputationExpressions.Helper\bin\Release\ComputationExpressions.Helper.dll"
#endif

  type OptionBuilder = FlowControl<DllPath, "Option", true>

  let option = OptionBuilder()

  let ``option flow control expression`` = test {
    let actual = option {
      let! x = Some "1"
      let! y = None
      return x + y
    }
    do! assertEquals None actual
  }

  let ``return value`` = test {
    let actual = option {
      return "1"
    }
    do! assertEquals (Some "1") actual
  }

  let ``break if call return`` = test {
    let actual = option {
      let! x = Some "1"
      if true then return "2"
      return x
    }
    do! assertEquals (Some "2") actual
  }
