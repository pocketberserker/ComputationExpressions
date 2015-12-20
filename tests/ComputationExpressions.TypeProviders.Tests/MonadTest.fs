namespace ComputationExpressions.TypeProviders.Tests

open ComputationExpressions.TypeProviders
open ComputationExpressions.Helper
open Persimmon
open UseTestNameByReflection

module MonadTest =

  open System.Reflection

#if DEBUG
  [<Literal>]
  let DllPath = @"..\ComputationExpressions.Helper\bin\Debug\ComputationExpressions.Helper.dll"
#else
  [<Literal>]
  let DllPath = @"..\ComputationExpressions.Helper\bin\Release\ComputationExpressions.Helper.dll"
#endif

  type OptionBuilder = Monad<DllPath, "Option">

  let option = OptionBuilder()

  let ``option monad expression`` = test {
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
