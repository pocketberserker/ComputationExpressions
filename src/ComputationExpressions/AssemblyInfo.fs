namespace System
open System.Reflection
open System.Runtime.InteropServices

[<assembly: AssemblyTitleAttribute("ComputationExpressions")>]
[<assembly: AssemblyDescriptionAttribute("provided computation expressions.")>]
[<assembly: GuidAttribute("a688b2b0-32c7-4ae8-b9ff-ec58e8a3e9ff")>]
[<assembly: AssemblyProductAttribute("ComputationsExpressions")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
[<assembly: AssemblyInformationalVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
