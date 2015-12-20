namespace System
open System.Reflection
open System.Runtime.InteropServices

[<assembly: AssemblyTitleAttribute("ComputationExpressions.TypeProviders")>]
[<assembly: AssemblyDescriptionAttribute("provided computation expressions.")>]
[<assembly: GuidAttribute("b32a9d71-ba66-403d-a136-3ca43ab53deb")>]
[<assembly: AssemblyProductAttribute("ComputationsExpressions")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
[<assembly: AssemblyInformationalVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
