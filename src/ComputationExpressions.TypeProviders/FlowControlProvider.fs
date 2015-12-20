namespace ComputationExpressions.TypeProviders
 
open System
open System.IO
open System.Linq
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

#nowarn "13730"
[<TypeProvider>]
type public FlowControlProvider(cfg: TypeProviderConfig) as this = 
  inherit TypeProviderForNamespaces()

  let asm = Assembly.GetExecutingAssembly()
  let ns = "ComputationExpressions.TypeProviders"
 
  let tempAsm = ProvidedAssembly (Path.ChangeExtension (Path.GetTempFileName(), ".dll"))
  let typ = ProvidedTypeDefinition(asm, ns, "FlowControl", Some typeof<obj>, IsErased = false)
  do
    let thisAssembly = Assembly.GetAssembly(typeof<FlowControlProvider>)
    let path = System.IO.Path.GetDirectoryName(thisAssembly.Location)
    this.RegisterProbingFolder(path)
    tempAsm.AddTypes [typ]
    let parameters = [
      ProvidedStaticParameter("assemblyName", typeof<string>)
      ProvidedStaticParameter("moduleName", typeof<string>)
      ProvidedStaticParameter("includeZero", typeof<bool>)
    ]
    typ.DefineStaticParameters (parameters, fun (typeName: string) (args: obj[]) ->
      let target = string args.[0] |> Loader.loadAssembly cfg
      let methods, properties = Loader.findMethodsAndProperties (string args.[1]) target
      let typ = ProvidedTypeDefinition (asm, ns, typeName, Some typeof<obj>, IsErased = false)
      let ctor = ProvidedConstructor(parameters = [], InvokeCode = fun _ -> <@@ () @@>)
      typ.AddMember(ctor)
      typ.AddMemberDelayed(fun () -> Generator.FlowControl.genBind methods)
      typ.AddMemberDelayed(fun () -> Generator.FlowControl.genReturn methods)
      typ.AddMemberDelayed(fun () -> Generator.FlowControl.genDelay methods)
      typ.AddMemberDelayed(fun () -> Generator.FlowControl.genRun methods)
      if unbox<bool> args.[2] then
        typ.AddMemberDelayed(fun () -> Generator.FlowControl.genZero properties)
      typ.AddMemberDelayed(fun () -> Generator.FlowControl.genCombine methods)
      //typ.AddMemberDelayed(Generator.FlowControl.genReturnFrom)
      tempAsm.AddTypes [typ]
      typ
    )
    this.AddNamespace(ns, [typ])
 
[<assembly:TypeProviderAssembly>]
do ()