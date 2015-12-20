module ComputationExpressions.TypeProviders.Loader

open System
open System.IO
open System.Reflection
open FSharp.Reflection
open Microsoft.FSharp.Core.CompilerServices

let private publicTypes (asm: Assembly) =
  asm.GetTypes()
  |> Seq.filter (fun typ -> typ.IsPublic)

let private publicNestedTypes (typ: Type) =
  typ.GetNestedTypes()
  |> Seq.filter (fun typ -> typ.IsNestedPublic)

let private collectModules (asm: Assembly) =
  seq {
    let tops = publicTypes asm
    yield! tops
    yield! tops |> Seq.collect publicNestedTypes
  }
  |> Seq.filter FSharpType.IsModule

let findModule (name: string) (asm: Assembly) =
  collectModules asm
  |> Seq.find (fun t -> t.Name = name || t.Name = name + "Module")

let findMethodsAndProperties (name: string) (asm: Assembly) =
  let t = findModule name asm
  let methods = t.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
  let properties = t.GetProperties(BindingFlags.Static ||| BindingFlags.Public)
  (methods, properties)

let private findFile (cfg: TypeProviderConfig) fileName =
  if Path.IsPathRooted(fileName) then 
    fileName 
  else 
    Path.Combine(cfg.ResolutionFolder, fileName)

let loadAssembly cfg name =
  let assemblyRef = AssemblyName.GetAssemblyName(findFile cfg name)
  Assembly.Load(assemblyRef)
