module ComputationExpressions.TypeProviders.Generator

open System
open System.Threading
open System.Reflection
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes

[<RequireQualifiedAccess>]
module ModuleLiteral =

  [<Literal>]
  let Point = "point"

  [<Literal>]
  let Bind = "bind"

  [<Literal>]
  let Id = "Identity"

  [<Literal>]
  let Operators = "Operators"

[<RequireQualifiedAccess>]
module MethodName =

  [<Literal>]
  let Return = "Return"

  [<Literal>]
  let Bind = "Bind"

  [<Literal>]
  let ReturnFrom = "ReturnFrom"

let private (|MethodCall|) = function
| x::xs -> (x, xs)
| _ -> failwith "parameters mismatch"

let private (|Single|Pair|) = function
| [x] -> Single x
| [x; y] -> Pair(x, y)
| _ -> failwith "parameters mismatch"

let private getCompiledName name =
  let cultureInfo = Thread.CurrentThread.CurrentCulture
  let textInfo = cultureInfo.TextInfo
  textInfo.ToTitleCase(name)

let private operatorsModule =
  typedefof<_ list>.Assembly
  |> Loader.findModule ModuleLiteral.Operators

let private operators =
  operatorsModule.GetMethods(BindingFlags.Static ||| BindingFlags.Public)

module Monad =

  let private gen originalName methodName (methods: MethodInfo []) =

    let compiledName = getCompiledName originalName
    let info = methods |> Array.find (fun m -> m.Name = originalName || m.Name = compiledName)

    let parameters =
      info.GetParameters()
      |> Array.rev
      |> Array.map (fun x -> ProvidedParameter(x.Name, x.ParameterType))
      |> Array.toList
    ProvidedMethod(methodName, parameters, info.ReturnType, IsStaticMethod = false,
      InvokeCode = function MethodCall(_, args) -> Expr.Call(info, args |> List.rev))

  let genBind methods = gen ModuleLiteral.Bind MethodName.Bind methods
  let genReturn methods = gen ModuleLiteral.Point MethodName.Return methods

  let genReturnFrom () =
    operators |> gen ModuleLiteral.Id MethodName.ReturnFrom

module FlowControl =

  open FSharp.Reflection

  let private composeRight =
    operators |> Array.find (fun m -> m.Name = "op_ComposeRight")

  let private gen originalName methodName (methods: MethodInfo []) =

    let compiledName = getCompiledName originalName
    let info = methods |> Array.find (fun m -> m.Name = originalName || m.Name = compiledName)

    let retType = FSharpType.MakeTupleType([| info.ReturnType; typeof<FlowControl> |])

    let mutable pt: Type option = None

    let parameters =
      info.GetParameters()
      |> Array.mapi (fun i x ->
        if i = 0 && FSharpType.IsFunction(x.ParameterType) then
          let m = x.ParameterType.GetMethod("Invoke")
          let p =
            let p = m.GetParameters() |> Array.head
            p.ParameterType
          pt <- Some p
          let t = FSharpType.MakeFunctionType(p, retType)
          ProvidedParameter(x.Name, t)
        else ProvidedParameter(x.Name, x.ParameterType)
      )
      |> Array.rev
      |> Array.toList

    let inner = function
    | MethodCall(_, Single x) -> Expr.NewTuple([ Expr.Call(info, [x]); <@@ Break @@> ])
    | MethodCall(_, Pair(x, f)) ->
      let pt =
        match pt with
        | Some p -> p
        | None -> failwithf "not found FSharpFunc<'T1, 'T2> in %s" info.Name
      let f =
        let tuple = Var("tuple", retType)
        let composeRight = composeRight.MakeGenericMethod([| pt; retType; info.ReturnType |])
        Expr.Call(composeRight, [ f; Expr.Lambda(tuple, Expr.TupleGet(Expr.Var(tuple), 0)) ])
      Expr.NewTuple([ Expr.Call(info, [ f; x ]); <@@ Continue @@> ])

    ProvidedMethod(methodName, parameters, retType, IsStaticMethod = false, InvokeCode = inner)

  let genBind methods = gen ModuleLiteral.Bind MethodName.Bind methods
  let genReturn methods = gen ModuleLiteral.Point MethodName.Return methods

  let genReturnFrom () =
    operators |> gen ModuleLiteral.Id MethodName.ReturnFrom

  let genZero (properties: PropertyInfo []) =

    let originalName = "zero"
    let compiledName = getCompiledName originalName
    let info = properties |> Array.find (fun m -> m.Name = originalName || m.Name = compiledName)

    let retType = FSharpType.MakeTupleType([| info.PropertyType; typeof<FlowControl> |])

    let inner = function
    | MethodCall(_, _) -> Expr.NewTuple([ Expr.PropertyGet(info); <@@ Continue @@> ])

    ProvidedMethod("Zero", [], retType, IsStaticMethod = false, InvokeCode = inner)

  let genCombine (methods: MethodInfo []) =

    let originalName = "continuable"
    let compiledName = getCompiledName originalName
    let info = methods |> Array.find (fun m -> m.Name = originalName || m.Name = compiledName)

    let t = info.GetParameters().[0].ParameterType
    let retType = FSharpType.MakeTupleType([| t ; typeof<FlowControl> |])

    let parameters = [
      ProvidedParameter("x", retType)
      ProvidedParameter("rest", FSharpType.MakeFunctionType(typeof<unit>, retType))
    ]

    let breakInfo = FSharpValue.GetUnionFields(Break, typeof<FlowControl>) |> fst

    let inner = function
    | MethodCall(_, Pair(x, rest)) ->
      let v = Var("v", t)
      let c = Var("c", typeof<FlowControl>)
      let cont =
        Expr.IfThenElse(
          Expr.Call(info, [Expr.Var(v)]),
          Expr.Application(rest, <@@ () @@>),
          Expr.NewTuple([ Expr.Var(v); <@@ Break @@>]))
      let flow =
        Expr.IfThenElse(
          Expr.UnionCaseTest(Expr.Var(c), breakInfo),
          Expr.NewTuple([ Expr.Var(v); <@@ Break @@> ]),
          cont)
      Expr.Let(v, Expr.TupleGet(x, 0), Expr.Let(c, Expr.TupleGet(x, 1), flow))
    | exprs -> failwithf "oops!: %A" exprs

    ProvidedMethod("Combine", parameters, retType, IsStaticMethod = false, InvokeCode = inner)

  let genDelay (methods: MethodInfo []) =

    let originalName = "point"
    let compiledName = getCompiledName originalName
    let info = methods |> Array.find (fun m -> m.Name = originalName || m.Name = compiledName)

    let parameter =
      let retType = FSharpType.MakeTupleType([| info.ReturnType; typeof<FlowControl> |])
      let t = FSharpType.MakeFunctionType(typeof<unit>, retType)
      ProvidedParameter("f", t)

    let inner = function
    | MethodCall(_, Single f) -> f
    | exprs -> failwithf "oops!: %A" exprs

    ProvidedMethod("Delay", [parameter], parameter.ParameterType, IsStaticMethod = false, InvokeCode = inner)

  let genRun (methods: MethodInfo []) =

    let originalName = "point"
    let compiledName = getCompiledName originalName
    let info = methods |> Array.find (fun m -> m.Name = originalName || m.Name = compiledName)

    let retType = FSharpType.MakeTupleType([| info.ReturnType; typeof<FlowControl> |])
    let parameter =
      let t = FSharpType.MakeFunctionType(typeof<unit>, retType)
      ProvidedParameter("f", t)

    let inner = function
    | MethodCall(_, Single f) ->
      Expr.TupleGet(Expr.Application(f, <@@ () @@>), 0)
    | exprs -> failwithf "oops!: %A" exprs

    ProvidedMethod("Run", [parameter], info.ReturnType, IsStaticMethod = false, InvokeCode = inner)
