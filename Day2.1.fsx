#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[1] - Simulate a sample from a Cauchy(0,1) distribution with the inversion method

(*
calculating the inverse function of cauchy(0,1)
u = 0.5 + Math.Atan(x) / Math.PI
u - 0.5 = Math.Atan(x) / Math.PI
(u - 0.5) * Math.PI = Math.Atan(x) 
Math.Tan( (u - 0.5) * Math.PI) = x
*)

let Nsim = 10000
let breaks = 25000
let U = R.runif(Nsim).AsNumeric()

//the function for inverse cauchy
let cauchy_inv u = (u - 0.5) * Math.PI
                   |> Math.Tan
let X = Seq.map (fun u -> cauchy_inv u) U

namedParams [
    "freq",box false
    "main",box "cauchy from uni"
    "x",box X
    "breaks", box breaks
    "xlim", box [-50; 50]
]
|> R.hist

let cauchy_fun x = 1.0 / (Math.PI * (1.0 + x ** 2.0))
let xs = [-50.0 .. 0.5 .. 50.0]
namedParams [
    "x", box xs
    "y",box <| Seq.map cauchy_fun xs
    "col", box "red"
    "lwd", box 3
]
|> R.lines

namedParams [
    "x", box X
    "y", box "pcauchy"
    "location",box 0
    "scale",box 1
]
|> R.ks_test
|> fun r -> r.Print() // we get a fairly high p-value and thus reject the null hypothesis that the generated numbers doesn't follow a cauchy dist
