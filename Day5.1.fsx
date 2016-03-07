#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[1] - Consider the function h(x) = [cos(50x) + sin(20x)]**2 

let f x = (Math.Cos(50.0 * x) + Math.Sin(20.0 * x))**2.0

//Plot this function on [0,1]
let xs = [0.0 .. 0.01 .. 1.0]
namedParams [
    "x", box xs
    "y", box <| Seq.map f xs
    "type", box "l"
    "xlab", box "x"
    "ylab", box "[cos(50x) + sin(20x)]**2"
]
|> R.plot

//Write an algorithm to compute the integral
let n = 10000
R.runif(n).AsNumeric()
|> Seq.map f
|> Seq.average

//Compare your result to those obtained with the R function integrate()
R.eval(R.parse(text="hx <- function(x) {(cos(x*50) + sin(20*x))**2}"))
//R.integrate(f="hx", lower=0, upper=1).AsList().Names
R.integrate(f="hx", lower=0, upper=1).AsList().["value"].AsNumeric()
|> Seq.head

