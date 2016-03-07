#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[1] - find maximum of given function in interval [-5;5]

let fx (x: float) = 
    3.0 * (R.dnorm(x, mean = -2, sd = 0.2).AsNumeric() |> Seq.head) +
    6.0 * (R.dnorm(x, mean = 1, sd = 0.5).AsNumeric() |> Seq.head) +
    (R.dnorm(x, mean = 3, sd = 0.3).AsNumeric() |> Seq.head)
let xs = [-5.0 .. 0.05 .. 5.0]
namedParams [
    "x", box xs
    "y", box <| Seq.map fx xs
    "type", box "l"
]
|> R.plot

//[1.1] - using uniform distribution
//sampling 100 random uniformly dist values and running them through the given function
let uniform_sample = 
    R.runif(100, min = -5, max = 5).AsNumeric() 
    |> List.ofSeq
    |> List.map (fun x -> x, fx x)

let maxu = uniform_sample |> List.maxBy snd 

uniform_sample 
|> List.unzip
||> fun X Y ->
    namedParams [
        "x", box X
        "y", box Y
    ]
|> R.plot

//[1.2] - using a mixture distribution
//the mixture distribution is created by sampling from three normal dists
let mixture_dist_sample =
    [R.rnorm(33, mean = -2, sd = 0.2).AsNumeric()
     R.rnorm(33, mean = 1, sd = 0.5).AsNumeric()
     R.rnorm(33, mean = 3, sd = 0.3).AsNumeric()]
    |> Seq.concat
    |> Seq.map (fun x -> x, fx x)

let maxm = mixture_dist_sample |> Seq.maxBy snd

mixture_dist_sample
|> List.ofSeq
|> List.unzip
||> fun X Y ->
    namedParams [
        "x", box X
        "y", box Y
    ]
|> R.plot

//[1.3] - Compare your result to the output of the R function optimize
let r_optimize = 
    R.eval(R.parse(text="optimise(function(x) {3*dnorm(x,m = -2,sd = 0.2) + 6*dnorm(x,m = 1,sd = 0.5) + 1*dnorm(x,m = 3,sd = 0.3)}, lower = -5, upper = 5, maximum = TRUE)"))
     .AsList()
     .["objective"]
     .AsNumeric() 
    |> Seq.head
