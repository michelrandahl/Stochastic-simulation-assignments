#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[3] - Simulate a sample from an N(0,1) distribution with the rejection method using the double exponential distribution 

let trials = 10000
//sampling random data uniformly for 3 standard deviations
//3 standard deviations account for 99.7%
let U1 = R.runif(trials,min = -3,max = 3).AsNumeric()
//sampling random data from the exponential distribution
let E = R.rexp(trials,1).AsNumeric()
         |> List.ofSeq


let accept_function scaling_factor (u1,u2) =
    let norm_res = 
        R.dnorm(u1,mean=0,sd=1).AsNumeric()
        |> Seq.head
    scaling_factor*u2 < norm_res

//generating the N(0,1) distributed data using the rejection method and double exponential function as auxiliary funciton
let rnorm =
    Seq.zip U1 E
    |> Seq.filter (accept_function 1.0)
    |> Seq.map fst

let accepted = 100.0 * float(rnorm |> Seq.length) / float trials
namedParams [
    "x", box rnorm
    "main", box (sprintf "%A%% accepted" accepted)
    "breaks", box 25
    "probability", box true
]
|> R.hist

let xs = [-5.0 .. 0.1 .. 5.0]
let norm_0_1_fun = 
    xs
    |> List.map (fun x -> Seq.head(R.dnorm(x, 0, 1).AsNumeric())) 
namedParams [
    "x", box xs
    "y",box norm_0_1_fun
    "col", box "red"
]
|> R.lines

//performing statistical significance check on the resulting dist to see if it is correct
let ks_res = 
    namedParams [
        "x", box rnorm
        "y", box "pnorm"
        "mean",box 0
        "sd",box 1
    ]
    |> R.ks_test
    |> fun r -> match r.AsList().["p.value"].Value with
                | :? array<float> as a -> a
                | _ -> [|0.0|]
    |> Seq.head

