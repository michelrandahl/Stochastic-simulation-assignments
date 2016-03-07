#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[2] - Simulate a sample from a Beta(2,5) distribution using the rejection method with a uniform distribution as auxiliary distribution

let trials = 10000
//list of random numbers to take from
let U1 = R.runif(trials).AsNumeric()
let U2 = R.runif(trials).AsNumeric()
//defining accept function for the rejection process
let accept_function scaling_factor (u1, u2) = 
    let beta_res = 
        R.dbeta(u1,shape1=2,shape2=5).AsNumeric()
        |> Seq.head
    scaling_factor*u2 < beta_res

//creating a sample rbeta with the rejection funciton
let rbeta =
    Seq.zip U1 U2
    |> Seq.filter (accept_function 2.5)
    |> Seq.map fst

let accepted = 100.0 * float(rbeta |> Seq.length) / float trials
namedParams [
    "x", box rbeta
    "main", box (sprintf "beta(2,5) from uniform dist (%A%% accepted)" accepted)
    "breaks", box 50
    "probability", box true
]
|> R.hist

let xs = [0.0 .. 0.01 .. 1.0]
let beta_2_5_fun = 
    xs
    |> List.map (fun x -> Seq.head(R.dbeta(x, 2, 5).AsNumeric())) 
namedParams [
    "x", box xs
    "y",box beta_2_5_fun
    "col", box "red"
]
|> R.lines

namedParams [
    "x", box rbeta
    "y", box "pbeta"
    "shape1",box 2
    "shape2",box 5
]
|> R.ks_test
|> fun r -> r.Print() // we get a fairly high p-value and thus reject the null hypothesis that the generated numbers doesn't follow a beta dist
