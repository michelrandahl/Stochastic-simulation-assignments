#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[6] - Variance in the estimation of the median of a Poisson distribution

//Estimate the theoretical median from your sample 
let n = 200.0
let rpois = R.rpois(n,10).AsNumeric() 
let median_val1 = R.median(Array.ofSeq rpois).Print()

//Implement the bootstrap estimation technique to evaluate the variance of your estimator 
//sampling 500 times from the previously random poision distributed sample and calculating the mean and median
let bootstrapped =
    [|0 .. 500|]
    |> Array.map 
        (fun _ -> 
            namedParams [
                "x", box rpois
                "size", box n
                "replace", box true
            ]
            |> R.sample
            |> fun r -> r.AsNumeric()
            |> Array.ofSeq)

let bootstrapped_medians =
    bootstrapped
    |> Array.map (fun xs -> R.median(xs).AsNumeric()
                            |> Seq.head)

R.mean(bootstrapped_medians).Print()
//estimating the variance of the 200 value sample median
let variance =
    R.sd(bootstrapped_medians).AsNumeric() 
    |> Seq.head
    |> fun x -> x**2.0