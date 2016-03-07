#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[5] - You want to sell your car. You receive a ﬁrst oﬀer at a price of 12....

//sample n random values from the poisson(10) dist, and divide into offers less than and greater than 12, and then extracting the indexes of the offers less than 12
let n = 1000000.0
let rpois = R.rpois(n,10).AsNumeric()

let avg_watingtime =
    rpois
    |> Seq.mapi (fun i x -> i, x > 12.0) //mapping to tuple of index and true/false
    |> Seq.filter snd
    |> Seq.map fst //extracting indexes
    |> Seq.windowed 2
    |> Seq.map (fun x -> match x with
                         | [|x1;x2|] -> x2 - x1 - 1
                         | _         -> 0)
    |> Seq.map float
    |> Seq.average

