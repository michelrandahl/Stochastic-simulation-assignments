#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[6] - Simulate a sample of size 1000 for this distribution using the Gibbs sampler..

let n = 100000
//choosing a rho of 0.7
let rho = 0.7

let gibbs_sampler n rho =
    //gibbs sampling function
    let gibbs_sample x' =
        let y = R.rnorm(1, rho*x', 1.0 - rho**2.0 ).AsNumeric()
                |> Seq.head
                //generate x using y to adjust the mean
        let x = R.rnorm(1, rho*y, 1.0 - rho**2.0 ).AsNumeric()
                |> Seq.head
        x,y

    //recursive loop for the gibbs sampling process
    let rec loop n' x' = seq {
        match n' with
               //end case
        | 0 -> yield gibbs_sample x' 
               //loop case
        | _ -> let res = gibbs_sample x' 
               yield res
               yield! loop (n'-1) (fst res)
    }

    //initialize loop
    R.rnorm(1, 0, 1).AsNumeric()
    |> Seq.head
    |> loop n

let result = gibbs_sampler n rho
             |> List.ofSeq
             |> List.unzip
result
||> fun X Y -> 
    namedParams [
        "x", box X
        "y", box Y
        "main", box "bivariant normal dist with cov of 0.95"
    ]
|> R.plot

//testing the covariance matrix
//if the relationships in the covariance matrix matches the relationships from the original covariance matrix, then we have succeeded the simulation
let cov = 
    result
    ||> fun X Y ->
        array2D [X ; Y]
    |> R.t
    |> fun x -> R.cov(x).AsNumericMatrix().ToArray()

//calculate the rho value
let rho' = (cov.[0,1] + cov.[1,0]) / (cov.[0,0] + cov.[1,1])
