#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[2] - Simulate a Markov chain with the Metropolis algorithm with the Cauchy distribution as target dist..
let markov_chain n =
    //target function
    let f (x: float) = 
        R.dcauchy(x, 0, 1).AsNumeric()
        |> Seq.head

    //auxillery function
    let q (x: float) =
        R.dnorm(x, mean=0, sd=1).AsNumeric()
        |> Seq.head

    //transition function using the basic metropolis algorithm
    let transition_fun prev = 
        //candidate value
        let y = R.rnorm(1, mean=0, sd=1).AsNumeric()
                |> Seq.head

        //compute metropolis ratio
        let met_ratio = f y  * q prev / (f prev * q y)

        let accept_prob = Seq.min [1.0; met_ratio]
        //toss unfair coin with accept prob
        let accept = R.rbinom(1, 1, accept_prob).AsNumeric()
                     |> Seq.head
                     |> int
        match accept with
        | 0 -> prev
        | _ -> y

    //recurive loop to perform n' itterations
    let rec loop n' prev = seq {
        match n' with
               //end case
        | 0 -> yield transition_fun prev
               //loop case
        | _ -> let res = transition_fun prev
               yield res
               yield! loop (n' - 1) res
    }

    //initializing the loop
    R.rnorm(1, mean=0, sd=1).AsNumeric()
    |> Seq.head
    |> loop n

let n = 10000
let result = markov_chain n |> List.ofSeq

namedParams [
    "x", box result
    "breaks", box 100
    "main", box "cauchy(0,1) from normal(0,1) using metropolis"
    "probability", box true
]
|> R.hist
let xs = [-5.0 .. 0.1 .. 5.0]
let normal_fun = 
    xs
    |> List.map (fun x -> Seq.head(R.dcauchy(x, 0, 1).AsNumeric())) 
namedParams [
    "x", box xs
    "y",box normal_fun
    "col", box "red"
]
|> R.lines
