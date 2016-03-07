#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[3] - Same as previous exercise with a Metropolis-Hastings algorithm with Gaussian proposal

//the difference between metropolis-hastings and the basic metropolis algorithms are that the metropolis-hastings algorithm calculates the auxillery function based on the proposed state and the previous state, and that the proposed value is drawn from a distribution with a mean based on the previous accepted value
let markov_chain n =
    //target function
    let f (x: float) = 
        R.dcauchy(x, 0, 1).AsNumeric()
        |> Seq.head

    //auxillery function
    let q (x: float) (y: float) =
        R.dnorm(x, mean=y, sd=1).AsNumeric()
        |> Seq.head

    //transition function using the metropolis-hastings algorithm
    let transition_fun prev = 
        //candidate value
        let y = R.rnorm(1, mean=prev, sd=1).AsNumeric()
                |> Seq.head

        //compute metropolis ratio
        let met_ratio = f y  * q prev y / (f prev * q y prev)

        let accept_prob = Seq.min [1.0; met_ratio]
        //toss unfair coin with accept prob
        let accept = R.rbinom(1, 1, accept_prob).AsNumeric()
                     |> Seq.head
                     |> int
        match accept with
        | 0 -> prev
        | _ -> y

    //recursive loop to perform n iterations in the markov chain
    let rec loop n' prev = seq {
        match n' with
               //end case
        | 0 -> yield transition_fun prev
               //loop case
        | _ -> let res = transition_fun prev
               yield res
               yield! loop (n' - 1) res
    }

    R.rnorm(1, mean=0, sd=1).AsNumeric()
    |> Seq.head
    |> loop n

let n = 25000
let result = markov_chain n |> List.ofSeq

namedParams [
    "x", box result
    "breaks", box 150
    "main", box "cauchy(0,1) from normal(0,1) using metropolis-hastings"
    "col", box "grey"
    "probability", box true
]
|> R.hist
let xs = [-20.0 .. 0.1 .. 20.0]
let normal_fun = 
    xs
    |> List.map (fun x -> Seq.head(R.dcauchy(x, 0, 1).AsNumeric())) 
namedParams [
    "x", box xs
    "y",box normal_fun
    "col", box "red"
]
|> R.lines
