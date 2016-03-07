#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[4] - Simulate a sample from a Beta B(1/2,1/2) with the rejection algorithm (using e.g. the uniform distribution as majorating density)
//Then simulate the same B(1/2,1/2) with the MH algorithm.

//simulating using the rejection algorithm
let n = 25000
let U1 = R.runif(n).AsNumeric() 
let U2 = R.runif(n).AsNumeric() 

let accept_function scaling_factor (u1, u2) =
    let beta_res =
        R.dbeta(u1, shape1=0.5, shape2=0.5).AsNumeric()
        |> Seq.head
    scaling_factor * u2 < beta_res

let rbeta_rej =
    Seq.zip U1 U2
    |> Seq.filter (accept_function 3.0)
    |> Seq.map fst
let accepted = 100.0 * float(rbeta_rej |> Seq.length) / float n

//plotting the result of the rejection algorithm
namedParams [
    "x", box rbeta_rej
    "main", box (sprintf "beta(0.5,0.5) from uniform dist (%A%% accepted)" accepted)
    "breaks", box 50
    "probability", box true
]
|> R.hist
let xs = [0.0 .. 0.01 .. 1.0]
namedParams [
    "x", box xs
    "y",box <| R.dbeta(xs, 0.5, 0.5)
    "col", box "red"
]
|> R.lines


//simulating using the MH algorithm
let markov_chain n =
    //target funciton
    let f (x: float) =
        R.dbeta(x, 0.5, 0.5).AsNumeric()
        |> Seq.head

    //auxillery function
    let q (x: float) (y: float) =
        R.dnorm(x, mean=y, sd=1).AsNumeric()
        |> Seq.head

    //transition function based on MH algorithm
    let transition_fun prev = 
        let y = R.rnorm(1, mean=prev, sd=1).AsNumeric()
                |> Seq.head
        let met_ratio =
            f y * q prev y /
            (f prev * q y prev)
        let accept_prob = Seq.min [1.0; met_ratio]
        let accept = R.rbinom(1, 1, accept_prob).AsNumeric()
                     |> Seq.head
                     |> int
        match accept with
        | 0 -> prev
        | _ -> y
    let rec loop n' prev = seq {
        match n' with
        | 0 -> yield transition_fun prev
        | _ ->
            let res = transition_fun prev
            yield res
            yield! loop (n' - 1) res
    }

    R.rnorm(1, mean=0, sd=1).AsNumeric()
    |> Seq.head
    |> loop n

//calculating and plotting the result
let rbeta_mh = markov_chain n |> List.ofSeq
namedParams [
    "x", box <| Seq.filter (fun x -> x >= 0.0) rbeta_mh
    "breaks", box 50
    "main", box "beta(0.5,0.5) from normal(0,1) using metropolis-hastings"
    "col", box "grey"
    "probability", box true
]
|> R.hist
namedParams [
    "x", box xs
    "y",box <| R.dbeta(xs, 0.5, 0.5)
    "col", box "red"
]
|> R.lines

//plotting the autocorrelation
namedParams [
    "mfrow",[1;2]
]
|> R.par

namedParams [
    "x", box rbeta_rej
    "main", box "rejection method"
]
|> R.acf 
namedParams [
    "x", box rbeta_mh
    "main", box "MH algo"
]
|> R.acf 

