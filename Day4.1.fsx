#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[1] - Simulate a Markov chain with values in S = {0,1} such that P(xi = 1|xi−1 = 0) = 1/√2 and P(xi = 1|xi−1 = 1) = 1/π Plot the result. 

let markov_chain n =
    //defining the transition function for the markov chain
    let transition_fun = function
               //return 1 with a probability of 1/sqrt(2) if input is 0
        | 0 -> R.rbinom(1, 1, 1.0 / Math.Sqrt(2.0)).AsNumeric()
               |> Seq.head
               |> int
               //return 1 with a probability of 1.PI if input is 1
        | _ -> R.rbinom(1, 1, 1.0 / Math.PI).AsNumeric()
               |> Seq.head
               |> int

    //recursive loop to perform n-number of iterations of the markov chain
    let rec loop n' prev = seq {
        match n' with
               //end case
        | 0 -> yield transition_fun prev
               //loop case
        | _ -> let res = transition_fun prev
               yield res
               yield! loop (n'-1) res
    }

    //initializing the loop
    R.rbinom(1, 1, 0.5).AsNumeric()
    |> Seq.head
    |> int
    |> loop n 

//running the markov chain and plotting the result in a histogram
markov_chain 1000 
|> R.hist

