module SimulatedAnnealingTSP

#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"
#load @"Utils.fsx"

open Utils
open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//function to swap two random indexes
let rswap_indexes (cities: Position []) = 
    let swap_index1 = random() * (float cities.Length) |> int
    let swap_index2 = 
        random() * (float cities.Length)
        |> int
        |> fun x -> 
            if x = swap_index1 then 
                if x > 0 then x - 1
                else x + 1
            else x

    cities |> Seq.mapi (fun i v ->
        if i = swap_index1 then cities.[swap_index2]
        else if i = swap_index2 then cities.[swap_index1]
        else v)
    |> Array.ofSeq

type Configuration = {
    StartTemp: float
    CoolingRate: float
}

//simulated annealing takes a configuration and a list of cities as arguments
//and attempts at finding an optimal permutation
let SimulatedAnnealing (config: Configuration) (cities: Position []) =
    let cities_count = Seq.length cities

    let rec loop prev_score cities temp = seq {
        let candidate_cities' = cities |> Array.copy |> rswap_indexes

        let score = Score candidate_cities'
        //calculating the probability of accepting the given solution
        let accept_prob = 
            if score > prev_score then
                let delta_h = prev_score - score
                Math.Exp(delta_h / temp)
            else 1.0

        //decrease the temperature
        let temp' = temp * (1.0 - config.CoolingRate)

        if random() < accept_prob then
            yield score,candidate_cities'
            yield! loop score candidate_cities' temp'
        else 
            yield prev_score,cities
            yield! loop prev_score cities temp'
    }

    let score = Score cities
    loop score cities config.StartTemp
            
            
