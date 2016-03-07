#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"
#load @"Utils.fsx"
#load @"GeneticSolution.fsx"
#load @"SimulatedAnnealingTSP.fsx"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics

open Utils
open GeneticSolution
open SimulatedAnnealingTSP


let stopwatch = System.Diagnostics.Stopwatch()

//generate list of positions of cities to visit
let cities = [|for _ in 1..35 -> random(),random()|]


//solve TSP with simulated annealing
stopwatch.Start()
let solution' = cities
                |> SimulatedAnnealing {StartTemp=10000.0; CoolingRate=0.003} 
                |> Seq.take 10000
let best_sol = solution'
               |> Seq.minBy fst
let scores = solution'
             |> Seq.map fst
             |> List.ofSeq
let solution = snd best_sol
stopwatch.Stop()

plot_solution scores solution stopwatch.ElapsedMilliseconds (fst best_sol)

//solve TSP with genetic algorithm
stopwatch.Start()
let genetic_config = {
    PopulationCount = 150
    FlatCount = 150
    Mutationchance = 0.5
    MatePercent = 0.25
    CrossOverPercent = 0.4
}
let scores2,solution2 = 
    GeneticAlgorithm genetic_config cities
    ||> fun scores solution -> scores, Seq.head solution
stopwatch.Stop()

plot_solution (List.rev scores2) solution2 stopwatch.ElapsedMilliseconds (Seq.head scores2)
