module SimulatedAnnealingOpimizer

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
open System

open Utils
open GeneticSolution
open SimulatedAnnealingTSP


//scores a given configuration for a genetic algorithm
//the score is a ratio between line segment intersections and number of cities in relation to population count
let ScoreConfig (cities: Position []) (genetic_config: GeneticSolution.Configuration) =
    let score,solution = 
        GeneticAlgorithm genetic_config cities
        ||> fun scores solution -> Seq.head scores, Seq.head solution

    let int_count = intersection_count solution |> float

    int_count / float(Seq.length cities) +  float(genetic_config.PopulationCount) / float(Seq.length cities)
    
let SimulatedAnnealing candidate_conf candidate_score pop_count_ratio = 
    let random_cities() = 
        let cities_count = 
            R.runif(1, 25, 50).AsNumeric() 
            |> Seq.head |> Math.Floor |> int
        [|for _ in 1..cities_count -> random(),random()|]

    let rec loop prev_score n (pop_count_ratio: float) (conf: GeneticSolution.Configuration) = seq {
        let cities = random_cities()
        let temperature = 1.0 / Math.Log(float n)

        let pop_count_ratio' = 
            R.rnorm(1, mean = pop_count_ratio, sd = 0.5).AsNumeric() 
            |> Seq.head 
            |> fun u -> 
                if u < 1.0 then 1.0 
                else if u > 4.0 then 4.0
                else u
        let mut_chance = 
            R.rnorm(1, mean = conf.Mutationchance, sd = 0.1).AsNumeric() 
            |> Seq.head 
            |> fun u -> 
                if u > 0.7 then 0.7
                else if u < 0.0 then 0.05
                else u
        let mate_percent = 
            R.rnorm(1, mean = conf.MatePercent, sd = 0.1).AsNumeric() 
            |> Seq.head 
            |> fun u -> 
                if u > 0.4 then 0.4
                else if u < 0.0 then 0.1
                else u
        let cross_percent = 
            R.rnorm(1, mean = conf.CrossOverPercent, sd = 0.1).AsNumeric() 
            |> Seq.head 
            |> fun u -> 
                if u > 0.4 then 0.4
                else if u < 0.0 then 0.1
                else u

        let candidate_conf = {
            PopulationCount = pop_count_ratio' * float(Seq.length cities) |> Math.Floor |> int
            FlatCount = conf.FlatCount
            Mutationchance = mut_chance
            MatePercent = mate_percent
            CrossOverPercent = cross_percent
        }

        printfn "candidate: %A" candidate_conf

        let candidate_score = ScoreConfig (random_cities()) candidate_conf

        let accept_prob =
            let delta_h = prev_score - candidate_score
            [1.0; Math.Exp(delta_h / temperature)]
            |> Seq.min

        let accept = R.rbinom(1, 1, accept_prob).AsNumeric() |> Seq.head

        if accept = 1.0 then
            yield (candidate_score, candidate_conf), pop_count_ratio'
            yield! loop candidate_score (n + 1) pop_count_ratio' candidate_conf
        else
            yield (prev_score, conf), pop_count_ratio 
            yield! loop prev_score (n + 1) pop_count_ratio conf

    }

    loop candidate_score 2 pop_count_ratio candidate_conf


let cities = [|for _ in 1..35 -> random(),random()|]

let pop_count_ratio = 2.0

let candidate_conf = {
    PopulationCount = pop_count_ratio * float(Seq.length cities) |> Math.Floor |> int
    FlatCount = 50
    Mutationchance = 0.3
    MatePercent = 0.25
    CrossOverPercent = 0.3
}

let candidate_score = ScoreConfig cities candidate_conf

let results = 
    SimulatedAnnealing candidate_conf candidate_score pop_count_ratio 
    |> Seq.take 50
    |> List.ofSeq

let best = results
           |> Seq.minBy (fun x -> fst(fst x))
let best_conf = snd(fst best)
let best_pop_scaling_fac = snd(best)

let scores = 
    results 
    |> Seq.map fst
    |> Seq.map fst
namedParams [
    "x", box scores
    "type", box "l"
]
|> R.plot
            
//test the proposed configuration
let cities2 = [|for _ in 1..50 -> random(),random()|]
let stopwatch = System.Diagnostics.Stopwatch()
let test = { best_conf with PopulationCount = int(50.0 * best_pop_scaling_fac) }
stopwatch.Start()
let scores2,solution2 = 
    GeneticAlgorithm test cities2
    ||> fun scores solution -> scores, Seq.head solution
stopwatch.Stop()
plot_solution (List.rev scores2) solution2 stopwatch.ElapsedMilliseconds (Seq.head scores)
