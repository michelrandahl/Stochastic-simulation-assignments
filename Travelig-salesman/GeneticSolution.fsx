module GeneticSolution

#load @"Utils.fsx"

open Utils
open System


type Configuration = {
    PopulationCount: int //number of population members (random gene permutations)
    FlatCount: int //algorithm will terminate when it has seen the same value this many times
    Mutationchance: float //chance of mutation 
    MatePercent: float //part of the population where mother candidates are chosen
    CrossOverPercent: float //percentage of genes to cross over when mating
}

//genetic algorithm which takes a set of 'genes' and a configuration as argument
//and attempts to find an optimal permutation of the genes
let GeneticAlgorithm (config: Configuration) (genes: Position []) = 
    //function that combines two members of the population
    let crossover (mother: Position []) (father: Position []) =
        let cut_length = float(mother.Length) * config.CrossOverPercent
                         |> Math.Ceiling |> int

        //function that swaps indexes to mutate a population member
        let mutate (genes: Position []) = 
            let swap_index1 = random() * (float genes.Length) |> int
            let swap_index2 = 
                random() * (float genes.Length)
                |> int
                |> fun x -> 
                    if x = swap_index1 then 
                        if x > 0 then x - 1
                        else x + 1
                    else x

            genes |> Seq.mapi (fun i v ->
                if i = swap_index1 then genes.[swap_index2]
                else if i = swap_index2 then genes.[swap_index1]
                else v)
            |> Array.ofSeq

        //function that performs the crossing 
        let produce_child parent1 parent2 =
            let cut_point = random() * float(Seq.length parent1 - cut_length) |> int

            let parent1_part = parent1
                               |> Seq.skip cut_point
                               |> Seq.take cut_length
            let parent2_part = parent2 |> not_in parent1_part

            let child =
                [parent2_part |> Seq.take cut_point
                 parent1_part
                 parent2_part |> Seq.skip cut_point]
                |> Seq.concat
                |> Array.ofSeq

            if random() < config.Mutationchance then 
                mutate child 
            else child 

        let child1 = produce_child mother father
        let child2 = produce_child father mother

        child1,child2

    let rec loop scores flat_count (population: Position [] []) =
        match flat_count with
                //end case, the algorithm returns
        | count when count = config.FlatCount -> scores, population
        | _ ->  //loop case
            let mate_count = float(config.PopulationCount) * config.MatePercent |> int

            //performing crossovers and mutations on a selection of the 'best'
            //population members (candidate solutions)
            let new_members =
                population
                |> Seq.take mate_count
                |> Seq.map (fun mother -> 
                    let father_index = 
                        float(config.PopulationCount) * 0.5 * random() 
                        |> int
                    crossover mother population.[mate_count + father_index]
                    ||> fun c1 c2 -> [|c1; c2|])
                |> Seq.concat
                |> Array.ofSeq

            //adding the new population members to the population
            //discarding the 'worst' performing members
            //and sorting the members by performance
            let new_pop =
                [|new_members;
                  population
                  |> Seq.take (config.PopulationCount - 2*mate_count)
                  |> Array.ofSeq|]
                |> Array.concat
                |> Array.map (fun m -> Score m, m)
                |> Array.sortBy fst

            //compares current best score with previous best score
            //in order to determine when the members are not improving anymore 
            //and the algorithm should return
            let score = fst new_pop.[0]
            let flat_count' =
                if Seq.head scores = score then
                    flat_count + 1
                else 0

            //invoking loop function recursively
            new_pop
            |> Array.map snd
            |> loop (score::scores) flat_count'

    //create an initial population and start the recursion
    let initial_pop =
        [|for _ in 1 ..config.PopulationCount  -> 
            let genes_order = Shuffle genes
            let score = Score genes_order
            score, genes_order|]
        |> Array.sortBy fst
    let score = fst initial_pop.[0]

    initial_pop
    |> Array.map snd
    |> loop [score] 0
