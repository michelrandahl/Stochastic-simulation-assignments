module Utils
#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

type Position = (float * float)

let rnd = Random()
let random() = rnd.NextDouble()


//shuffles an array
let Shuffle (xs: 'a []) = 
    let arr = Array.copy xs
    let max = (arr.Length - 1)
    let randomSwap (arr: 'a []) i =
        let pos = rnd.Next(max)
        let tmp = arr.[pos]
        arr.[pos] <- arr.[i]
        arr.[i] <- tmp
        arr
    [|0..max|] 
    |> Array.fold randomSwap arr


//calculates the diff between two lists
let not_in (ps': 'a seq) (ps: 'a seq) =
    ps |> Seq.filter (fun p -> not(Seq.exists (fun p' -> p' = p) ps'))


//euclidian dist between two points in a plane
let Distance (g1: Position) (g2: Position) =
    Math.Sqrt((fst g1 - fst g2)**2.0 + (snd g1 - snd g2)**2.0)


//score a list of cities for TSP
let Score population_member = 
    let rec loop = function
        | x::[],      score -> score
        | x1::x2::xs, score -> 
            let score' = Distance x1 x2
            loop (x2::xs, score + score')
        | _,score -> score
    loop(List.ofArray population_member, 0.0)


//check for intersection between two line segments
let intersection (line1: Position*Position) (line2: Position*Position) =
    let cmp = fst(fst line2) - fst(fst line1), snd(fst line2) - snd(fst line1)
    let r = fst(snd line1) - fst(fst line1), snd(snd line1) - snd(fst line1)
    let s = fst(snd line2) - fst(fst line2), snd(snd line2) - snd(fst line2)

    let cmpxr = fst cmp * snd r - snd cmp * fst r
    let cmpxs = fst cmp * snd s - snd cmp * fst s
    let rxs = fst r * snd s - snd r * fst s

    if rxs < 0.001 then
        false
    else
        let rxsr = 1.0 / rxs
        let t = cmpxs * rxsr
        let u = cmpxr * rxsr
        (t >= 0.001) && (t <= 1.0) && (u >= 0.001) && (u <= 1.0)


//count number of intersections in a given TSP plan
let intersection_count (cities: Position []) =
    let connected_cities = cities |> Seq.windowed 2
    seq {
        for con in connected_cities do
            for con' in connected_cities do
                if con <> con' && con.[1] <> con'.[0] && con'.[1] <> con.[0] then
                    let intersects = intersection (con.[0],con.[1]) (con'.[0],con'.[1])
                    if intersects then
                        yield set [(con.[0],con.[1]); (con'.[0],con'.[1])]
    } 
    |> Set.ofSeq
    |> Seq.length


let plot_solution scores sol time best_score =
    let num_intersections = intersection_count sol
    let details = sprintf "intersections: %A, time: %A" num_intersections time 

    namedParams [
        "mfrow", box [1;2]
        "title", box "aloha"
    ]
    |> R.par
    |> ignore

    namedParams [
        "x", box scores
        "type", box "l"
        "ylab", box "score"
        "main", box best_score
    ]
    |> R.plot
    |> ignore

    namedParams [
        "x", box (sol |> Seq.map fst)
        "y", box (sol |> Seq.map snd)
        "type", box "l"
        "main", box details
    ]
    |> R.plot
    |> ignore
