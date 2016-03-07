#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open RProvider.mvtnorm
open System

//[2] - The unit ball ...
let n = 10000

//function to calcualte exact volume of a unit ball
let unit_ball_true_volume dim =
    Math.PI**(float(dim)/2.0) / 
        (R.gamma(1.0 + float(dim)/2.0).AsNumeric() 
        |> Seq.head)

//function to estimate volume of a unit ball
let unit_ball_volume_estimate simulations dimensions =
    let identity_fun xs =
        xs
        |> Seq.map (fun x -> x**2.0)
        |> Seq.sum
        |> fun x -> 
            match x <= 1.0 with
            | true -> 1.0
            | false -> 0.0

    let U = [|for i in 1 .. dimensions -> 
              R.runif(simulations, -1, 1).AsNumeric().ToArray()|] 
            |> array2D

    let scored_vectors = 
        [0 .. simulations-1]
        |> Seq.map (fun n -> U.[0..,n], identity_fun U.[0..,n])

    let points_in_ball =
        scored_vectors
        |> Seq.filter (fun x -> snd x > 0.0)
        |> Seq.map fst
        |> array2D

    let point_count = 
        points_in_ball
        |> Array2D.length1

    [for n in 0 .. (point_count-1) ->
     points_in_ball.[n,0], points_in_ball.[n,1]]
    |> List.ofSeq
    |> List.unzip
    ||> fun X Y ->
        namedParams [
            "x", box X
            "y", box Y
            "xlim", box [-1;1]
            "ylim", box [-1;1]
        ]
    |> R.plot
    |> ignore
    
    let ball_ratio = scored_vectors |> Seq.map snd |> Seq.average
    let estimate = 2.0**float(dimensions) * ball_ratio
    let variance = 
        1.0 / float(simulations)**2.0 * 
            ([for x in scored_vectors -> 
              if snd x > 0.0 then 1.0 else 0.0]
             |> Seq.map (fun x -> (x - estimate)**2.0)
             |> Seq.sum)
    estimate, variance

unit_ball_volume_estimate n 2
unit_ball_true_volume 2

unit_ball_volume_estimate n 3
unit_ball_true_volume 3

unit_ball_volume_estimate n 10
unit_ball_true_volume 10

//Modify the previous method by simulating a symmetric multivariate normal

//function to estimate volume of unit ball by simulating from a symmetrix multivariate normal dist
let unit_ball_volume_estimate2 (simulations: int) (dimensions: int) =
    let sigma = R.diag(dimensions).AsNumericMatrix().ToArray()

    let identity_fun xs = 
        xs
        |> Seq.map (fun x -> x**2.0)
        |> Seq.sum
        |> fun x -> 
            match x <= 1.0 with
            | true -> 
                let dmvnorm = R.dmvnorm(xs).AsNumeric()
                              |> Seq.head
                1.0 / dmvnorm
            | false -> 0.0

    let mv_random_normal_dist = 
        namedParams [
            "n", box (dimensions * simulations)
            "mean", box <| R.rep(0, dimensions)
        ]
        |> R.rmvnorm

    let U =
        namedParams [
            "data", box mv_random_normal_dist
            "ncol", box dimensions
        ]
        |> fun x -> R.t(R.matrix(x)).AsNumericMatrix().ToArray()

    let scored_vectors = 
        [0 .. simulations-1]
        |> Seq.map (fun n -> U.[0..,n], identity_fun U.[0..,n])

    let points_in_ball =
        scored_vectors
        |> Seq.filter (fun x -> snd x > 0.0)
        |> Seq.map fst
        |> array2D
    let point_count = 
        points_in_ball
        |> Array2D.length1

    [for n in 0 .. (point_count-1) ->
     points_in_ball.[n,0], points_in_ball.[n,1]]
    |> List.ofSeq
    |> List.unzip
    ||> fun X Y ->
        namedParams [
            "x", box X
            "y", box Y
            "xlim", box [-1;1]
            "ylim", box [-1;1]
        ]
    |> R.plot
    |> ignore
    
    let estimate = scored_vectors |> Seq.map snd |> Seq.average
    let variance = 
        1.0 / float(simulations)**2.0 * 
            ([for x in scored_vectors -> snd x]
             |> Seq.map (fun x -> (x - estimate)**2.0)
             |> Seq.sum)
    estimate, variance

//testing the function on the unit balls 2,3,10
unit_ball_volume_estimate2 n 2
unit_ball_volume_estimate2 n 3
unit_ball_volume_estimate2 n 10
