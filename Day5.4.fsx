#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[2] - Implement the simulated annealing method to maximize h on [−3,3]^2
let r_fun = """
            h = function(x,y) 
            { 
                3* dnorm(x,m=0,sd=.5)*dnorm(y,m=0,sd=.5) + 
                dnorm(x,m=-1,sd=.5)*dnorm(y,m=1,sd=.3) + 
                dnorm(x,m=1,sd=.5)*dnorm(y,m=1,sd=.3) 
            }
            """
R.eval(R.parse(text = r_fun))

let h (x: float, y: float) = 
    3.0 * (R.dnorm(x, mean =  0, sd = 0.5).AsNumeric() |> Seq.head) *
          (R.dnorm(y, mean =  0, sd = 0.5).AsNumeric() |> Seq.head) +

    1.0 * (R.dnorm(x, mean = -1, sd = 0.5).AsNumeric() |> Seq.head) *
          (R.dnorm(y, mean =  1, sd = 0.3).AsNumeric() |> Seq.head) +

    1.0 * (R.dnorm(x, mean =  1, sd = 0.5).AsNumeric() |> Seq.head) *
          (R.dnorm(y, mean =  1, sd = 0.3).AsNumeric() |> Seq.head)

let xs = [-3.0 .. 0.01 .. 3.0]
let ys = [-3.0 .. 0.01 .. 3.0]
let H = R.outer(xs, ys, FUN = "h").AsNumericMatrix().ToArray()
namedParams [
    "x", box xs
    "y", box ys
    "z", box H
]
|> R.image
namedParams [
    "x", box xs
    "y", box ys
    "z", box H
    "add", box true
]
|> R.contour

//simulated annealing algorithm
let n = 1000

let sim_ann n h =
    let rec loop (n': int) (prev: float * float) = seq {
        if n' < n then
            //random vector
            let zeta = R.rnorm(2,0,1).AsNumeric() |> List.ofSeq
            let x,y = zeta.[0], zeta.[1]

            //limiting values to interval [-3;3]
            let limit v =
                match v with
                | v when v <= -3.0 -> -3.0
                | v when v >= 3.0  -> 3.0
                | _ -> v

            //adding random vector to the previous step
            let candidate = 
                limit(fst prev + zeta.[0]),
                limit(snd prev + zeta.[1])

            let delta_h = (h candidate) - (h prev)
            let Temperature = 1.0 / Math.Log(float n')
            let accept_prob = Seq.min [1.0; Math.Exp(delta_h / Temperature)]

            //accepting based on unfair coin with accept_prob
            let accept = R.rbinom(1, 1, accept_prob).AsNumeric() |> Seq.head

            if accept = 1.0 then
                yield candidate
                yield! loop (n' + 1) candidate
            else
                yield! loop (n' + 1) prev
        }

    let initial = R.runif(2, -3, 3).AsNumeric() |> List.ofSeq
    loop 1 (initial.[0], initial.[1])


let result = sim_ann n h |> List.ofSeq
let result_arr = 
    result
    |> Seq.map (fun x -> [fst x; snd x])
    |> array2D
namedParams [
    "x", box result_arr
    "type", box "l"
]
|> R.lines

let last_point_amplitude =
    (List.ofSeq result).[(Seq.length result) - 2]
    h (List.ofSeq result).[(Seq.length result) - 2]
let best_point =
    result
    |> Seq.map (fun x -> x, h x)
    |> Seq.maxBy snd
