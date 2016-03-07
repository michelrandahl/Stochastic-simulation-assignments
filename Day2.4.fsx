#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[4] - One throws a needle of length l at random on a parquet floor with planck width l. Study by simulation this experiment and estimate the probability of the needle to intersect a line between two planks?

//IDEA: generate random coordinate and random angle, that is three random numbers.. should they be uniformly distributed?
//lets say length of needle and planck width is 1
//we observe the planck width as the interval 0 to 1, and the planck lengths as 1
//we now need to record how often the needle intersects with either x=0 or x=1
//this should be easy... we calculate the end-point of the needle given by the chosen coordinate and its angle, and if the x' value is less than 0 or greater than 1 then we record an intersect
//we assume no dependence between the x and y and the angle, so we just draw them from a uniform dist

//we only need to simulate x points because we just need to calculate the end x point of the vector
let l = 1.0 //length of the needles
let needle_simulations = 100000
let needle_head_x = R.runif(needle_simulations, min=0, max=l).AsNumeric()
let needle_angle = R.runif(needle_simulations, min=0, max=360).AsNumeric()

//function that checks if a given needle intersects with any planck gap
let intersects x angle = 
    let x' = x + l * Math.Cos angle
    if x' < 0.0 then
        true
    else if x' > l then
        true
    else
        false

let simulations = 
    Seq.zip needle_head_x needle_angle
    |> Seq.map (fun (x,a) -> intersects x a)

let needle_intersect_prob =
    let intersections = 
        simulations 
        |> Seq.filter id //'id' is the identity function
        |> Seq.length 
    float(intersections) / float(needle_simulations)

//number approximates PI
float needle_simulations * 2.0 / (float needle_intersect_prob * float needle_simulations)
