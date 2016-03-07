#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

//[1] Simulate a sample from a Cauchy(0,1) distribution with the inversion method
let breaks = 50

//sample 100 random uniformly dist numbers
let Nsim = 1000
let U = R.runif(Nsim).AsNumeric()

(* 
calculating the inverse function of cauchy(0,1)
u = 0.5 + Math.Atan(x) / Math.PI
u - 0.5 = Math.Atan(x) / Math.PI
(u - 0.5) * Math.PI = Math.Atan(x) 
Math.Tan( (u - 0.5) * Math.PI) = x
*)
let cauchy_inv u = Math.Tan(u - 0.5) * Math.PI

//applying the inv function over the random uniformly sampled numbers
let X = Seq.map (fun u -> cauchy_inv u) U
let Y = R.rcauchy(Nsim, 0, 1).AsNumeric()

namedParams [
    "freq",box false
    "main",box "cauchy from uni"
    "x",box X
    "breaks", box breaks
    "xlim", box [-2; 2]
]
|> R.hist

let testfun x = 1.0 / (Math.PI * (1.0 + x ** 2.0))
let xs = [-50.0 .. 0.5 .. 50.0]
namedParams [
    "x", box xs
    "y",box <| Seq.map testfun xs
    "col", box "red"
]
|> R.lines

namedParams [
    "x", box X
    "y", box "pcauchy"
    "location",box 0
    "scale",box 1
]
|> R.ks_test
|> fun r -> r.Print() // we get a fairly high p-value and thus reject the null hypothesis that the generated numbers doesn't follow a cauchy dist

//[2] Simulate a sample from a Beta(2,5) distribution using the rejection method with a uniform distribution as auxiliary distribution

let rbeta = R.rbeta(1000000, 2, 5).AsNumeric()
Seq.max rbeta
namedParams [
    "x", box rbeta
    "breaks", box 50
    "probability", box true
]
|> R.hist

let trials = 10000
let U1 = R.runif(trials).AsNumeric()
let U2 = R.runif(trials).AsNumeric()
let rbeta2 =
    Seq.zip U1 U2
    |> Seq.filter (fun (u1,u2) -> 2.5*u2 <= Seq.head(R.dbeta(u1,shape1=2,shape2=5).AsNumeric()))
    |> Seq.map fst
float(rbeta2 |> Seq.length) / float trials
namedParams [
    "x", box rbeta2
    "breaks", box 50
    "probability", box true
]
|> R.hist

let xs_test = [0.0 .. 0.01 .. 1.0]
let ftest = List.map (fun x -> Seq.head(R.dbeta(x, 2, 5).AsNumeric())) [0.0 .. 0.01 .. 1.0]
namedParams [
    "x", box xs_test
    "y",box ftest
    "col", box "red"
]
|> R.lines


//[3] Simulate a sample from an N(0,1) distribution with the rejection method using the double exponential distribution

//[4] One throws a needle of length l at random on a parquet floor with planck width l. Study by simulation this experiment and estimate the probability of the needle to intersect a line between two planks?
//IDEA: generate random coordinate and random angle, that is three random numbers.. should they be uniformly distributed?
//lets say length of needle and planck width is 1
//we observe the planck width as the interval 0 to 1, and the planck lengths as 1
//we now need to record how often the needle intersects with either x=0 or x=1
//this should be easy... we calculate the end-point of the needle given by the chosen coordinate and its angle, and if the x' value is less than 0 or greater than 1 then we record an intersect
//we assume no dependence between the x and y and the angle, so we just draw them from a uniform dist

//we only need to simulate x points because we just need to calculate the end x point of the vector
let l = 1.0
let needle_simulations = 100000
let needle_head_x = R.runif(needle_simulations, min=0, max=l).AsNumeric()
let needle_angle = R.runif(needle_simulations, min=0, max=360).AsNumeric()

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
    //id is the identity function
    let intersections = simulations |> Seq.filter id |> Seq.length
    float(intersections) / float(needle_simulations)

//number approximates PI
float needle_simulations * 2.0 / (float needle_intersect_prob * float needle_simulations)
