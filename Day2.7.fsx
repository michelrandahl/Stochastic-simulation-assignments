#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open RProvider.MASS
open System

//[7] - Simulate a sample of size 50000 from a bivariate centred, standardized Gaussian vector with correlation coeﬃcient ρ = 0.7. Plot this sample
let n = 50000
//target covariance matrix
let M = array2D [[1.0; 0.7]
                 [0.7; 1.0]]

//performing cholesky decomposition
let U = R.chol(M).AsNumericMatrix().ToArray()
let L = R.t(U).AsNumericMatrix().ToArray()

//generate bivariate random normal distributed data set
let x1 = R.rnorm(n,0,1).AsNumeric()
let x2 = R.rnorm(n,0,1).AsNumeric()
let X = array2D [x1
                 x2]

//performing matrix multiplication between the cholesky decomposed matrix and the random bivariate sample
let Y = R.``%*%``(L, X).AsNumericMatrix().ToArray()
let Y' = R.t(Y).AsNumericMatrix().ToArray()

namedParams [
    "x", box Y'
    "asp", box 1
    "las", box 1
    "cex", box 1
    "col", box 2
]
|> R.plot
namedParams [
    "x", box <| R.kde2d(Y.[0,0..], Y.[1,0..])
    "add", box true
]
|> R.contour

//verifying that the covariance matrix is similiar to the initial covariance matrix
R.cov(Y').AsNumericMatrix().ToArray()

//Extract a sample of a random variable approximately distributed as Y2|Y1=.5
//no Y1 values are exactly 0.5
//so we have to sample from an interval, for example 0.49 to 0.51
let interval x = 0.49 <= x && x <= 0.51
Y.[0,0..]
|> Seq.filter (fun x -> interval x)
|> Seq.length

//extracting values
let sample_slice = 
    [0 .. Array2D.length1 Y' - 1]
    |> Seq.map (fun i -> interval Y'.[i,0], Y'.[i,1])
    |> Seq.filter fst
    |> Seq.map snd

let mean = R.mean(sample_slice).AsNumeric() |> Seq.head
let varaince = R.var(sample_slice).AsNumeric() |> Seq.head

namedParams [
    "x", box sample_slice
    "breaks", box 25
    "probability", box true
]
|> R.hist
let xs = [-4.0 .. 0.1 .. 4.0]
let normal_fun = 
    xs
    |> List.map (fun x -> Seq.head(R.dnorm(x, 0, 1).AsNumeric())) 
namedParams [
    "x", box xs
    "y",box normal_fun
    "col", box "red"
]
|> R.lines
