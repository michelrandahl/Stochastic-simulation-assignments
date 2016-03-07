#r @"..\packages\R.NET.Community.1.5.16\lib\net40\RDotNet.dll"
#r @"..\packages\R.NET.Community.FSharp.0.1.9\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.dll"
#r @"..\packages\RProvider.1.1.8\lib\net40\RProvider.Runtime.dll"

open RDotNet
open RProvider
open RProvider.stats
open RProvider.graphics
open System

let isprime n =
    let rec check i =
        i > n/2 || (n % i <> 0 && check (i + 1))
    check 2

//[1] - Implement the congruence method a = 16807,b = 0 and M = 2147483647. 
let congruential_method (initial: int) (a: int) (b: int) (M: int) =
    let rec loop prev = seq {
        let x = (a * prev + b) % M
        yield float x / float M
        yield! loop x
    }
    seq {
        yield float initial / float M
        yield! loop initial
    }

//[2] - Simulate 1000 realisations on [0,1] 
let seed = 3
let a = 16807
let b = 0
let M = 2147483647
let U1 = congruential_method seed a b M 
         |> Seq.take 1000 
         |> List.ofSeq

//Plot a histogram of the sample 
namedParams [
    "x", box U1
    "breaks", box 50
]
|> R.hist 
//Illustrate graphically the distribution of (xk,xk+2) by a scatter plot 
R.plot(Seq.take (U1.Length-1) U1, List.tail U1)
//Plot the auto-correlation function of the sample
R.pacf U1
//Perform a statistical test of the hypothesis that the simulated values 
namedParams [
    "x", box U1
    "y", box "punif"
    "min",box -1
    "max",box 1
]
|> R.ks_test
|> fun r -> r.Print() // we get a fairly high p-value and thus reject the null hypothesis that the generated numbers doesn't follow a uniform dist

//[3] - Repeat the previous steps with parameters a,b and M of your choice
let seed2 = 5
let a2 = 1373
let b2 = 899
let M2 = 7777
let U2 = congruential_method seed2 a2 b2 M2 
         |> Seq.take 10000
         |> List.ofSeq

//Plot a histogram of the sample 
namedParams [
    "x", box U2
    "breaks", box 50
]
|> R.hist 
//Illustrate graphically the distribution of (xk,xk+2) by a scatter plot 
R.plot(Seq.take (U2.Length-1) U2, Seq.skip 1 U2)
//Plot the auto-correlation function of the sample
R.pacf U2
//from the autocorrelation plot we can see that there is a spike for 20, so lets try and plot this as scatter plot
R.plot(Seq.take (U2.Length-20) U2, Seq.skip 20 U2)
//Perform a statistical test of the hypothesis that the simulated values 
namedParams [
    "x", box U2
    "y", box "punif"
    "min",box -1
    "max",box 1
]
|> R.ks_test
|> fun r -> r.Print() // we get a very low p-value and can thus accept the null hypothesis that the generated numbers doesn't follow a uniform dist
