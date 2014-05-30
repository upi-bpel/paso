// Per ulteriori informazioni su F#, visitare http://fsharp.net. Vedere il progetto 'Esercitazione su F#'
// per ulteriori linee guida sulla programmazione F#.

#load "Exec.fs"
open lx_bpel

let costExpectation iterations workflow =
    let mutable totalTime,totalPrice,totalSuccesses = 0.0,0.0,0.0
    for i = 1 to iterations do
        let env = Map.empty
        let env,outcome,cost = Eval.Exec env workflow
        let price,time = cost
        totalPrice <- totalPrice + price
        if outcome = Success && (time <= (2400.0)) then
            totalTime <- totalTime + time
            totalSuccesses <- totalSuccesses + 1.0
    totalTime,totalPrice,totalSuccesses


let time10000,price10000,successes10000 = costExpectation 10000 ()
time10000/successes10000
price10000/successes10000
successes10000/10000.0
let time100000,price100000,successes100000 = costExpectation 1000000 ()
time100000/successes100000
price100000/successes100000
successes100000/1000000.0// Viene definito qui il codice di script della libreria

for i in [100000000;1000000000] do
    let t,p,s = costExpectation i <| Eval.total
    printfn "Iterations: %d\tReliability: %A\tPrice: %A\tTime:%A" i (s/(float i)) (p/s) (t/s)

0.15**2.0