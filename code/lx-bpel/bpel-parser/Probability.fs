module Probability

open System

type Probability(name,success,fault,stuck,cost,time) =
    static member val path = "" with get, set
    member val name = name with get,set
    member val success = success with get,set
    member val fault = fault with get,set
    member val stuck = stuck with get,set
    member val cost = cost with get,set
    member val time = time with get,set

    new () =
        let nan = Double.NaN
        Probability("",nan,nan,nan,nan,nan)

    member this.print () =
        printfn ""
        printfn "Condition = %s" this.name
        printfn " success =%f" this.success
        printfn " fault =%f" this.fault
        printfn " stuck =%f" this.stuck
        printfn " cost =%f" this.cost
        printfn " time =%f" this.time
        printfn " -------------------"
    
    static member getConditionProbability n param =
        let Prob_list = new System.Collections.Generic.List<Probability> ()
        for line in System.IO.File.ReadAllLines(Probability.path) do
            let t = line.Split ';'
            for p in t do
                let parts = p.Split ','
                match param with
                | 1 ->  for v in Seq.skip 1 parts do
                            let p1 = v.Split ':'
                            let temp = Probability ()
                            temp.name <- n
                            temp.success <- Double.Parse(v,System.Globalization.CultureInfo.InvariantCulture)
                            Prob_list.Add temp
                            //temp.print()
                | 2 -> for v in Seq.skip 1 parts do
                            let p1 = v.Split ':'
                            let temp = Probability ()
                            temp.name <- n
                            let x = Double.Parse(p1.[1],System.Globalization.CultureInfo.InvariantCulture)
                            temp.cost <- Double.Parse(p1.[2].Replace("$",""),System.Globalization.CultureInfo.InvariantCulture)
                            temp.time <- Double.Parse(p1.[3].Replace("sec",""),System.Globalization.CultureInfo.InvariantCulture)
                            match p1.[0] with
                            | "s" -> temp.success <- x
                            | "f" -> temp.fault <- x
                            | "st" -> temp.stuck <- x
                            | _ -> printfn "error: invalid character found in probability list"
                            Prob_list.Add temp
                            //temp.print ()
                | _ -> printfn "error: invalid parameter"
        Prob_list
    
    static member getInvokeSamplingFunction n =
        let prob_List = System.Collections.Generic.List<double*(lx_bpel.Outcome*lx_bpel.Cost)>()
        let temp_List = Probability.getConditionProbability n 2

        for item in temp_List do
            item.print()
            if Double.IsNaN item.success |> not then
                prob_List.Add (item.success,(lx_bpel.Success,(item.cost,item.time)))
            if Double.IsNaN item.fault |> not then
                prob_List.Add (item.fault,(lx_bpel.Fault,(item.cost,item.time)))
            if Double.IsNaN item.stuck |> not then
                prob_List.Add (item.fault,(lx_bpel.Stuck,(item.cost,item.time)))

        Seq.toList prob_List 
        |> lx_bpel.Eval.outcomeFromProbabilities 
