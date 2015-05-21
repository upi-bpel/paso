(*
 *
 * This code is provided for evaluation of the algorithm presented in the paper:
 *
 * Bartoloni, Leonardo, Antonio Brogi, and Ahmad Ibrahim. 
 * "Probabilistic Prediction of the QoS of Service Orchestrations: A Truly Compositional Approach."
 * In Service-Oriented Computing, pp. 378-385. Springer Berlin Heidelberg, 2014.
 *
 *)

[<EntryPoint>]
let main argv =
  
    let data_path = @"..\..\..\..\..\BPEL_Examples\ShippingService_Paper\"
    let mutable BPEL_path = sprintf "%sShippingService.bpel" data_path    
    //let data_path = @"..\..\..\..\..\BPEL_Examples\loanExample_Paper\"
    //let mutable BPEL_path = sprintf "%sloan.bpel" data_path
 
    let mutable Annotation_Path = sprintf "%sAnnotation.xml" data_path
    let mutable iterationCount = 500
   // let mutable iterationCount = 10
    match argv.Length with
    | 0 -> ()
    | 1 ->
        BPEL_path <- argv.[0]
    | 2 -> 
        BPEL_path<- argv.[0]
        Annotation_Path <- argv.[1]
    | _ -> 
        BPEL_path<- argv.[0]
        Annotation_Path <- argv.[1]
        System.Int32.TryParse(argv.[2],&iterationCount) |> ignore

    let doc = System.Xml.XmlDocument()
    doc.Load(BPEL_path)

    let test_analyzer = Analyzer.Analyzer(Probability.ProbabilityAnnotation.load Annotation_Path)

    let activity =
        let h,activityList = test_analyzer.TraverseNodes doc.ChildNodes (System.Collections.Generic.List<Analyzer.Link>() )
        let mainActivity =
            match activityList |> Seq.toList with 
            | [] -> lx_bpel.Nothing
            | (name,_,a)::[] -> a
            | l -> lx_bpel.Sequence (List.map (fun (_,_,a) ->a) l)   //Creates a new collection whose elements are the results of applying the given function to each of the elements of the collection.
        match h with                  //h is for fault handler
        | [] -> mainActivity
        | a::[] -> lx_bpel.Scope (mainActivity,a)
        | _ -> lx_bpel.Scope (mainActivity, h |> lx_bpel.Sequence)
 
    printfn "\n\n\n Eval = \n\n%s" <| lx_bpel.Eval.PrintActivity activity
    printfn "\n..............."

    let samplesSeq =
        Seq.init iterationCount <| fun number ->
            lx_bpel.Eval.Exec Map.empty activity ()   ///exec is called here for the first time
        |> Seq.cache


    let probability e =
         Seq.averageBy (e>>function true ->1.0 |false -> 0.0) samplesSeq

    let timeLessThan5sec (env,outcome,(price,time)) =
        time < 5.0

    let probabilityTimeLessThan5sec = probability timeLessThan5sec

    printfn "probabilityTimeLessThan5sec = %f" probabilityTimeLessThan5sec

    Visualizer.show samplesSeq
//    let costExpectation iterations workflow =
//        let mutable totalTime,totalPrice,totalSuccesses = 0.0,0.0,0.0
//        for i = 1 to iterations do
//            let env = Map.empty
//            let env,outcome,cost = Eval.Exec env workflow
//            let price,time = cost
//            totalPrice <- totalPrice + price
//            if outcome = Success && (time <= (2400.0)) then
//                totalTime <- totalTime + time
//                totalSuccesses <- totalSuccesses + 1.0
//        totalTime,totalPrice,totalSuccesses

    0