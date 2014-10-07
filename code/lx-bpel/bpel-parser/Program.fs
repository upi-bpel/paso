

[<EntryPoint>]
let main argv =
    //************************************ 1 ************************************************
    //1.	sequence        
    //2.	if
//    let data_path = @"D:\Dropbox\Code\ConsoleApplication1\BPEL_Examples\List\Concat4\"
//    let mutable BPEL_path = sprintf "%sConcat4.bpel" data_path
    //************************************ 2 ************************************************
    //1.	sequence
    //2.	flow (source, target, joinCondition, transitionCondition)
//    let data_path = @"D:\Dropbox\Code\ConsoleApplication1\BPEL_Examples\List\flowLinksCondition\"
//    let mutable BPEL_path = sprintf "%sflowLinksCondition.bpel" data_path    
    //************************************* 3 ***********************************************
    //1.	flow (source, target, transitionCondition)
    //2.	invoke
    //3.	faultHandlers
//    let data_path = @"D:\Dropbox\Code\ConsoleApplication1\BPEL_Examples\List\loanApprovalProcess\"
//    let mutable BPEL_path = sprintf "%sloanApprovalProcess.bpel" data_path    
    //************************************** 4 **********************************************
    //1.	sequence
    //2.	flow (source, target, joinCondition, transitionCondition)
    //3.	if else
    //4.	while
    //5.	invoke
//    let data_path = @"D:\Dropbox\Code\ConsoleApplication1\BPEL_Examples\List\TestActivityFlow\"
//    let mutable BPEL_path = sprintf "%sTestActivityFlow.bpel" data_path
    //************************************** 5 **********************************************
    //1.	Scope
    //2.	Sequence
//    let data_path = @"D:\Dropbox\Code\ConsoleApplication1\BPEL_Examples\List\Scope-CompensateScope\"
//    let mutable BPEL_path = sprintf "%sScope-CompensateScope.bpel" data_path
    //************************************************************************************

    let mutable Annotation_Path = sprintf "%sAnnotation.xml" data_path
    match argv.Length with
    | 0 -> ()
    | 1 ->
        BPEL_path <- argv.[0]
    | _ -> 
        BPEL_path<- argv.[0]
        Annotation_Path <-argv.[1]

    let doc = System.Xml.XmlDocument()
    doc.Load(BPEL_path)

    let test_analyzer = Analyzer.Analyzer(Probability.ProbabilityAnnotation.load Annotation_Path)
    //Probability.Probability.path <- Annotation_Path
    let activity =
        test_analyzer.TraverseNodes doc.ChildNodes <| System.Collections.Generic.List<Analyzer.Link>()
        |> Seq.map snd |> lx_bpel.Eval.makeSequence
    printfn "\n\n\n Eval = \n\n%s" <| lx_bpel.Eval.PrintActivity activity
    printfn "\n..............."
    printfn "\n\n\n Exec = \n\n%A" <| lx_bpel.Eval.Exec Map.empty activity
    System.Console.Read() |> ignore
    0