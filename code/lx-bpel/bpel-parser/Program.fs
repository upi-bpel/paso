// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv =
    let data_path = @"D:\Dropbox\Code\ConsoleApplication1\Data\"
    let data_path = @"C:\Users\pq\Documents\GitHub\bpel_to_fsharp\Data\"
    let mutable BPEL_path = sprintf "%sflow.bpel" data_path
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