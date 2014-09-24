module Analyzer
open System.Xml
open lx_bpel
open Probability

let tuple2 a b = (a,b)

type Analyzer () =
    member x.TraverseNodes (nodes:XmlNodeList) : System.Collections.Generic.List<lx_bpel.Activity>=
        let temp = System.Collections.Generic.List<lx_bpel.Activity>()
        let r = new System.Random 4
        for node in nodes do
            match node.Name.ToLower() with
            | "sequence" ->
                x.TraverseNodes(node.ChildNodes)
                |> Eval.makeSequence
                |> temp.Add
            | "if" ->
                let temp_List = Probability.getConditionProbability node.ChildNodes.[0].InnerText 1
                let thenL = x.TraverseNodes node.ChildNodes.[1].ChildNodes
                let thenA = if thenL.Count = 0 then Nothing else thenL.[0]
                let elseL = x.TraverseNodes node.ChildNodes.[2].ChildNodes
                let elseA = if elseL.Count = 0 then Nothing else elseL.[0]
                let variableName = sprintf "Var%d" <| r.Next()
                (variableName,temp_List.[0].success)
                |> OpaqueAssign
                |> temp.Add
                (Variable variableName,thenA,elseA)
                |> IfThenElse
                |> temp.Add
            | "while" ->
                let temp_List = Probability.getConditionProbability node.ChildNodes.[0].InnerText 1
                let variableName = sprintf "Var%d" <| r.Next()
                let opaqueAssignActivity =
                    temp_List
                    |> Seq.filter (fun l -> not <| System.Double.IsNaN l.success)
                    |> Seq.sumBy (fun l -> l.success)
                    |> tuple2 variableName
                    |> OpaqueAssign 
                let while_List = x.TraverseNodes node.ChildNodes.[1].ChildNodes
                
                temp.Add opaqueAssignActivity
                if while_List.Count = 0 then
                    opaqueAssignActivity
                else
                    while_List.Add opaqueAssignActivity
                    Eval.makeSequence while_List
                |> tuple2 (Variable variableName)
                |> While
                |> temp.Add
            | "invoke" ->
                Probability.getInvokeSamplingFunction node.Attributes.["partnerLink"].Value
                |> Invoke
                |> temp.Add
            | "empty" -> temp.Add Nothing
            | "flow" -> ()
            | _ ->
                x.TraverseNodes node.ChildNodes 
                |>temp.AddRange
        temp