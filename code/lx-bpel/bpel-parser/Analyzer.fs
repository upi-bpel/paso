module Analyzer
open System.Xml
open lx_bpel
open Probability

let tuple2 a b = (a,b)

[<Struct>]
type Link =
    val mutable name:string
    val mutable transitionCondition:string
    val mutable source:string
    val mutable target:string


let Update_List (mylink:Link System.Collections.Generic.List) (t:Link) = 
    let index = mylink.FindIndex(fun y -> y.name = t.name)
    let mutable x = mylink.[index]

    if x.transitionCondition = null then
        x.transitionCondition <- t.transitionCondition

    if x.source = null then
        x.source <- t.source

    if x.target = null then
        x.target <- t.target

    mylink.[index] <- x
    
type Analyzer () =
    member x.TransverseNodesActivity 
        (nodes:XmlNodeList)
        (linkList:System.Collections.Generic.List<Link>)
        parentName =
        let mutable activityList = System.Collections.Generic.List<lx_bpel.Activity>()
        for node in nodes do
            match node.Name.ToLower() with
            | "source" | "target" ->
                let mutable t = Link()
                t.name <- node.Attributes.["linkName"].Value

                if node.Name = "source" then
                    t.source <- parentName
                else if node.Name = "target" then
                    t.target <- parentName

                //it may or may not contain the Transition Condition
                if node.ChildNodes.Count > 0 then 
                    t.transitionCondition <- node.ChildNodes.[0].InnerText
                else
                    t.transitionCondition <- null

                Update_List linkList t
            | "link" ->
                let mutable t = Link()
                t.name  <- node.Attributes.["name"].Value

            | _ -> ()
        ()

    member x.TraverseNodes (nodes:XmlNodeList) (linkList:System.Collections.Generic.List<Link>): System.Collections.Generic.List<lx_bpel.Activity> =
        let temp = System.Collections.Generic.List<lx_bpel.Activity>()
        let r = new System.Random ()

        let emptyList = System.Collections.Generic.List<Link>()
        for node in nodes do
            let nodeName = node.Name.ToLower()
            let parentName = sprintf "%s%d" nodeName (r.Next())
            match nodeName with
            | "sequence" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName
                x.TraverseNodes(node.ChildNodes) emptyList
                |> Eval.makeSequence
                |> temp.Add
            | "if" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName
                let temp_List = Probability.getConditionProbability node.ChildNodes.[0].InnerText 1
                let thenL = x.TraverseNodes node.ChildNodes.[1].ChildNodes emptyList
                let thenA = if thenL.Count = 0 then Nothing else thenL.[0]
                let elseL = x.TraverseNodes node.ChildNodes.[2].ChildNodes emptyList
                let elseA = if elseL.Count = 0 then Nothing else elseL.[0]
                let variableName = sprintf "Var%d" <| r.Next()
                (variableName,temp_List.[0].success)
                |> OpaqueAssign
                |> temp.Add
                (Variable variableName,thenA,elseA)
                |> IfThenElse
                |> temp.Add
            | "while" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                let pchild = seq{ for x in node.ChildNodes -> x } |> Seq.find (fun x -> x.Name.ToLower() = "condition") 
                let temp_List = Probability.getConditionProbability pchild.InnerText 1
                let variableName = sprintf "Var%d" <| r.Next()
                let opaqueAssignActivity =
                    temp_List
                    |> Seq.filter (fun l -> not <| System.Double.IsNaN l.success)
                    |> Seq.sumBy (fun l -> l.success)
                    |> tuple2 variableName
                    |> OpaqueAssign 
                let while_List = x.TraverseNodes node.ChildNodes.[1].ChildNodes emptyList
                
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
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                Probability.getInvokeSamplingFunction node.Attributes.["partnerLink"].Value
                |> Invoke
                |> temp.Add
            | "empty" -> 
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                temp.Add Nothing
            | "link" -> ()
            | "flow" -> 
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                //activityName,joinCondition,innerActivity

                //XmlNode t3 = Sort_Activities.sort(node);
                let linkList = System.Collections.Generic.List<Link>()
                let activityList =
                    x.TraverseNodes node.ChildNodes linkList 
                    |> Seq.map (fun a -> "weneedaname",lx_bpel.Constant true,a )
                    |> Seq.toList
                linkList
                |> Seq.map (fun (l:Link) -> (l.name,lx_bpel.Variable l.transitionCondition,l.source,l.target))
                |> Seq.toList
                |> tuple2 activityList
                |> lx_bpel.Flow
                |> temp.Add
            | _ ->
                //printf "unrecognized activity: %s" activityName
                x.TraverseNodes node.ChildNodes linkList
                |> temp.AddRange
        temp