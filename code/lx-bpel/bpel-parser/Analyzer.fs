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
    
type Analyzer (probabilityAnnotations:Probability.ProbabilityAnnotation) =
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

    member x.TraverseNodes (nodes:XmlNodeList) (linkList:System.Collections.Generic.List<Link>): System.Collections.Generic.List<string*lx_bpel.Activity> =
        let temp = System.Collections.Generic.List<string*lx_bpel.Activity>()
        let r = new System.Random ()

        let emptyList = System.Collections.Generic.List<Link>()
        for node in nodes do
            let nodeName = node.Name.ToLower()
            let parentName = sprintf "%s%d" nodeName (r.Next())
            match nodeName with
            | "sequence" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName
                x.TraverseNodes(node.ChildNodes) emptyList
                |> Seq.map snd
                |> Eval.makeSequence
                |> tuple2 parentName
                |> temp.Add
            | "if" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName
                let probability =  probabilityAnnotations.conditions.[node.ChildNodes.[0].InnerText]
                let thenL = x.TraverseNodes node.ChildNodes.[1].ChildNodes emptyList
                let thenA = if thenL.Count = 0 then Nothing else snd thenL.[0]
                let elseL = x.TraverseNodes node.ChildNodes.[2].ChildNodes emptyList
                let elseA = if elseL.Count = 0 then Nothing else snd elseL.[0]
                let variableName = sprintf "Var%d" <| r.Next()
                (variableName,probability)
                |> OpaqueAssign
                |> tuple2 parentName
                |> temp.Add
                (Variable variableName,thenA,elseA)
                |> IfThenElse
                |> tuple2 parentName
                |> temp.Add
            | "while" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                let pchild = seq{ for x in node.ChildNodes -> x } |> Seq.find (fun x -> x.Name.ToLower() = "condition") 
                let probability = probabilityAnnotations.conditions.[pchild.InnerText]
                let variableName = sprintf "Var%d" <| r.Next()
                let opaqueAssignActivity =
                    OpaqueAssign (variableName,probability)
                let while_List = x.TraverseNodes node.ChildNodes.[1].ChildNodes emptyList
                
                temp.Add (parentName,opaqueAssignActivity)
                if while_List.Count = 0 then
                    opaqueAssignActivity
                else
                    while_List.Add (parentName,opaqueAssignActivity)
                    Eval.makeSequence (Seq.map snd while_List)
                |> tuple2 (Variable variableName)
                |> While
                |> tuple2 parentName
                |> temp.Add
            | "invoke" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName
                match Map.tryFind (node.Attributes.["name"].Value+"_pl_"+node.Attributes.["partnerLink"].Value) probabilityAnnotations.endpoints with
                | Some (list,samplingFunction) ->
                    Invoke samplingFunction
                    |> tuple2 parentName
                    |> temp.Add
                | None -> temp.Add (parentName,lx_bpel.Nothing)
            | "empty" -> 
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                temp.Add (parentName,Nothing)
            | "link" -> ()
            | "flow" -> 
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                //activityName,joinCondition,innerActivity

                //XmlNode t3 = Sort_Activities.sort(node);
                let linkList = System.Collections.Generic.List<Link>()
                let activityList =
                    x.TraverseNodes node.ChildNodes linkList 
                    |> Seq.map (fun (name,activity) -> name,lx_bpel.Constant true,activity )
                    |> Seq.toList
                let linkList =
                    linkList
                    |> Seq.map (fun (l:Link) -> (l.name,lx_bpel.Variable l.transitionCondition,l.source,l.target))
                    |> Seq.toList
                let activityList = lx_bpel.Eval.activityToposort (activityList) linkList

                lx_bpel.Flow (activityList,linkList)
                |> tuple2 parentName
                |> temp.Add
            | _ ->
                //printf "unrecognized activity: %s" activityName
                x.TraverseNodes node.ChildNodes linkList
                |> temp.AddRange
        temp