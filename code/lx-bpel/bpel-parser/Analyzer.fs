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

type Tokens =
    | TkNot
    | TkAnd
    | TkOr
    | TkOpenPar
    | TkClosedPar
    | TkVariable of string

type Analyzer (probabilityAnnotations:Probability.ProbabilityAnnotation) =
    member x.ParseCondition (expression:string) =
        let rec tokenize (s:char list) =
            match s with
            | 'n'::'o'::'t'::tail -> TkNot::(tokenizeSeparator tail)
            | 'a'::'n'::'d'::tail -> TkAnd::(tokenizeSeparator tail)
            | 'o'::'r'::tail -> TkOr::(tokenizeSeparator tail)
            | '$'::tail -> variableToken "" tail
            | _ -> tokenizeSeparator s
        and tokenizeSeparator (s:char list) =
            match s with
            | ' '::tail -> tokenize tail
            | '('::tail -> TkOpenPar::tokenize tail
            | ')'::tail -> TkClosedPar::tokenize tail
            | [] -> []
            | _ -> failwith "parse error"
        and variableToken (varname) (s:char list) =
            match s with
            | l::tail when l <> ' ' && l <> '('  && l <> ')' -> variableToken (sprintf "%s%c" varname l) tail
            | _ -> TkVariable varname::tokenizeSeparator s

        let tokenizedExpr = tokenize (expression |> Seq.toList)
        //printf "%+A" tokenizedExpr
        let rec parser tkns =
            match tkns with
            | TkOpenPar::tail ->
                let e,t = parser tail
                match t with
                | TkClosedPar::t2 -> binayOperatorParser e t2
                | _ -> failwith "Unbalanced parenthesis"
            | TkVariable s::tail ->
                binayOperatorParser (lx_bpel.Variable s) tail
            | TkNot::tail ->
                let e,t = parser tail
                binayOperatorParser (lx_bpel.BoolExpr.Not e) t
        and binayOperatorParser lhs tkns =
            match tkns with
            | TkAnd::tail ->
                let e,t = parser tail
                (lx_bpel.BoolExpr.And (lhs,e)),t
            | TkOr::tail ->
                let e,t = parser tail
                (lx_bpel.BoolExpr.Or (lhs,e)),t
            | [] -> lhs,[]
            | _ -> lhs,tkns 
        let parsedExpr,tail = parser tokenizedExpr
        match tail with
        | [] -> printf "\n%+A\n" parsedExpr
        | _ -> failwithf "trailing tokens %+A" tail

    member x.TransverseNodesActivity 
        (nodes:XmlNodeList)
        (linkList:System.Collections.Generic.List<Link>)
        parentName =
        let mutable activityList = System.Collections.Generic.List<lx_bpel.Activity>()
        for node in nodes do
            match node.Name.ToLower() with
            | "sources" | "targets" | "links" ->
                x.TransverseNodesActivity (node.ChildNodes) linkList parentName
            | "joincondition" ->
                x.ParseCondition (node.InnerText)             
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
                linkList.Add t
            | _ -> ()
        ()

    member x.TraverseNodes (nodes:XmlNodeList) (linkList:System.Collections.Generic.List<Link>): (lx_bpel.Activity list)*System.Collections.Generic.List<string*lx_bpel.Activity> =
        let temp = System.Collections.Generic.List<string*lx_bpel.Activity>()
        let mutable handler : lx_bpel.Activity list = []
        let r = new System.Random ()

        let emptyList = System.Collections.Generic.List<Link>()
        for node in nodes do
            let nodeName = node.Name.ToLower() 
            let parentName = sprintf "%s%d" nodeName (r.Next())
            match nodeName with
            | "faulthandlers" ->
                let newHandler =
                    let _,actlist =x.TraverseNodes (node.ChildNodes) linkList
                    actlist
                    |> Seq.map snd
                    |> Eval.makeSequence 
                handler <- List.Cons (newHandler,handler) // newHandler::handler
                // Some (Eval.makeSequence (Seq.map snd (snd(x.TraverseNodes (node.ChildNodes) linkList))))
            | "sequence" ->
                let seqActivity =
                    x.TransverseNodesActivity node.ChildNodes linkList parentName
                    let h,actlist = x.TraverseNodes(node.ChildNodes) linkList
                    handler <- List.append h handler //  h @ handler
                    actlist
                    |> Seq.map snd
                    |> Eval.makeSequence
                temp.Add (parentName,seqActivity)
            | "scope" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName
                let h,actlist = x.TraverseNodes(node.ChildNodes) linkList
                let seqActivity =
                    actlist
                    |> Seq.map snd
                    |> Eval.makeSequence
                let handlerActivity =
                    match h with
                    | [] -> lx_bpel.Throw
                    | a::[] -> a
                    | _ -> Eval.makeSequence h
                let scopeActivity = lx_bpel.Scope (seqActivity,handlerActivity)
                temp.Add (parentName,scopeActivity)
            | "if" ->
                x.TransverseNodesActivity node.ChildNodes linkList parentName
                let probability =  probabilityAnnotations.conditions.[node.ChildNodes.[0].InnerText]
                let h,thenL = x.TraverseNodes node.ChildNodes.[1].ChildNodes emptyList
                handler <- List.append h handler
                let thenA = if thenL.Count = 0 then Nothing else snd thenL.[0]
                let h,elseL = x.TraverseNodes node.ChildNodes.[2].ChildNodes emptyList
                handler <- List.append h handler
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
                let h, while_List = x.TraverseNodes node.ChildNodes.[1].ChildNodes emptyList
                handler <- List.append h handler
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
                let nameNode = node.Attributes.["name"]
                match Map.tryFind ((if nameNode <> null then nameNode.Value else "" )+"_pl_"+node.Attributes.["partnerLink"].Value) probabilityAnnotations.endpoints with
                | Some (list,samplingFunction) ->
                    Invoke samplingFunction
                    |> tuple2 parentName
                    |> temp.Add
                | None -> temp.Add (parentName,lx_bpel.Nothing)
            | "empty" | "assign" | "receive" | "reply" -> 
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                temp.Add (parentName,Nothing)
            | "link" -> ()
            | "flow" -> 
                x.TransverseNodesActivity node.ChildNodes linkList parentName

                //activityName,joinCondition,innerActivity

                //XmlNode t3 = Sort_Activities.sort(node);
                //let linkList = System.Collections.Generic.List<Link>()
                let activityList =
                    let h,activitylist = x.TraverseNodes node.ChildNodes linkList 
                    handler <- List.append h handler
                    activitylist
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
                let h,activityList = x.TraverseNodes node.ChildNodes linkList 
                handler <- List.append h handler
                temp.AddRange activityList
        handler,temp