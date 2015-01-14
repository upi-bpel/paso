namespace lx_bpel
module utility =
    let invtuple2 a b = (b,a)


type BoolExpr =
    | Constant of bool
    | Or of BoolExpr*BoolExpr
    | And of BoolExpr*BoolExpr
    | Not of BoolExpr
    | Variable of string

    
type Cost = float* float
    

type Outcome =
    | Success
    | Fault
    | Stuck
type Activity =
    | Nothing
    | Throw
    | Assign of string * BoolExpr
    | Sequence of Activity list
    | Scope of Activity * Activity
    | IfThenElse of BoolExpr * Activity * Activity
    | While of BoolExpr * Activity
    | OpaqueAssign of string*float
    | Invoke of (unit -> Outcome*Cost)
    | Flow of ((string*BoolExpr*Activity) list)*((string*BoolExpr*string*string) list)



module Eval =

    let makeSequence x =
        Seq.toList x |> Sequence   //Creates a list from the given collection.

    let rec PrintBoolExpr = function
        | Constant b -> if b then "True" else "False"
        | Or (b1,b2) -> sprintf "(%s||%s)" (PrintBoolExpr b1) (PrintBoolExpr b2)
        | And (b1,b2) -> sprintf "(%s&&%s)" (PrintBoolExpr b1) (PrintBoolExpr b2)
        | Not b -> sprintf "!%s" (PrintBoolExpr b)
        | Variable (vname) -> vname

    let rec PrintActivity = function
        | Nothing -> "Nothing"
        | Throw -> "Throw"
        | Assign (s , v) -> sprintf "Assign(%s <- %s)" s (PrintBoolExpr v) 
        | Sequence  l ->
            sprintf "Sequence(%A)" ( Seq.map PrintActivity l ) //Creates a new collection whose elements are the results of applying the given function to each of the elements of the collection
        | Scope  (ma,fh) -> sprintf "Scope(%s,handler:%s)"(PrintActivity ma) (PrintActivity fh)
        | IfThenElse  (guard,thenActivity,elseActivity) -> sprintf "If(%s,%s,%s)" (PrintBoolExpr guard) (PrintActivity thenActivity) (PrintActivity elseActivity) 
        | While  (guard,body) -> sprintf "While(%s,%s)" (PrintBoolExpr guard)  (PrintActivity body)
        | OpaqueAssign  (s , v) -> sprintf "OpaqueAssign(%s:%f%%)" s  (100.0 * v)
        | Invoke  func -> sprintf "Invoke(%+A)" func
        | Flow (a1,a2)-> sprintf "Flow(\n\t%+A\n\t%+A)" a1 a2 

    
    let flip =
        let g =new System.Random()
        fun v () -> g.NextDouble() < v   //Assign True if g is less than v (i.e. Probability assign to condition)
        
    let Zero = 0.0,0.0:Cost
    let Both(c1:Cost,c2:Cost) =
        let p1,t1 = c1
        let p2,t2 = c2
        (p1 + p2), (max t1 t2):Cost
    let Delay(c1:Cost,c2:Cost) =
        let p1,t1 = c1
        let p2,t2 = c2
        (p1 ), ( t1 + t2):Cost

    let All =
        Seq.fold (fun a b -> Both(a,b)) Zero   //Applies a function to each element of the collection

    let rec All2 = function
    | [] -> Zero
    | h::t ->  Both(h,(All2 t))
    
    let rec AllOutcome = function
    | [] -> Success
    | h::t ->
        match h with
        | Fault -> Fault
        | Success -> AllOutcome t
        | Stuck -> if AllOutcome t = Fault then Fault else Stuck

    let AllOutcome2 oList =
        if (Seq.exists ((=) Fault) oList) then Fault
        else if (Seq.exists ((=) Stuck) oList) then Stuck
        else Success


    let rec boolExprEval (env:Map<string,bool>) = function
        | Constant b -> b
        | Or (e1,e2) ->
            let v1 = boolExprEval env e1
            let v2 = boolExprEval env e2
            v1 || v2
        | And (e1,e2) ->
            let v1 = boolExprEval env e1
            let v2 = boolExprEval env e2
            v1 && v2
        | Not (e) ->
            let e = boolExprEval env e
            not e
        | Variable (name) -> env.[name]

    let rec FlowMatch (env:Map<string,bool>) activityList linkList =
        let linkTargetIs t (_,_,_,target) = (target = t)
        let linkSourceIs s (_,_,source,_) = (source = s)
        let JoinOutcomesAndCosts outcomes costs names =
            let o,c =
                [for n in names -> Map.find n outcomes, Map.find n costs] |> List.unzip
            (AllOutcome o,All c)
        let mutable env,activitiesOutcomes,delayedCosts =
            env,Map.empty,Map.empty
        for activityName,joinCondition,innerActivity in activityList do
            let incomingLinks = List.filter (linkTargetIs activityName) linkList
            let outgoingLinks = List.filter (linkSourceIs activityName) linkList
            let dependencies  = [ for _,_,source,_ in incomingLinks -> source ]
            let dependenciesOutcome,dependenciesCost =
                JoinOutcomesAndCosts activitiesOutcomes delayedCosts dependencies
            let isExecuted = (dependenciesOutcome = Success) && boolExprEval env joinCondition
            let innerModifiedEnv,innerOutcome,innerCost =
                if isExecuted then
                    Exec env innerActivity
                else
                    env,dependenciesOutcome,Zero
            env <- Seq.fold (fun e (name,transitionCondition,_,_) ->
                Map.add name (boolExprEval innerModifiedEnv transitionCondition) e )
                innerModifiedEnv outgoingLinks
            activitiesOutcomes <- Map.add activityName innerOutcome activitiesOutcomes
            delayedCosts <- Map.add activityName (Delay(innerCost,dependenciesCost)) delayedCosts
        let flowOutcome,flowCost =
            [ for name,_,_ in activityList -> name]
            |> JoinOutcomesAndCosts activitiesOutcomes delayedCosts
        env,flowOutcome,flowCost
    and Exec (env:Map<string,bool>) = function             ////exec
    | Nothing -> env,Success,Zero
    | Throw -> env,Fault,Zero
    | Sequence ([]) -> env,Success,Zero
    | Sequence (h::t) ->
        let newEnv,outcome,cost = Exec env h
        if outcome = Success then
            let newerEnv,outcome, newCost = Exec newEnv (Sequence t)
            newerEnv,outcome,Both(cost,Delay(newCost,cost))            //val for both and delay is calculated here           
        else
            newEnv,outcome,cost
    | Scope (activity,handler) ->
        let newEnv,outcome,cost = Exec env activity
        if outcome = Fault then
            let newerEnv,outcome, newCost = Exec newEnv handler
            newerEnv,outcome,Both(cost,Delay(newCost,cost))
        else
            newEnv,outcome,cost
    | IfThenElse(guard,thenActivity,elseActivity) ->
        let guardValue = boolExprEval env guard
        if guardValue then
            Exec env thenActivity
        else
            Exec env elseActivity
    | While(guard,body) ->
        let guardValue = boolExprEval env guard
        if guardValue then
            Exec env (Sequence ([body;While(guard,body)]))
        else
            env,Success,Zero
    | Assign (name,expr) ->
        env.Add(name,boolExprEval env expr),Success,Zero
    | OpaqueAssign (name,prob) ->
        env.Add(name,flip prob ()),Success,Zero
    | Invoke (samplingFun) ->
        let outcome,cost = samplingFun() //possible error here since it always equal to success..never to fault
        env,outcome,cost
    | Flow (activities,links) ->
        // sort activities so that for each link its source is before its target
        //let activities = topoSort activities
        let allCostsOutcomes (activityOutcomes:Map<string,Outcome>) (delayedCosts:(Map<string,Cost>)) =
            List.fold (fun (o,c) n ->
                            let o =
                                match o,(activityOutcomes.[n]) with
                                | Fault,_ | _,Fault -> Fault
                                | Success,Success -> Success
                                | _ -> Stuck
                            o,Both(c,(delayedCosts.[n]))
                            ) (Success,Zero)
        let env,allDelayedCosts,allActivitiesOutcomes =
            activities |> Seq.fold (fun (env,(delayedCosts:(Map<string,Cost>)),(activitiesOutcomes:Map<string,Outcome>)) (activityName,joinCondition,innerActivity) ->
                let dependencies = [for l in links do let _,_, source,target = l in if target = activityName then yield source]
                let dependenciesOutcome,dependenciesCosts =
                    allCostsOutcomes activitiesOutcomes delayedCosts dependencies 
                let outgoingLinks = [for l in links do let name,condition, source,_ = l in if source = name then yield name,condition]
                if dependenciesOutcome = Success && boolExprEval env joinCondition then
                    let env,outcome,cost = Exec env innerActivity
                    outgoingLinks |>
                    (Seq.fold (fun env (name,transCond) -> env.Add(name,boolExprEval env transCond)) env),
                        delayedCosts.Add(activityName,Delay(cost,dependenciesCosts)),
                        activitiesOutcomes.Add(activityName,outcome)
                else
                    env,
                    delayedCosts.Add(activityName,Delay(Zero,dependenciesCosts)),
                    activitiesOutcomes.Add(activityName,dependenciesOutcome)
                ) (env,Map.empty,Map.empty)
        let outcome,cost = allCostsOutcomes allActivitiesOutcomes allDelayedCosts (activities |> List.map (fun (a,_,_)->a))
        env,outcome,cost

    let outcomeFromProbabilities =
        let g = new System.Random()
        fun (plist:(float*(Outcome*Cost)) list) ->
            let norm = 1.0/(List.sumBy fst plist)
            let plist,vlist = List.unzip plist
            let cprob = plist |> List.map ((*)norm) |>  List.scan (+) 0.0 |> List.tail
            let cpv = List.zip cprob vlist
            fun () ->
                let number = g.NextDouble()
                List.find (fun (x,y) -> x > number) cpv |> snd




    
    let activityToposort (activities:(string*BoolExpr*Activity) seq) (links:(string*BoolExpr*string*string) seq) =
        //let mutable nodemark = Map.empty
        //for (name,guard,activity) in activities do
        //    let key,value = name,(guard,activity,ref false)
        //    nodemark <- Map.add key value nodemark
        let nodemark = 
            activities
            |> Seq.map (fun (name,guard,activity) -> name,(guard,activity, ref false))
            |> Seq.fold (fun map (key,value) -> Map.add key value map) Map.empty

        //seq{0..100} |> Seq.fold (fun st num -> st + num) 0
        //seq{0..100} |> Seq.fold (+) 0

        let neighborhoods = 
            links
            |> Seq.groupBy (fun (name,guard,source,target) -> source)
            |> Seq.fold (fun map (key,value) -> Map.add key (Seq.toList value) map) Map.empty



        let rec visit tail nodename =
            let guard,activity,mark = Map.find nodename nodemark
            if not !mark then
                mark := true
                match Map.tryFind nodename neighborhoods with
                | Some n -> 
                    let neighborhood = n |> Seq.map (fun (name,guard,source,target) -> target)|> Seq.toList
                    //let mutable tail = tail
                    //for target in neighborhood do
                    //    tail <- visit tail target
                    let tail2 = Seq.fold visit tail neighborhood

                    (nodename,guard,activity)::tail2
                | None -> (nodename,guard,activity)::tail
            else
                tail


        nodemark |> Map.fold (fun t name (guard,activity,mark) -> visit t name) []
        
    let riskSamplingFun () =
        if flip 0.01 () then
            Stuck,(0.1,0.0)
        else if flip (0.69/0.99) ()then
            Success,(0.1,1.0)
        else
            Success,(0.1,2.0)

    let approvalSamplingFun() =
        if flip 0.15 ()then
            Fault,(0.0,300.0)
        else if flip (0.3/0.85) () then
            Success,(5.0,600.0)
        else if flip (0.35/0.55) () then
            Success,(10.0,1200.0)
        else
            Success,(15.0,1800.0)
            
    let receiveBlock = OpaqueAssign ("bigAmount",0.5)
    let riskAssessBlock = IfThenElse(Variable "bigAmount",Assign("highRisk",Constant false),Sequence([Invoke(riskSamplingFun);OpaqueAssign("highRisk",0.6)]))
    let approvalBlock = 
        IfThenElse(Or(Variable "bigAmount",Variable "highRisk"),
            Sequence([Assign("whileGuard",Constant true);
                     While(Variable "whileGuard",
                           Scope(Sequence([Invoke approvalSamplingFun;Assign("whileGuard",Constant false)]),Nothing))]),Nothing)
    let total = Sequence([receiveBlock;riskAssessBlock;approvalBlock])