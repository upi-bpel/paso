namespace lx_bpel
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
    | Assign of string * BoolExpr
    | Sequence of Activity * Activity
    | Scope of Activity * Activity
    | IfThenElse of BoolExpr * Activity * Activity
    | While of BoolExpr * Activity
    | OpaqueAssign of string*float
    | Invoke of (unit -> Outcome*Cost)
    | Flow of ((string*BoolExpr*Activity) list)*((string*BoolExpr*string*string) list)

module Eval =
    
    let flip =
        let g =new System.Random()
        fun v -> g.NextDouble() < v
        
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
        Seq.fold (fun a b -> Both(a,b)) Zero

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
    and Exec (env:Map<string,bool>) = function
    | Nothing -> env,Success,Zero
    | Sequence (a1,a2) ->
        let newEnv,outcome,cost = Exec env a1
        if outcome = Success then
            let newerEnv,outcome, newCost = Exec newEnv a2
            newerEnv,outcome,Both(cost,Delay(newCost,cost))
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
            Exec env (Sequence (body,While(guard,body)))
        else
            env,Success,Zero
    | Assign (name,expr) ->
        env.Add(name,boolExprEval env expr),Success,Zero
    | OpaqueAssign (name,prob) ->
        env.Add(name,flip prob),Success,Zero
    | Invoke (samplingFun) ->
        let outcome,cost = samplingFun()
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
                    env,Map.empty,Map.empty
                ) (env,Map.empty,Map.empty)
        let outcome,cost = allCostsOutcomes allActivitiesOutcomes allDelayedCosts (activities |> List.map (fun (a,_,_)->a))
        env,outcome,cost

    let riskSamplingFun () =
        if flip 0.01 then
            Stuck,(0.1,0.0)
        else if flip (0.69/0.99) then
            Success,(0.1,1.0)
        else
            Success,(0.1,2.0)

    let approvalSamplingFun() =
        if flip 0.15 then
            Fault,(0.0,300.0)
        else if flip (0.3/0.85) then
            Success,(5.0,600.0)
        else if flip (0.35/0.55) then
            Success,(10.0,1200.0)
        else
            Success,(15.0,1800.0)
            
    let receiveBlock = OpaqueAssign ("bigAmount",0.5)
    let riskAssessBlock = IfThenElse(Variable "bigAmount",Assign("highRisk",Constant false),Sequence(Invoke(riskSamplingFun),OpaqueAssign("highRisk",0.6)))
    let approvalBlock = 
        IfThenElse(Or(Variable "bigAmount",Variable "highRisk"),
            Sequence(Assign("whileGuard",Constant true),
            While(Variable "whileGuard",
                Scope(Sequence(Invoke approvalSamplingFun,Assign("whileGuard",Constant false)),Nothing))),Nothing)
    let total = Sequence(Sequence(receiveBlock,riskAssessBlock),approvalBlock)