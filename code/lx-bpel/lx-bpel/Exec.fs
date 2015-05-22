namespace lx_bpel
module utility =
    let invtuple2 a b = (b,a)

//type TMonad<'V> = unit -> 'V
//type DMonad () =
//    let g = new System.Random()
//    member x.Return(v:'T): TMonad<'T> = fun () -> v
//    member x.ReturnFrom(v:TMonad<'T>) = v
//    member x.Bind(v:TMonad<'T>,f:'T->TMonad<'U>) :TMonad<'U> = fun () ->
//         f (v ()) ()
//        
//
//    member x.flip (v): TMonad<bool> = fun () ->
//        g.NextDouble() < v   //Assign True if g is less than v (i.e. Probability assign to condition)
type TMonad<'V> = Integration.D<'V>
type DMonad = Integration.IntegratorBuilder

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
    | Invoke of TMonad<Outcome*Cost>
    | Flow of ((string*BoolExpr*Activity) list)*((string*BoolExpr*string*string) list)



module Eval =
    let dist = DMonad.IntegratorBuilder

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

    let rec FlowMatch (env:Map<string,bool>) activityList linkList :TMonad<Map<string,bool>*Outcome*Cost> =
        let JoinOutcomesAndCosts outcomes costs names =
            let o,c =
                [for n in names -> Map.find n outcomes, Map.find n costs] |> List.unzip
            (AllOutcome o,All c)
        let linkTargetIs t (_,_,_,target) = (target = t)
        let linkSourceIs s (_,_,source,_) = (source = s)
        let rec loopp1 (env,activitiesOutcomes,delayedCosts) alist =
            match alist with
            | [] -> dist.Return (env,activitiesOutcomes,delayedCosts)
            | (activityName,joinCondition,innerActivity)::t ->
                let incomingLinks = List.filter (linkTargetIs activityName) linkList

                let dependencies  = [ for _,_,source,_ in incomingLinks -> source ]
                let dependenciesOutcome,dependenciesCost =
                    JoinOutcomesAndCosts activitiesOutcomes delayedCosts dependencies
                let isExecuted = (dependenciesOutcome = Success) && boolExprEval env joinCondition
                dist {
                    let! parms =
                        if isExecuted then
                            Exec env innerActivity
                        else
                            dist.Return(env,dependenciesOutcome,Zero)
                    return! cont dependenciesCost activityName (env,activitiesOutcomes,delayedCosts) parms t
                }
        and cont dependenciesCost activityName (env,activitiesOutcomes,delayedCosts) (innerModifiedEnv,innerOutcome,innerCost) alist =
                let outgoingLinks = List.filter (linkSourceIs activityName) linkList
                let env =
                    Seq.fold (fun e (name,transitionCondition,_,_) ->
                        Map.add name (boolExprEval innerModifiedEnv transitionCondition) e )
                        innerModifiedEnv outgoingLinks
                let activitiesOutcomes = Map.add activityName innerOutcome activitiesOutcomes
                let delayedCosts = Map.add activityName (Delay(innerCost,dependenciesCost)) delayedCosts
                loopp1 (env,activitiesOutcomes,delayedCosts) alist
        dist {
            let! newEnv,activitiesOutcomes,delayedCosts = loopp1 (env,Map.empty,Map.empty) activityList
            let flowOutcome,flowCost =
                [ for name,_,_ in activityList -> name]
                |> JoinOutcomesAndCosts activitiesOutcomes delayedCosts
            return newEnv,flowOutcome,flowCost
        }
    and Exec (env:Map<string,bool>) : Activity -> TMonad<_> = function             ////exec
    | Nothing -> dist.Return(env,Success,Zero)
    | Throw -> dist.Return(env,Fault,Zero)
    | Sequence ([]) -> dist.Return(env,Success,Zero)
    | Sequence (h::t) ->
        dist {
            let! newEnv,outcome,cost = Exec env h
            if outcome = Success then
                let! newerEnv,outcome, newCost = Exec newEnv (Sequence t)
                return newerEnv,outcome,Both(cost,Delay(newCost,cost))            //val for both and delay is calculated here           
            else
                return newEnv,outcome,cost
        }
    | Scope (activity,handler) ->
        dist {
            let! newEnv,outcome,cost = Exec env activity
            if outcome = Fault then
                let! newerEnv,outcome, newCost = Exec newEnv handler
                return newerEnv,outcome,Both(cost,Delay(newCost,cost))
            else
                return newEnv,outcome,cost
        }
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
            dist.Return(env,Success,Zero)
    | Assign (name,expr) ->
        dist.Return (env.Add(name,boolExprEval env expr),Success,Zero)
    | OpaqueAssign (name,prob) ->
        let p = dist.flip prob
        dist {
            let! p = p
            return env.Add(name,p),Success,Zero
        }
    | Invoke (endpointDist) ->
        dist {
            let! outcome,cost = endpointDist
            return env,outcome,cost
        }
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

        dist {
            let folder accum (activityName,joinCondition,innerActivity) = 
                dist {
                    let! (env,(delayedCosts:(Map<string,Cost>)),(activitiesOutcomes:Map<string,Outcome>)) = accum
                    let dependencies = [for l in links do let _,_, source,target = l in if target = activityName then yield source]
                    let dependenciesOutcome,dependenciesCosts =
                        allCostsOutcomes activitiesOutcomes delayedCosts dependencies 
                    let outgoingLinks = [for l in links do let name,condition, source,_ = l in if source = name then yield name,condition]
                    if dependenciesOutcome = Success && boolExprEval env joinCondition then
                        let! env,outcome,cost = Exec env innerActivity
                        return
                            outgoingLinks |> (Seq.fold (fun (env:Map<string,bool>) (name,transCond) -> env.Add(name,boolExprEval env transCond) ) env),
                            delayedCosts.Add(activityName,Delay(cost,dependenciesCosts)),
                            activitiesOutcomes.Add(activityName,outcome)
                       
                    else
                        return 
                            env,
                            delayedCosts.Add(activityName,Delay(Zero,dependenciesCosts)),
                            activitiesOutcomes.Add(activityName,dependenciesOutcome)
                }
            let startState = dist.Return ((env,Map.empty,Map.empty))
            let! env,allDelayedCosts,allActivitiesOutcomes = Seq.fold folder startState activities

            let outcome,cost = allCostsOutcomes allActivitiesOutcomes allDelayedCosts (activities |> List.map (fun (a,_,_)->a))
            return env,outcome,cost
        }

    let outcomeFromProbabilities : (_ -> TMonad<Outcome*Cost>) =
        fun (plist:(float*(Outcome*Cost)) list) ->
            let norm = 1.0/(List.sumBy fst plist)
            let nlist = List.map (fun (p,v) -> norm*p,v) plist
            //let plist,vlist = List.unzip plist
            //let cprob = plist |> List.map ((*)norm) |>  List.scan (+) 0.0 |> List.tail
            //let cpv = List.zip cprob vlist
            Integration.Integrator <| fun f s a ->
                nlist |> Seq.map (fun (scale,v)-> s scale <| f v) |> Seq.reduce a

    let ldist (plist:('T *float) list) : TMonad<'T> =
        let norm = 1.0/(List.sumBy snd plist)
        let nlist = List.map (fun (v,p) -> norm*p,v) plist
        Integration.Integrator <| fun f s a ->
            nlist |> Seq.map (fun (scale,v)-> s scale <| f v) |> Seq.reduce a
    let listlr (d:Integration.D<'T>) =
        let smap (v:'T) =
            Map.empty.Add(v,1.0)
        let rec smul scale m =
            Map.map (fun v weight -> weight*scale) m
        let ssum m1 m2 =
            let fder acc key value =
                match Map.tryFind key acc with
                | None ->  Map.add key value acc
                | Some v -> Map.add key (v  + value) acc
            Map.fold fder m1 m2
        d.intfun smap smul ssum
        |> Map.toList
    let flattn (d:Integration.D<'T>) : Integration.D<'T> =
        listlr d
        |> ldist
    let monter itcount (d:Integration.D<'T>) =
        let mapper (x:'T) = Seq.singleton (1.0,x)
        let multp s x = Seq.map (fun (x,(y:'T)) -> (x*s,y)) x
        let sump = Seq.map fst >> Seq.reduce (+)
        let collect a b =
            Seq.append a b 
            |> Seq.groupBy snd 
            |> Seq.map (fun (k,v) -> sump v,k) 
        let probs =
            d.intfun 
                <| mapper
                <| multp
                <| collect
        let probs = Seq.sortBy fst probs |> Seq.toList
        let plist,vlist = List.unzip probs
        let cprob = plist |> List.scan (+) 0.0 |> List.tail
        let cpv = List.zip cprob vlist
        Array.init itcount <| fun i -> List.find (fun (cp,v) -> cp > ((float i) / (float itcount))) cpv |> snd
             
    
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
        
    let riskSamplingFun :TMonad<Outcome*Cost> =
        dist {
            let! f = dist.flip 0.01
            if f then
                return Stuck,(0.1,0.0)
            else
                let! f = dist.flip (0.69/0.99) 
                if f then
                    return Success,(0.1,1.0)
                else
                    return Success,(0.1,2.0)
        }
    let approvalSamplingFun =
        dist {
            let! f = dist.flip 0.15
            if f then
                return Fault,(0.0,300.0)
            else
                let! f = dist.flip (0.3/0.85)
                if f then
                    return Success,(5.0,600.0)
                else
                let! f = dist.flip (0.35/0.55)
                if f then
                    return Success,(10.0,1200.0)
                else
                    return Success,(15.0,1800.0)
        }
    let receiveBlock = OpaqueAssign ("bigAmount",0.5)
    let riskAssessBlock = IfThenElse(Variable "bigAmount",Assign("highRisk",Constant false),Sequence([Invoke(riskSamplingFun);OpaqueAssign("highRisk",0.6)]))
    let approvalBlock = 
        IfThenElse(Or(Variable "bigAmount",Variable "highRisk"),
            Sequence([Assign("whileGuard",Constant true);
                     While(Variable "whileGuard",
                           Scope(Sequence([Invoke approvalSamplingFun;Assign("whileGuard",Constant false)]),Nothing))]),Nothing)
    let total = Sequence([receiveBlock;riskAssessBlock;approvalBlock])