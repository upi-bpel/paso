module lx_bpel.compiler

type DiscreteDistribution<'T> (probabilities: ('T*float) list) =
    let normalization = probabilities |> Seq.map snd |> Seq.sum
    member x.Probabilities = probabilities
    member x.Map (f:'T->'U) =
        let newProbabilities =
            probabilities
            |> Seq.map (fun (x,p) -> (f x),p) //apply function to each possible value in this distribution's domain
            |> Seq.groupBy fst //group together results which are equal
            |> Seq.map (fun (x,s) -> //for each result
                let p = s |> Seq.map snd |> Seq.sum //sum probabilities
                x,p) //then yield result and probabilities
            |> Seq.toList //convert into a list
        new DiscreteDistribution<'U> (newProbabilities)
    member x.MMap (f:'T->DiscreteDistribution<'U>) =
        let newProbabilities =
            probabilities
            |> Seq.collect (fun (x,p1) ->
                (f x).Probabilities
                |> Seq.map (fun (y,p2) -> y,p1*p2 ) ) //apply function to each possible value in this distribution's domain
            |> Seq.groupBy fst //group together results which are equal
            |> Seq.map (fun (x,s) -> //for each result
                let p = s |> Seq.map snd |> Seq.sum //sum probabilities
                x,p) //then yield result and probabilities
            |> Seq.toList //convert into a list
        new DiscreteDistribution<'U> (newProbabilities)


let always (x:'U) = new DiscreteDistribution<_>([x,1.0])
let bind (f:'U->DiscreteDistribution<'T>) (x:DiscreteDistribution<'U>) = x.MMap f

[<Struct>]
type QoS(time:float,price:float) =
    //properties:
    // a + b = b + a (commutative +)
    // a + ( b + c ) = (a + b) + c (associative +)
    // a + b = max(a,a+b) (order preserving +) (a + b > a)
    // (a.b).c = a.(b.c) (associative .)
    // max(a.b,b) = a.b (order preserving .) (a.b > b)
    // (a+b).c = max(a,b).c (left subaddictive .)
    // a.(b+c) = a.b + a.c (right distributive)
    [a;[b;c]]
    a + a.(b+b.c) //def
    a + a.b + a.b.c // right distributive
    [[a;b];c]
    (a+a.b) + (a+a.b).c //def
    a + a.b + max(a,a.b).c //left subaddictive .
    a + a.b + a.b.c //order preserving .
    static member Top = QoS(0.0,0.0)
    member x.Time = time
    member x.Price = price
    member q1.Delay ( q2:QoS) =
        QoS(q1.Time + q2.Time,q1.Price)
    static member payBoth ( q1:QoS,q2:QoS) =
        QoS(max q1.Time q2.Time,q1.Price+q2.Price)
    // to solve concurrency issues. Probably not needed
    //static member payFaster ( q1:QoS,q2:QoS) =
    //    QoS(min q1.Time q2.Time,q1.Price+q2.Price)
    // When two possible costs can be paid, and we want to have worst case behavior.
    // WARNING: this is likely to be not compositional
    // and should be done on the final costs over all uncertain branches
    static member payMostExpensive ( q1:QoS,q2:QoS) = 
        QoS(max q1.Time q2.Time,max q1.Price q2.Price)

type Result =
    | True
    | False
    | Bottom

type BoolExpr =
    | TrueExp
    | FalseExp
    | Or of BoolExpr * BoolExpr
    | And of BoolExpr * BoolExpr
    | Not of BoolExpr
    //| Pick of BoolExpr * BoolExpr
    | Dep of string
    //| Var of string
and Activity =
    | Nothing of QoS
    | Throw
    | Scope of Activity * Activity //first is the inner scope, second is the False handler
    | Sequence of Activity * Activity
    | Invoke of Result*QoS 
    | IfThenElse of BoolExpr * Activity * Activity
    | While of BoolExpr * Activity
    //| VarSet of string * BoolExpr
    | Flow of (BoolExpr * Activity * string) list

type EnvType = Map<string,Result*QoS> 
let expEval exp ( env:EnvType ) = 
    let rec ee = function
    | TrueExp -> True,QoS.Top
    | FalseExp ->  False,QoS.Top
    | Dep activity -> env.[activity]
    //| Var activity -> env.variables.[activity]
    | Or (a,b) ->
        //we perform De Morgan transforms to get an And instead
        ee (Not (And(Not(a),Not(b))))
    | And (a,b) ->
        let a,qa = ee a
        let b,qb = ee b
        let r = match a,b with
                | True,True -> True
                | Bottom,_|_,Bottom -> Bottom
                | _ -> False 
        r,QoS.payBoth(qa,qb)
    | Not e ->
        let e,qe = ee e
        let r = match e with
                | True -> False
                | False -> True
                | Bottom -> Bottom
        r,qe
    (*| Pick (a,b) ->
        let a,qa = ee a
        let b,qb = ee b
        let r = match a,b with
                | False,False -> False
                | True,_|_,True -> True
                | _ -> Bottom 
        r,QoS.pickBehavior(qa,qb)*)
    ee exp


//our semantic is a True function plus a status transformation
let rec getQoS = function
    | Nothing q -> fun (env:EnvType) -> (True,q),env  // Nothing always succeeds, not changing status
    | Throw -> fun env -> (False, QoS.Top),env // throw always fails, not changing status
    | Scope (inner,faultHandler) ->
        let inner = getQoS inner
        let FalseHandler = getQoS faultHandler
        fun env ->
            match inner env with
            | (False,quality),env ->
                let (result,quality2),env = FalseHandler env
                (result,QoS.payBoth(quality,quality2.Delay(quality))),env
            | other -> other
             
    | Sequence  (a1,a2) ->
        let a1 = getQoS a1 //recu
        let a2 = getQoS a2
        fun env ->
            match a1 env with
            | (True,quality),env ->
                let (result,quality2),env = a2 env
                (result,QoS.payBoth(quality,quality2.Delay(quality))),env
            | other -> other
    | Flow l ->
        //we assume:
        // - activities in the list are ordered in respect to links
        // - no race condition, i.e. no variables name collision, variables are always defined in every path which could lead to an activity
        // - each activity which is supposed to set a link status will set a variable with the same name
        // //- activities which are link targets will instead be wrapped in a if() over the join condition
        let l = l |> List.map (fun (g,a,n) ->
            let a = getQoS a
            let g = expEval g
            fun env ->
                match g env with
                | Bottom,q -> (Bottom,q),env
                | False,q -> let r = (True,q) in r,env.Add(n,r) //shortcut rule
                | True,q ->
                    let (result,quality),env = a env
                    let r =  (result,quality.Delay(q))
                    r,env.Add(n,r)
            )
        let rec re = function
            | [] -> fun (env:EnvType) -> (True,QoS.Top),env
            | f::t ->
                let t = re t
                fun env ->
                let (r,q),env =  f env
                let (rt,qt),env = t env
                let r =
                    match r,rt with
                    | False, _| _,False -> False
                    | True,True -> True
                    | _ -> Bottom
                (r,QoS.payBoth(q,qt)),env
        re l
    | IfThenElse (g,t,e) ->
        let t = getQoS t
        let e = getQoS e
        let g = expEval g
        fun env ->
            let r,q = g env
            let (r2,q2),env =
                match r with
                | True -> t env
                | False -> e env
                | Bottom -> (Bottom,QoS.Top),env
            (r2,q2.Delay(q)),env
    | Invoke (r,q) ->
        fun env -> (r,q),env

    | While (g,b) ->
        let g = expEval g
        let b = getQoS b
        let rec whileSem previousQuality env=
            let guardValue,guardQuality = g env
            match guardValue with
            | True ->
                //run body
                let (bodyTrue,bodyQuality),env2 = b env
                //compose costs as sequence
                let q = QoS.payBoth(previousQuality,bodyQuality.Delay(previousQuality))
                match bodyTrue with
                | True -> whileSem q env2
                | other -> (other,q),env2
            | False -> (True,previousQuality),env
            | Bottom -> (Bottom,previousQuality),env
        whileSem QoS.Top



let invok = Invoke(True,QoS.Top)

let f = getQoS (invok)
let (r,q),e = f Map.empty
q.Time



// to evaluate an expression we need an environment which holds
// the pre-computed results of external invocations
let eval expr env =
    match expr with
    | Invoke (invocationId) ->
        // we just pick up the value from the environment
        env.[invocationId]
    | Sequence (a1,a2) ->
        // in a sequence we evaluate the second activity only if
        // the first is successful.
        let outcome1,cost1 = eval a1 env
        match outcome with
        | Success ->
            // the outcome of the sequence is the outcome of a2
            // the cost of the sequence is the cost of both a1 
            // and a2 delayed by a1 
            let outcome2,cost2 = eval a2 env
            outcome2, Both(cost1,Delay(cost2,cost1))
        | other ->
            // in any other case a2 is dead code, hence the
            // cost and outcome is the same as just a1
            other, cost1
    | Scope (mainActivity,faultHandler) ->
        // scope is used to define a fault handler.
        // Its control flow is the same as the sequence except
        // that it continues execution only when the first
        // activity is faulty
        let outcome1,cost1 = eval mainActivity env
        match outcome with
        | Fault ->
            // we execute fault handler paying its cost delayed
            // by the cost of the main activity
            let outcome2,cost2 = eval faultHandler env
            outcome2, Both(cost1,Delay(cost2,cost1))
        | other ->
            // we do not execute fault handler and keep the
            // main activity result
            other, cost1
    | If (branchId, thenActivity,elseActivity) ->
        // since guard evaluation depends on variables we can't
        // compute it. We will use an estimation from the environment
        if env.[branchId] then
            eval thenActivity env
        else
            eval elseActivity env
    | Flow (activityList) ->
        // We sort activities so that for each link the source
        // activity appears before the target activity. This
        // is possible because the link graph is acyclic
        let sortedActivities = Sort(activityList)
        // we evaluate each activity in order, delaying them to wait
        // for link to be set, and we store them in a structure
        let mutable linkStatus = Map.empty // maps a link to its status
        let mutable activityResults = Map.empty // map an activity to its result
        let mutable flowOutcome = Success
        let mutable flowCost = Cost.Zero
        for a in sortedActivities do
            // we compute the cost of dependencies
            let mutable dependenciesCost = Cost.Zero
            let mutable dependenciesOutcome = Success
            // for each link which we are target of
            for t in a.linkTargets do
                let depActivity = t.sourceActivity
                let outcome,cost = activityResults.[x]
                match dependenciesOutcome,outcome with
                | Fault,_ | _,Fault -> dependenciesOutcome <- Fault
                | Stuck,_ | _,Stuck -> dependenciesOutcome <- Stuck
                | Success,Success -> dependenciesOutcome <- Success
                dependenciesCost <- Cost.Both(dependenciesCost,cost)
            // if all dependencies are successful we can assume this activity
            // will be executed
            let mutable cost = Cost.Zero
            let mutable outcome = dependenciesOutcome
            if dependenciesOutcome = Success then
                // we suppose to have a booleanEval function to evaluate
                // the condition given the link status
                let condition = booleanEval (a.joinCondition) linkStatus
                if condition = true then
                    let o,c = a.activity
                    outcome <- o
                    cost <- c
                for l in a.linkSources do
                    linkStatus <- linkStatus.Add(l, env.[l.transitionCondition.branchId])
            activityResults <- activityResults.Add(a,(outcome,Cost.Delay(cost,dependenciesCost)))
            flowCost <- Cost.Both(flowCost,cost)
            match flowOutcome,outcome with
            | Fault,_ | _,Fault -> flowOutcome <- Fault
            | Stuck,_ | _,Stuck -> flowOutcome <- Stuck
            | Success,Success -> dependenciesOutcome <- Success
        flowOutcome,flowCost


//sampling code (naive version)
let availabilityOutcome () =
    let r = Random(0.0,1.0)
    if r < 0.02 then
        Fail
    else
        Success
        
let expectation projection expression =
    // costs are converted into summable property by a suitable projection expression
    let mutable sum = projection (Success,Cost.Zero)
    // we get bigNumber samples
    for i = 0 to bigNumber do
        let mutable env = Map.empty
        // for each invocation needed by the expression we are evaluating
        for invocationId in expression.listInvocationIds() do
            // generate a sample for the service invocation result
            // and store it into the environment
            env <- env.Add(invocationId,serviceDescription.[invocationId].getInvocationSample())
        // evaluate the expression, yielding a sample for outcome and cost
        // of the service orchestration
        let outcome,cost = eval expression env
        // use the projection function to get a summable quantity
        sum <- sum + (projection (outcome,cost))
    //divide the quantity by the number of samples generated, to yield the average
    sum / bigNumber

//Example: 