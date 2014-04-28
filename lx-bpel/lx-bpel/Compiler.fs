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
    static member Top = QoS(0.0,0.0)
    member private x.Time = time
    member private x.Price = price
    static member sequentialBehavior ( q1:QoS,q2:QoS) =
        QoS(q1.Time + q2.Time,q1.Price+q2.Price)
    static member parallelBehavior ( q1:QoS,q2:QoS) =
        QoS(max q1.Time q2.Time,q1.Price+q2.Price)
    static member pickBehavior ( q1:QoS,q2:QoS) =
        QoS(min q1.Time q2.Time,q1.Price+q2.Price)
    static member worseCaseBehavior ( q1:QoS,q2:QoS) = //not yet used
        QoS(max q1.Time q2.Time,max q1.Price q2.Price)


//carries both QoS and value information. Extends QoS type with bottom (Min) to represent nontermination and top value (Max), i.e. the QoS value for independent expressions
type Result =
    | Success
    | Fault
    | Stuck


//defines composition function for QoSStructural. Need not to be a type, it's just for collecting them together
type QoSComposer () =
    member x.After<'T,'U,'W> ((q:QoSStructural<'T>,(env:'W)),f:('T*'W)->(QoSStructural<'U>*'W)) =
        //after represent sequential dependance. 
        match q with
        | Max v -> f v
        | Min -> Min,env
        | Some (v,t) ->
            match f v with
            | Max v1,env -> Some (v1,t),env
            | Min,env -> Min,env
            | Some (v1,t1),env -> Some(v1,QoS.sequentialBehavior(t,t1)),env
    member x.After<'T,'U> (q:QoSStructural<'T>,f:'T->QoSStructural<'U>) =
        fst(x.After((q,()),(fun (x,_) -> (f x),())))
    member x.Return (b:'T) =
        //return represent values with maximum QoS, either success or failure.
        // Used for composition 
        Max b
    member x.Both (q1:QoSStructural<'T>,q2:QoSStructural<'U>,f:'T*'U->QoSStructural<'T>) =
        //both represents parallel (AND) dependence.
        match q1,q2 with
        | Min,_|_,Min -> Min
        | Max v1,e -> x.After (e,(fun v2 -> f (v1,v2)))
        | e,Max v2 -> x.After (e,(fun v1 -> f (v1,v2)))
        | Some(v1,t1),Some(v2,t2) -> x.After(Some((v1,v2),QoS.parallelBehavior(t1,t2)),f)
    member x.Any (q1:QoSStructural<bool>,q2:QoSStructural<bool>,f:bool->QoSStructural<'T>) =
        //any represents pick (parallel OR) dependance.
        match q1,q2 with
        | Max true,_ | _,Max true -> f true //if any of them is the "true" constant, the other one isn't even evaluated
        | Max false,e | e,Max false | Min,e| e,Min  -> x.After(e,f) // if any of them is false or bottom, it is just the behavior of the other branch
        | Some(v1,t1),Some(v2,t2) -> x.After(Some(v1 || v2,QoS.pickBehavior(t1,t2)),f )

let qos = new QoSComposer()

type BoolExpr =
    | True
    | False
    | Or of BoolExpr * BoolExpr
    | And of BoolExpr * BoolExpr
    | Not of BoolExpr
    | Pick of BoolExpr * BoolExpr
    | Dep of string
    //| Var of string
and Activity =
    | Nothing of QoS
    | Throw
    | Scope of Activity * Activity //first is the inner scope, second is the fault handler
    | Sequence of Activity * Activity
    | Invoke of Result*QoS 
    | IfThenElse of BoolExpr * Activity * Activity
    //| While of BoolExpr * Activity
    //| VarSet of string * BoolExpr
    | Flow of (BoolExpr * Activity * string) list

type EnvType = Map<string,Result*QoS> 
let expEval exp ( env:EnvType ) = 
    let rec ee = function
    | True -> Success,QoS.Top
    | False ->  Fault,QoS.Top
    | Dep activity -> env.[activity]
    //| Var activity -> env.variables.[activity]
    | Or (a,b) ->
        //we perform De Morgan transforms to get an And instead
        ee (Not (And(Not(a),Not(b))))
    | And (a,b) ->
        let a,qa = ee a
        let b,qb = ee b
        let r = match a,b with
                | Success,Success -> Success
                | Stuck,_|_,Stuck -> Stuck
                | _ -> Fault 
        r,QoS.parallelBehavior(qa,qb)
    | Not e ->
        let e,qe = ee e
        let r = match e with
                | Success -> Fault
                | Fault -> Success
                | Stuck -> Stuck
        r,qe
    | Pick (a,b) ->
        let a,qa = ee a
        let b,qb = ee b
        let r = match a,b with
                | Fault,Fault -> Fault
                | Success,_|_,Success -> Success
                | _ -> Stuck 
        r,QoS.pickBehavior(qa,qb)
    ee exp


//our semantic is a success function plus a status transformation
let rec getQoS = function
    | Nothing q -> fun (env:EnvType) -> (Success,q),env  // Nothing always succeeds, not changing status
    | Throw -> fun env -> (Fault, QoS.Top),env // throw always fails, not changing status
    | Scope (inner,faultHandler) ->
        let inner = getQoS inner
        let faultHandler = getQoS faultHandler
        fun env ->
            match inner env with
            | (Fault,quality),env ->
                let (result,quality2),env = faultHandler env
                (result,QoS.sequentialBehavior(quality,quality2)),env
            | other -> other
             
    | Sequence  (a1,a2) ->
        let a1 = getQoS a1
        let a2 = getQoS a2
        fun env ->
            match inner env with
            | (Success,quality),env ->
                let (result,quality2),env = a2 env
                (result,QoS.sequentialBehavior(quality,quality2)),env
            | other -> other
    | Flow l ->
        //we assume:
        // - activities in the list are ordered in respect to links
        // - no race condition, i.e. no variables name collision, variables are always defined in every path which could lead to an activity
        // - each activity which is supposed to set a link status will set a variable with the same name
        // //- activities which are link targets will instead be wrapped in a if() over the join condition
        let l1 = l |> List.map (fun (join,activity,linkname) -> 
            linkname,(
                let activity = getQoS activity
                let g = expEval join
                fun env ->
                qos.After ((g env,env),fun (g,env) ->
                if g then
                    let activityResult,modifiedEnv = activity env
                    //after hearing activity result we trigger links by adding the result to the environment
                    qos.After( (activityResult,modifiedEnv), (fun (success,env) -> 
                        (qos.Return true),//variable assignation always succeed with maximum QoS
                            if success then 
                                { EnvType.variables = env.variables; incomingLinks=env.incomingLinks.Add(linkname,q)}
                            else
                                env))

            )))
