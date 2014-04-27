﻿module lx_bpel.compiler

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

type QoS(time:float) =
    member x.Equals(y:QoS) = // todo: check how to do it properly
        time = y.Time
    member private x.Time = time
    static member sequentialBehavior ( q1:QoS,q2:QoS) =
        new QoS(q1.Time + q2.Time)
    static member parallelBehavior ( q1:QoS,q2:QoS) =
        new QoS(max q1.Time q2.Time)
    static member pickBehavior ( q1:QoS,q2:QoS) =
        new QoS(min q1.Time q2.Time)
    static member worseCaseBehavior ( q1:QoS,q2:QoS) = //not yet used
        new QoS(max q1.Time q2.Time)

//carries both QoS and value information. Extends QoS type with bottom (Min) to represent nontermination and top value (Max), i.e. the QoS value for independent expressions
type QoSStructural<'T> =
    | Max of 'T
    | Min
    | Some of 'T*QoS

//defines composition function for QoSStructural. Need not to be a type, it's just for collecting them together
type QoSComposer () =
    member x.After (q:QoSStructural<'T>,f:'T->QoSStructural<'U>) =
        //after represent sequential dependance. 
        match q with
        | Max v -> f v
        | Min -> Min
        | Some (v,t) ->
            match f v with
            | Max v1 -> Some (v1,t)
            | Min -> Min
            | Some (v1,t1) -> Some(v1,QoS.sequentialBehavior(t,t1))
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
    | Var of string
and Activity =
    | Nothing
    | Throw
    | Scope of Activity * Activity //first is the inner scope, second is the fault handler
    | Sequence of Activity * Activity
    | Invoke of DiscreteDistribution<QoSStructural<bool>> //we assume invokes are uncorrelated
    | IfThenElse of BoolExpr * Activity * Activity
    | While of BoolExpr * Activity
    | VarSet of string * BoolExpr
    | Flow of (BoolExpr * Activity * string) list

type EnvType ={ variables:Map<string,QoSStructural<bool>>; incomingLinks:Map<string,QoSStructural<bool>>}
let expEval exp ( env:EnvType ) = 
    let rec ee = function
    | True -> qos.Return true
    | False ->  qos.Return false
    | Dep activity -> env.incomingLinks.[activity]
    | Var activity -> env.variables.[activity]
    | Or (a,b) ->
        //we perform De Morgan transforms to get an And instead
        ee (Not (And(Not(a),Not(b))))
    | And (a,b) ->
        let a = ee a
        let b = ee b
        qos.Both(a,b,(fun (a,b) -> qos.Return (a && b)))
    | Not e ->
        let e = ee e
        qos.After (e,fun e -> qos.Return (not e))
    | Pick (e1,e2) ->
        let e1 = ee e1
        let e2 = ee e2
        qos.Any (e1,e2,qos.Return)
    ee exp


//our semantic is a success function plus a status transformation
let rec getQoS = function
    | Nothing -> fun (env:EnvType) -> qos.Return (true,env)  // Nothing always succeeds, not changing status
    | Throw -> fun env -> qos.Return (false,env) // throw always fails, not changing status
    | Scope (inner,faultHandler) ->
        let inner = getQoS inner
        let faultHandler = getQoS faultHandler
        fun env ->
        qos.After(inner env,
            fun (success,status) ->
                if success then
                    qos.Return(true,status)
                else
                    qos.After(faultHandler status, // if inner activity failed we run the fault handler
                        fun (success,status) ->
                            qos.Return(success,status)// the composed status transform
                        )
            ) 
    | Sequence  (a1,a2) ->
        let a1 = getQoS a1
        let a2 = getQoS a2
        fun env ->
        qos.After(a1 env,
            fun (success,status) ->
                if success then
                    qos.After(a2 status, // if a1 succeeded we run the a2
                        fun (success,status) ->
                            qos.Return(success,status)
                        )
                else
                    qos.Return(false,status) 
            )
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
                qos.After (g env,fun g ->
                if g then
                    qos.After(activity env,fun (success,env) -> 
                        if success then
                            qos.Return (true,{ EnvType.variables = env.variables; incomingLinks=env.incomingLinks.Add(} )
                else
                    qos.Return (true,env) // "shortcut rule" : if the join condition is false we consider the activity successful
            )))
        let rec rev = function
        | [] -> fun (env:EnvType) -> qos.Return(true,env)
        | (n,h)::t -> fun env ->
            match h env with
            | Min -> Min
            | Max (false,env) -> Max (false,env)
            
        rev l1
//evaluates expression. We require an environment where all dependent activities have been evaluated.

//activity are imperative, i.e. we add an environment to the input and the output of semantic function
//The semantic domain for activities is the distribution domain conditioned on the environment status
let combinedSeq a1 a2 env=
    bind ( fun (e1,q1)-> 
            bind (fun (e2,q2) ->
            always (e2,qos.After(q1,fun () -> q2))
            ) (a2 e1)
    ) (a1 env)
let rec activityEval = function
    | Invoke d -> fun env -> bind(fun d -> always (env,d)) d
    | Flow l ->
        //we expect l to be sorted respect to dependencies. As race conditions are undefined we just evaluate everything according to the order given.
        //
        match l with
        | [] -> fun env -> always (env,qos.Return true)
        | (g,a,n)::t ->
            fun (env:EnvType) ->
                let g = expEval g env
                let a = activityEval a env
                bind (fun (newEnv,q) ->
                    always (qos.After( g,fun g -> if g then newEnv,q else env,true))
                ) a


