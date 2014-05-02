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
        let a1 = getQoS a1
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
