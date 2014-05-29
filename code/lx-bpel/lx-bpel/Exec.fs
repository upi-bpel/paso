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
    | Flow of ((string*Activity list)*(string*string*string list))

module Eval =
    
    let flip =
        let g =new System.Random()
        fun v -> g.NextDouble() < v
        
    let Zero = 0.0,0.0
    let Both(c1:Cost,c2:Cost) =
        let p1,t1 = c1
        let p2,t2 = c2
        (p1 + p2), (max t1 t2):Cost
    let Delay(c1:Cost,c2:Cost) =
        let p1,t1 = c1
        let p2,t2 = c2
        (p1 ), ( t1 + t2):Cost

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

    let rec Exec (env:Map<string,bool>) = function
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
            let newerEnv,outcome, newCost = Exec newEnv activity
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

    let riskSamplingFun () =
        if flip 0.01 then
            Stuck,(1.0,0.0)
        else if flip (0.69/0.99) then
            Success,(0.1,1.0)
        else
            Success,(0.1,2.0)

    let approvalSamplingFun() =
        if flip 0.15 then
            Fault,(0.0,60.0)
        else if flip (0.3/0.85) then
            Success,(5.0,600.0)
        else if flip (0.35/0.55) then
            Success,(10.0,1200.0)
        else
            Success,(15.0,1800.0)
            
    let receiveBlock = OpaqueAssign ("g",0.5)
    let riskAssessBlock = IfThenElse(Variable "g",Assign("l",Constant false),Sequence(Invoke(riskSamplingFun),Assign("l",Constant true)))
    let approvalBlock = IfThenElse(Or(Not(Variable "g"),Variable "l"),Sequence(Assign("w",Constant true),While(Variable "w",Scope(Sequence(Invoke approvalSamplingFun,Assign("w",Constant false)),Nothing))),Nothing)
    let total = Sequence(Sequence(receiveBlock,riskAssessBlock),approvalBlock)