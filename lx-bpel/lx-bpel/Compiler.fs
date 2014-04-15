module lx_bpel.compiler

type BpelLogicValue =
    | True
    | False
    | Undefined
type BpelLogicExpr =
    | Val of BpelLogicValue
    | And of BpelLogicExpr* BpelLogicExpr
    | Or of BpelLogicExpr*BpelLogicExpr
    | Not of BpelLogicExpr
    | VarEval of string
    | Link of string

type BpelActivity =
    | Invoke of string*string*BpelLogicExpr
    | VarAssign of string*BpelLogicExpr
type BpelTree =
    | Activity of BpelActivity
    | Sequence of BpelTree*BpelTree
    | Flow of BpelTree list
    | IfElse of BpelLogicExpr*BpelTree*BpelTree
    | ForEach of BpelTree
    | While of BpelLogicExpr*BpelTree

let rec standardSemanticLogicEval (e) ctx =
    match e with
    | Val v ->
        fun () -> v
    | And (e1,e2) ->
        let r1,r2 = standardSemanticLogicEval e1 ctx,standardSemanticLogicEval e2 ctx
        fun () ->
            match r1 (),r2 () with
            | Undefined,_ -> Undefined
            | _,Undefined -> Undefined
            | True,True -> True
            | _ -> False
    | Or (e1,e2) -> //or is parallel
        let r1,r2 = standardSemanticLogicEval e1 ctx,standardSemanticLogicEval e2 ctx
        fun () ->
            match r1 (),r2 () with
            | True,_ -> True
            | _,True -> True
            | False,False -> False
            | _ -> Undefined
    | Not e1 -> //or is parallel
        let r1 = standardSemanticLogicEval e1 ctx
        fun () ->
            match r1 () with
            | True -> False
            | False -> True
            | Undefined -> Undefined
    | VarEval vname ->
        let cell = Map.find vname ctx
        fun () ->
            match !cell with
            | None -> Undefined
            | Some s -> s
    | Link lname ->
        let cell = Map.find lname ctx
        fun () ->
            match !cell with
            | None -> Undefined
            | Some s -> s

let rec standardSemanticActivityEval (services:Map<string,BpelLogicValue -> BpelLogicValue option>) a ctx =
    match a with
    | Invoke (varname,servicename,param)->
        let s = Map.find servicename services
        let compiledParam = standardSemanticLogicEval param ctx
        if varname = "" then
            fun () -> s (compiledParam ())
        else
            let cell = Map.find varname ctx
            fun () -> let v = s (compiledParam ()) in cell := v; v
    | VarAssign (varname, expression) ->
        let compiledExpr = standardSemanticLogicEval expression ctx
        let cell = Map.find varname ctx
        fun () -> let v = Some (compiledExpr ()) in cell := v; v
