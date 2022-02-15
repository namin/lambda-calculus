open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec expand ctx t = match t with
  | TmVar(fi,n,_) ->
     (match getbinding fi ctx n with
        TmAbbBind(t) -> expand ctx t
      | _ -> t)
  | TmApp(fi,t1,t2) -> TmApp(fi,expand ctx t1,expand ctx t2)
  | TmAbs(fi,x,t1) -> TmAbs(fi,x,expand (addbinding ctx x NameBind) t1)

let rec isval ctx t = match t with
  | TmAbs(_,_,_) -> true
  | _ -> false

let maybe_trace_termSubstTop trace ctx x t2 t12 =
  let r = termSubstTop t2 t12 in
  (*
  (if trace then
     (pr "    ";
      printtm_ATerm true (addbinding ctx x NameBind) t12;
      pr "["; pr x; pr ":=";
      printtm_ATerm true ctx t2;
      pr "] =";
      force_newline();
      pr "    ";
      printtm_ATerm true ctx r;
      force_newline())); *)
  r

let rec eval1 trace order ctx t = match t with
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t) -> t 
        | _ -> raise NoRuleApplies)
  | TmApp(fi,t1,t2) when order=ApplicativeOrder or order=CbvOrder->
      (match t1 with
          TmAbs(_,x,t12) when isval ctx t2 ->
            maybe_trace_termSubstTop trace ctx x t2 t12
       | _ when isval ctx t1 -> TmApp(fi, t1, eval1 trace order ctx t2)
       | _ -> TmApp(fi, eval1 trace order ctx t1, t2))
  | TmApp(fi,t1,t2) when order=CbnOrder->
      (match t1 with
       TmAbs(_,x,t12) -> maybe_trace_termSubstTop trace ctx x t2 t12
       | _ -> TmApp(fi, eval1 trace order ctx t1, t2))
  | TmApp(fi,t1,t2) when order=FullOrder or order=NormalOrder ->
     (match t1 with
       TmAbs(_,x,t12) -> maybe_trace_termSubstTop trace ctx x t2 t12
       | _ -> try TmApp(fi, eval1 trace order ctx t1, t2)
              with NoRuleApplies -> TmApp(fi, t1, eval1 trace order ctx t2))
  | TmAbs(e,x,t11) when order=FullOrder or order=ApplicativeOrder or order=NormalOrder ->
     TmAbs(e,x,eval1 trace order (addbinding ctx x NameBind) t11)
  | _ -> 
      raise NoRuleApplies

let rec eval0 trace stepN order ctx t =
  if stepN==0 then ((if trace then (pr "..."; force_newline())); t) else (
  try let t' = eval1 trace order ctx t in
    (if trace then (pr "--> "; printtm_ATerm true ctx t; force_newline()));
    eval0 trace (stepN-1) order ctx t'
  with NoRuleApplies -> t)

let rec eval trace stepN order ctx t =
  eval0 trace stepN order ctx (if order=NormalOrder then (expand ctx t) else t)

let evalbinding trace stepN order ctx b = match b with
    TmAbbBind(t) ->
      let t' = eval trace stepN order ctx t in 
      TmAbbBind(t')
  | bind -> bind

(* ------------------------   TYPING  ------------------------ *)

type constr = (ty * ty) list

let emptyconstr = []
let combineconstr = List.append

let prconstr constr =
  let pc (tyS,tyT) =
    printty_Type false tyS; pr "="; printty_Type false tyT in
  let rec f l = match l with
      [] -> ()
    | [c] -> pc c
    | c::rest -> (pc c; pr ", "; f rest)
  in 
    pr "{"; f constr; pr "}"

type nextuvar = NextUVar of string * uvargenerator
and uvargenerator = unit -> nextuvar

let uvargen =
  let rec f n () = NextUVar("?X" ^ string_of_int n, f (n+1))
  in f 0

let rec recon ctx nextuvar t = match t with
      TmVar(fi,i,_) ->
      (match getbinding fi ctx i with
         VarBind(tyT) -> (tyT, nextuvar, [])
       | TmAbbBind(t') -> recon ctx nextuvar t')
    | TmAbs(fi, x, t2) ->
        let NextUVar(tyX,nextuvar') = nextuvar() in
        let tyT1 = TyId(tyX) in
        let ctx' = addbinding ctx x (VarBind(tyT1)) in
        let (tyT2,nextuvar2,constr2) = recon ctx' nextuvar' t2 in
        (TyArr(tyT1, tyT2), nextuvar2, constr2)
    | TmApp(fi,t1,t2) ->
        let (tyT1,nextuvar1,constr1) = recon ctx nextuvar t1 in
        let (tyT2,nextuvar2,constr2) = recon ctx nextuvar1 t2 in
        let NextUVar(tyX,nextuvar') = nextuvar2() in
        let newconstr = [(tyT1,TyArr(tyT2,TyId(tyX)))] in
        ((TyId(tyX)), nextuvar', 
         List.concat [newconstr; constr1; constr2])

let substinty tyX tyT tyS =
  let rec f tyS = match tyS with
      TyArr(tyS1,tyS2) -> TyArr(f tyS1, f tyS2)
    | TyId(s) -> if s=tyX then tyT else TyId(s)
  in f tyS

let applysubst constr tyT =
  List.fold_left
    (fun tyS (TyId(tyX),tyC2) -> substinty tyX tyC2 tyS)
    tyT (List.rev constr)

let substinconstr tyX tyT constr =
  List.map
    (fun (tyS1,tyS2) ->
       (substinty tyX tyT tyS1, substinty tyX tyT tyS2))
    constr

let occursin tyX tyT =
  let rec o tyT = match tyT with
      TyArr(tyT1,tyT2) -> o tyT1 || o tyT2
    | TyId(s) -> (s=tyX)
  in o tyT

let unify fi ctx msg constr =
  let rec u constr = match constr with
      [] -> []
    | (tyS,TyId(tyX)) :: rest ->
        if tyS = TyId(tyX) then u rest
        else if occursin tyX tyS then
          error fi (msg ^ ": circular constraints")
        else
          List.append (u (substinconstr tyX tyS rest)) 
                      [(TyId(tyX),tyS)]
    | (TyId(tyX),tyT) :: rest ->
        if tyT = TyId(tyX) then u rest
        else if occursin tyX tyT then
          error fi (msg ^ ": circular constraints")
        else
          List.append (u (substinconstr tyX tyT rest))
                      [(TyId(tyX),tyT)]
    | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) :: rest ->
        u ((tyS1,tyT1) :: (tyS2,tyT2) :: rest)
    | (tyS,tyT)::rest -> 
        error fi "Unsolvable constraints"
  in
    u constr
