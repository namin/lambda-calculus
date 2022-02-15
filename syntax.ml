open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type ty =
  | TyArr of ty * ty
  | TyId of string
          
type term =
  | TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

type binding =
    NameBind
  | VarBind of ty
  | TmAbbBind of term

type context = (string * binding) list

type order =
  | FullOrder
  | NormalOrder
  | ApplicativeOrder
  | CbnOrder
  | CbvOrder

type command =
  | Trace of info * bool
  | Typing of info * bool
  | Step of info * int
  | Order of info * order
  | Eval of info * term
  | Bind of info * string * binding

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tmmap onvar c t = 
  let rec walk c t = match t with
  | TmVar(fi,x,n) -> onvar fi c x n
  | TmAbs(fi,x,t2) -> TmAbs(fi,x,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  in walk c t

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) else TmVar(fi,x,n+d))
    c t

let termShift d t = termShiftAbove d 0 t

let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | VarBind(x) -> VarBind(x)
  | TmAbbBind(t) -> TmAbbBind(termShift d t)

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap
    (fun fi c x n -> if x=j+c then termShift c s else TmVar(fi,x,n))
    0
    t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bindingshift (i+1) bind 
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
 let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
       VarBind(tyT) -> tyT
     | _ -> error fi 
       ("getTypeFromContext: Wrong kind of binding for variable " 
        ^ (index2name fi ctx i)) 
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
  | TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_) -> fi
  | TmApp(fi, _, _) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let remap_tyvars tyT =
  let tbl = Hashtbl.create 10 in
  let next () = Char.escaped (Char.chr ((Char.code 'A')+(Hashtbl.length tbl))) in
  let rec iter tyT = match tyT with
    | TyArr(tyT1, tyT2) -> let tyT1' = iter tyT1 in let tyT2' = iter tyT2 in TyArr(tyT1', tyT2')
    | TyId(x) -> TyId(try (Hashtbl.find tbl x) with Not_found -> (
                        let r = next() in Hashtbl.add tbl x r; r
                   )) in
  iter tyT

let rec printty_Type outer tyT = match tyT with
      tyT -> printty_ArrowType outer tyT

and printty_ArrowType outer  tyT = match tyT with 
    TyArr(tyT1,tyT2) ->
      obox0(); 
      printty_AType false tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space() else break();
      printty_ArrowType outer tyT2;
      cbox()
  | tyT -> printty_AType outer tyT

and printty_AType outer tyT = match tyT with
  | TyId(b) -> pr b
  | tyT -> pr "("; printty_Type outer tyT; pr ")"

let printty tyT = printty_Type true (remap_tyvars tyT)

let rec printtm_Term outer ctx t = match t with
  | TmAbs(fi,x,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
            obox(); pr "lambda "; pr x'; pr ".";
            if (small t2) && not outer then break() else print_space();
            printtm_Term outer ctx' t2;
            cbox())
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | t -> printtm_PathTerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
  | TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let prbinding ctx b = match b with
    NameBind -> ()
  | TmAbbBind(t) -> pr "= "; printtm ctx t 


