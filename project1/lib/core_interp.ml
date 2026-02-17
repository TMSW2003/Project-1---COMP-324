(* Ocaml- interpreter.
 *
 * N. Danner
 *)

module Ast = Core_ast

module E = Ast.Expr

(* UndefinedFunction f is raised when f is called but not defined.
 *)
exception UndefinedFunction of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
 *)
exception UnboundVariable of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
 * of the incorrect type.  s is any (hopefuly useful) message.
 *)
exception TypeError of string

(* Values.
 *)
module Value = struct
  type t = 
    | V_Int of int
    | V_Bool of bool
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
end

(* Environments.  An environment is a finite map from identifiers to values.
 * We will interchangeably treat environments as functions or sets or lists
 * of pairs in documentation.  We will use ρ as a metavariable over
 * environments.
 *)
module Env = struct

  type t = (Ast.Id.t * Value.t) list
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅.
   *)
  let empty : t = []

    (*  lookup ρ x = ρ(x).
   *)
(*! env lookup !*)
  let lookup (rho : t) (x : Ast.Id.t) : Value.t = 
    try List.assoc x rho with
    | Not_found -> raise (UnboundVariable x)
(*! end !*)

  (*  update ρ x v = ρ{x → v}.
   *)
  let addrho (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x, v) :: List.remove_assoc x rho
end


(*  unop op v = v', where v' is the result of applying the semantic
 *  denotation of `op` to `v`.
 *)
let unop (op: E.unop) (v : Value.t) : Value.t =
  match (op, v) with
  | (E.Neg, Value.V_Int n) -> Value.V_Int (-n)
  | (E.Not, Value.V_Bool b) -> Value.V_Bool (not b)
  | _ -> raise (TypeError "Should call you Alexandre, cause you're a Dumas")


(*  binop op v0 v1 = v', where v' is the result of applying the semantic
 *  denotation of `op` to `v0` and `v1`.
 *)
let binop (op : E.binop) (v0 : Value.t) (v1 : Value.t) : Value.t =
  match (op, v0, v1) with
  (*Basic arithmetic operations*)
  | (E.Plus, Value.V_Int n0, Value.V_Int n1) -> Value.V_Int (n0 + n1)
  | (E.Minus, Value.V_Int n0, Value.V_Int n1) -> Value.V_Int (n0 - n1)
  | (E.Times, Value.V_Int n0, Value.V_Int n1) -> Value.V_Int (n0 * n1)
  | (E.Div, Value.V_Int n0, Value.V_Int n1) -> Value.V_Int (n0 / n1)
  | (E.Mod, Value.V_Int n0, Value.V_Int n1) -> Value.V_Int (n0 mod n1)
  (*Basic boolean operations*)
  | (E.And, Value.V_Bool b0, Value.V_Bool b1) -> Value.V_Bool (b0 && b1)
  | (E.Or, Value.V_Bool b0, Value.V_Bool b1) -> Value.V_Bool (b0 || b1)
  (*Equality and Inequality*)
  | (E.Eq, Value.V_Int n0, Value.V_Int n1) -> Value.V_Bool (n0 == n1)
  | (E.Eq, Value.V_Bool b0, Value.V_Bool b1) -> Value.V_Bool (b0 == b1)
  | (E.Ne, Value.V_Int n0, Value.V_Int n1) -> Value.V_Bool (n0 != n1)
  | (E.Ne, Value.V_Bool b0, Value.V_Bool b1) -> Value.V_Bool (b0 != b1)
  (*Ineqality operations*)
  | (E.Lt, Value.V_Int n0, Value.V_Int n1) -> Value.V_Bool (n0 < n1)
  | (E.Le, Value.V_Int n0, Value.V_Int n1) -> Value.V_Bool (n0 <= n1)
  | (E.Gt, Value.V_Int n0, Value.V_Int n1) -> Value.V_Bool (n0 > n1)
  | (E.Ge, Value.V_Int n0, Value.V_Int n1) -> Value.V_Bool (n0 >= n1)
  | _ -> raise (TypeError "Should call you Alexandre, cause you're a Dumas")


(*  eval ρ e = v, where ρ ├ e ↓ v according to our evaluation rules.
 *)
let rec eval (rho : Env.t) (funs : Ast.Script.fundef list) (e : E.t) : Value.t =
  match e with
  | E.Var x -> Env.lookup rho x
  | E.Num n -> Value.V_Int n
  | E.Bool b -> Value.V_Bool b
  | E.Unop (op, e') ->
    let v = eval rho funs e' in unop op v
  | E.Binop (op, e0, e1) -> 
    let v0 = eval rho funs e0 in
    let v1 = eval rho funs e1 in
    binop op v0 v1
  | E.If(e0, e1, e2) ->
    (match eval rho funs e0 with
    | (Value.V_Bool b) -> if b then eval rho funs e1 else eval rho funs e2
    | _ -> raise (TypeError "Should call you Alexandre, cause you're a Dumas"))
  | E.Let(x, e0, e1) ->
    let x' = eval rho funs e0 in
    let rho' = Env.addrho rho x x' in eval rho' funs e1
  | _ -> failwith "Unimplemented"

                          
(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (p : Ast.Script.t) : Value.t =
  match p with
  | Ast.Script.Pgm (funs, e) -> eval Env.empty funs e