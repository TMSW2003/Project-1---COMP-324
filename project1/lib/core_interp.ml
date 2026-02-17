(* Ocaml- interpreter.
 *
 * N. Danner
 *)

module Ast = Core_ast

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

end


(*  lookup rho id = v, where v is the value associated with id in rho.
*)
let rec lookup (rho : Env.t) (id : Ast.Id.t) : Value.t
  match rho with
  | (id, v)::tail -> v
  | _::tail -> lookup tail id
  | _ -> failwth "Unbound variable or something buddy-o"

let rec addrho (rho : Env.t) (id : Id.t) (v : Value.t) : Env.t
  match rho with
  | (id, _)::tail -> (id, v)::tail
  | x::tail -> x::(addrho tail id v)
  | _ -> (id, v)::Env.empty
  
(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (_ : Ast.Script.t) : Value.t =
  failwith "Unimplemented:  Core.Interp.exec"


let rec eval (rho : Env.t) (e : Ast.Expr.t) : Value.t =
  match e with
  | Var s -> lookup rho s
  | Num n -> n
  | Bool b -> b
  | Unop (Neg, e0) ->       (let m = eval eo in
                            0 - m)
  | Unop (Not, e0) ->       (let m = eval e0 in
                            not e0)
  | Binop (Plus, e0, e1) -> (let m = eval e0 in
                            let n = eval e1 in
                            let p = m + n in
                            p)
  | Binop(Minus, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m - n in
                            p)
  | Binop(Times, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m * n in
                            p)
  | Binop(Div, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m / n in
                            p)
  | Binop(Mod, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m mod n in
                            p)
  | Binop(And, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m && n in
                            p)
  | Binop(Or, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m || n in
                            p)
  | Binop(Eq, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m = n in
                            p)
  | Binop(Ne, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m != n in
                            p)
  | Binop(Lt, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m < n in
                            p)
  | Binop(Gt, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m > n in
                            p)
  | Binop(Ge, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m >= n in
                            p)
  | Binop(Le, e0, e1) ->  (let m = eval e0 in
                            let n = eval e1 in
                            let p = m <= n in
                            p)