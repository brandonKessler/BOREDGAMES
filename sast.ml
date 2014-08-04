open Ast

module StringMap = Map.Make(String)

type expr_s = 
   Lint_s of int * bg_t list
 | Lfloat_s of float * bg_t list
 | Lbool_s of bool * bg_t list
 | Lcoord_s of coord_t * bg_t list
 | Lstring_s of string * bg_t list
 | Lpieces_s of pieces_t * bg_t list
 | Lmat_s of mat_t * bg_t list
 | Id_s of string * bg_t list
 | Binop_s of expr_s * op * expr_s * bg_t list
 | Through_s of expr_s * expr_s * bg_t list
 | Incr_s of expr_s * inc * bg_t list
 | Assign_s of string * expr_s * bg_t list
 | Call_s of string * expr_s list * bg_t list
 | Access_s of expr_s * expr_s * bg_t list
 | Daccess_s of expr_s * string list * bg_t list
 | Noexpr_s of bg_t list

type stmt_s = 
   Block_s of stmt_s list
 | Expr_s of expr_s
 | Return_s of expr_s
 | If_s of expr_s * stmt_s * stmt_s
 | Loop_s of expr_s * stmt_s



