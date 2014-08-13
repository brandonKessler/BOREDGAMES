open Ast
type overallTypes = Players | Pieces
type scope = Global 
             | Local
type datatype = 
     Datatype of Ast.bg_t
type scoord_t = {
   sxc : int;
   syc : int;
} 
type splayer_t = {
   splrname : string;
}
type spiece_t = { 
   sowner : string;
   sname : string;
   snum : int;
   sptval : int;
   scloc : scoord_t;
}
type smat_t = {
   srows : int;
   scols : int;
}
type sexpr = 
   SLint of int * datatype
 | SLfloat of float * datatype
 | SLbool of bool * datatype
 | SLstring of string * datatype
 | SId of string * scope * datatype
 | SBinop of sexpr * op * sexpr * datatype
 | SThrough of sexpr * sexpr * datatype
 | SIncr of sexpr * Ast.inc * datatype
 | SAssign of string * sexpr * scope * datatype
 | SCall of expr * sexpr list * scope * datatype
 | SAccess of sexpr * sexpr * datatype
 | SBaccess of sexpr * scoord_t * datatype
 | SDaccess of sexpr * sexpr * datatype
 | SNoexpr

 type sstmt = 
   SBlock of sstmt list
 | SExpr of sexpr
 | SReturn of sexpr
 | SIf of sexpr * sstmt * sstmt 
 | SLoop of sexpr * sstmt 
 | SDecl of datatype * sexpr * scope
 | SNextPlayer
 type ssetup_dec = 
   SSetbd of smat_t
 | SSetpc of spiece_t 
 | SSetplr of splayer_t 
 | SStmt of sstmt

 type srules_t = {
   srname : string;
   srbody : sstmt list;
 }
 type srules_decl =
         SRules_Decl of srules_t * datatype
 type sprogram = 
         Prog of ssetup_dec list * srules_decl list * sstmt list
