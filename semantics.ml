open Ast
open Sast

exception Error of string

type symbol_table = { parent : symbol_table option; variables : (string *
                        datatype * bg_t option) list; 
}

type rules_table = { rules: (string *  sstmt list) list }

type translation_environment = {
        return_type: datatype;
        return_seen: bool;
        location: string;
        global_scope : symbol_table;
        local_scope: symbol_table;
        rule_scope: rules_table;
}
let rec find_rule (rule_scope : rules_table) name = List.find(fun (s,_) ->
        s=name) rule_scope.rules
let math e1 e2 = match (e1, e2) with
        (Int, Int) -> (Int,true)
        |(Int, Float) -> (Float, true)
        |(Float, Int) -> (Float, true)
        |(Float, Float) -> (Float, true)
        |(_,_) -> (Int, false)
let relations e1 e2 = match(e1, e2) with
        (Int, Int) -> (Bool, true)
        | (Float, Float) -> (Bool,true)
        | (Int, Float) -> (Bool, true)
        | (Float, Int) -> (Bool, true)
        | (_,_) -> (Bool, false)
let logic e1 e2 = match(e1, e2) with 
        (Bool, Bool) -> (Bool,true)
        | (_,_) -> (Int, false)
let equality e1 e2 = match(e1, e2) with 
        (Bool, Bool) -> (Bool, true)
        | (Int, Int) -> (Bool, true)
        | (Float, Int) -> (Bool, true)
        | (Int, Float) -> (Bool, true)
        | (Float, Float) -> (Bool, true)
        | (String, String) -> (Bool, true)
        | (Piece, Piece) -> (Bool, true)
        | (Player, Player) -> (Bool,true)
        | (_,_) -> (Int, false)

let rec get_type = function
        Datatype(t) -> t

let get_op_return_value ops e1 e2 = 
        let type1 = get_type e1 and type2 = get_type e2 in
        let(t,valid) = match ops with
              Add -> math type1 type2
              | Sub -> math type1 type2
              | Mult -> math type1 type2
              | Div -> math type1 type2
              | Equal -> equality type1 type2
              | Neq -> equality type1 type2
              | Less -> relations type1 type2
              | Leq -> relations type1 type2
              | Greater -> relations type1 type2
              | Geq -> relations type1 type2          
              | Or -> logic type1 type2
              | And -> logic type1 type2
        in (Datatype(t), valid)

let update_var env(name, datatype, value) =
        let ((_,_,_), location) = 
                try (fun local_scope -> ((List.find(fun (s,_,_) -> s=name)
                local_scope),1)) env.local_scope.variables
                with Not_found -> try( fun local_scope -> ((List.find (fun
                        (s,_,_) -> s=name) local_scope),2))
                env.global_scope.variables
                with Not_found -> raise Not_found in
        let new_envfun = match location with 
                1 -> let new_vars = List.map (fun(n,t,v) -> if(n=name) then
                        (name, datatype, value) else (n,t,v))
                        env.local_scope.variables in
                        let new_symbol_table = {parent = env.local_scope.parent;
                        variables = new_vars;} in
                        let new_env = {env with local_scope = new_symbol_table} in
                        new_env
                 |2 -> let new_vars = List.map(fun (n,t,v) -> if(n=name) then
                         (name, datatype, value) else (n,t,v))
                        env.global_scope.variables in 
                        let new_symbol_table = {parent = env.local_scope.parent;
                        variables = new_vars;} in
                        let new_env = {env with global_scope = new_symbol_table} in
                        new_env
                  | _ -> raise(Error("Scope is not defined")) in new_envfun

let find_var env name = try List.find(fun (s,_,_) -> s=name)
         env.local_scope.variables with Not_found -> try List.find(fun(s,_,_) ->
                 s=name) env.global_scope.variables with Not_found -> raise
                 (Error("variable not found"))                 


let rec check_expr env e = match e with
        Lint(i) -> Datatype(Int)
        | Lfloat(f) -> Datatype(Float)
        | Lbool(b) -> Datatype(Bool)
        | Lstring(s) -> Datatype(String)
        | Id(s) -> let (_,stype, _) = try find_var env s with Not_found ->
                        (*check if rule id?  *)
                        raise(Error("Undeclared Variable Identifier")) in stype
        | Binop(e1, op, e2) -> let type1 = check_expr env e1 and type2 =
                                 check_expr env e2 in
                               let (t, valid) = get_op_return_value op type1
                               type2 in
                               if valid then t else raise(Error("Incompatible
                                type for operation"));
        |Through(e1, e2) -> let type1 = check_expr env e1 and type2 = check_expr
                                        env e2 in
                                        if (type1 = Datatype(Int) && type2 =
                                                Datatype(Int)) then
                                                        Datatype(Bool)
                                        else raise(Error("Through needs two ints
                                        as parameters"));
        | Incr(e1, inc) -> let type1 = check_expr env e1 in 
                if type1 = Datatype(Int) then type1 else if type1 =
                        Datatype(Float) then type1 else raise (Error("Cannot
                        perform value change on non numeric datatype"))
        | Assign(s1, e1) -> let (_,type1,_) = (find_var env s1)
                            and type2 = check_expr env e1 in
                             (if not (type1 = type2) then (raise (Error("Types in
                             variable assignment are not matching"))));
                             check_expr env e1
(* needs to work with pieces and player access *)
        | Access(e1, e2) ->  let typee2 = check_expr env e2 and type1 =
                check_expr env e1 in
                                if typee2 = Datatype(Int) then type1 else raise
                                (Error("Lookup index needs to be of type int"))
        | Call(Id("Output"), e1) -> let _ = List.map(fun exp -> check_expr env
                                           exp) e1 in Datatype(Bool)
        | Call(Id("Input"), e1) -> let _ = List.map(fun exp -> check_expr env exp)
                                        e1 in Datatype(Bool)
        | Call(Id("inventory"), e1) -> let _ = List.map(fun exp -> check_expr env
                                        exp) e1 in Datatype(Piece)
        | Call(Id("onBoard"), e1) -> let _ = List.map(fun exp -> check_expr env
                                         exp) e1 in Datatype(Piece)
        | Call(Id("Pieces"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(Piece)
        | Call(Id("location"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(Coord)    
        | Call(Id("name"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(String) 
        | Call(Id("point"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(Int)
        | Call(Id("unoccupied"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(Bool)
        | Call(Id("owner"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(String)                                   
        | Call(Id("move"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(Bool)                                  
        | Call(Id("add"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(Bool)
        | Call(Id("EndGame"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(String)   
        | Call(Id("CurrentPlayer"), e1) -> let _ = List.map(fun exp -> check_expr env
                                          exp) e1 in Datatype(Player)                                 
        | Call(id, e1) -> raise(Error("Function does not exist"))
        
        | Baccess(id, c) -> Datatype(Tile)
        | Daccess(e1, e2) -> let type1 = check_expr env e1 and type2 =
                check_expr env e2 in
                        if ( type1 = Datatype(Player) || type1 =
                                Datatype(Piece) || type1 = Datatype(Tile)) then
                                type2 else raise(Error("Invalid dot access")) 
        | Noexpr ->  Datatype(String) (* is this ever used?*)

let get_var_scope env name = try( let(_,_,_) = List.find (fun (s,_,_) -> s=name)
        env.local_scope.variables in Local) with Not_found -> try(let(_,_,_) = List.find
        (fun (s,_,_) -> s=name) env.global_scope.variables in Global) with Not_found ->
        raise(Error("cant get variable scope"))

let rec get_sexpr env e = match e with
         Lint(i) -> SLint(i, Datatype(Int))
        | Lfloat(f) -> SLfloat(f, Datatype(Float))
        | Lbool(b) -> SLbool(b, Datatype(Bool))
        | Lstring(s) -> SLstring(s, Datatype(String))
        | Id(s) -> SId(s, get_var_scope env s, check_expr env e)
        | Binop(e1, op, e2) -> SBinop(get_sexpr env e1, op, get_sexpr env e2,
                check_expr env e)
        | Through(e1,e2) -> SThrough(get_sexpr env e1, get_sexpr env e2,
                check_expr env e1)
        | Incr(e1, inc) -> SIncr(get_sexpr env e1, inc, check_expr env e)
        | Assign(s, e1) -> SAssign(s, get_sexpr env e1, get_var_scope env s,
                 check_expr env e1)
       (*stringofexpr*)
        | Call(Id("Output"), e1) -> let  s_ex_list = List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("Output"),s_ex_list, Global, check_expr env e)
        | Call(Id("Input"), e1) -> let  s_ex_list = List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("Input"),s_ex_list, Global, check_expr env e)
        | Call(Id("inventory"), e1) -> let s_ex_list = List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("inventory"),s_ex_list, Global, check_expr env e)
        | Call(Id("onBoard"), e1) ->  let s_ex_list =  List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("onBoard"),s_ex_list, Global, check_expr env e)
        | Call(Id("Pieces"), e1) ->  let s_ex_list = List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("Pieces"),s_ex_list, Global, check_expr env e)
        | Call(Id("location"), e1) ->  let s_ex_list =  List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("location"),s_ex_list, Global, check_expr env e)    
        | Call(Id("name"), e1) ->  let  s_ex_list = List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("name"),s_ex_list, Global, check_expr env e) 
        | Call(Id("point"), e1) -> let  s_ex_list = List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("point"),s_ex_list, Global, check_expr env e)
        | Call(Id("unoccupied"), e1) ->  let s_ex_list = List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("unoccupied"),s_ex_list, Global, check_expr env e)
        | Call(Id("owner"), e1) ->  let s_ex_list =  List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("owner"),s_ex_list, Global, check_expr env e)                                
        | Call(Id("move"), e1) ->  let s_ex_list =  List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("move"),s_ex_list, Global, check_expr env e)                                 
        | Call(Id("add"), e1) ->  let s_ex_list = List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("add"),s_ex_list, Global, check_expr env e)
        | Call(Id("EndGame"), e1) ->  let s_ex_list =  List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("EndGame"),s_ex_list, Global, check_expr env e)
         | Call(Id("CurrentPlayer"), e1) ->  let s_ex_list =  List.map(fun exp -> get_sexpr env exp) e1 in
                                 SCall(Id("CurrentPlayer"),s_ex_list, Global, check_expr env e)
        | Call(id, e1) -> raise(Error("Function does not exist"))
        
        | Baccess(id, c) -> SBaccess(get_sexpr env id, {sxc = c.xc; syc=
                c.yc}  ,check_expr env e)
        | Daccess(e1, e2) -> SDaccess(get_sexpr env e1, get_sexpr env e2,
        check_expr env e)
        | Noexpr -> SNoexpr
        | Access(e1, e2) -> SAccess(get_sexpr env e1, get_sexpr env e2,
        check_expr env e)

let get_sexpr_list env expr_list = let sexpr_list= List.map(fun expr -> let t1
= get_type(check_expr env (List.hd expr_list)) and t2 =
        get_type(check_expr env expr) in if(t1=t2) then get_sexpr
        env expr else raise(Error("Type Mistmatch"))) expr_list in sexpr_list

let get_sdecl env decl = match decl with 
        Decl(datatype, Id(s)) -> SDecl(Datatype(datatype), SId(s, Global,
        Datatype(datatype)), Global)
        |Decl(datatype, Assign(s,e1)) ->  SDecl(Datatype(datatype), SAssign(s,
        get_sexpr env e1, Global, check_expr env e1),Global)
        | _ -> raise(Error("Bad Declaration"))
let get_name_type_from_decl decl = match decl with
        Decl(datatype, Id(s)) -> (s, datatype)
        | Decl(datatype, Assign(v,e)) -> (v, datatype)
        | _ ->raise(Error("Bad variable declaration"))
let get_name_type_value_from_decl decl = match decl with
        Decl(datatype, Id(s)) -> (s, datatype, None)
        | Decl(datatype, Assign(v,e)) -> (v, datatype, Some(e))
        | _ ->raise(Error("Bad variable declaration"))
let get_name_type_from_var env = function
        Decl(datatype, Id(s)) -> (s, datatype, None)
       | Decl(datatype, Assign(v,e)) -> (v, datatype, Some(e))
       | _ ->raise(Error("Bad variable declaration"))
let add_to_var_table env name t v = (* does local add ? change to global when in
play?*)
        let new_vars = (name,t,v):: env.local_scope.variables in
        let new_sym_table = {parent = env.local_scope.parent; variables =
                new_vars;} in
        let new_env = {env with local_scope = new_sym_table} in
        new_env
let add_to_global_table env name t v = 
         let new_vars = (name,t,v):: env.global_scope.variables in
        let new_sym_table = {parent = env.global_scope.parent; variables =
                new_vars;} in
        let new_env = {env with global_scope = new_sym_table} in
        new_env
let check_assignment t1 t2 = match (t1, t2) with
        (Int, Int) -> true
       | (Float, Float) -> true
       | (Int, Float) -> true
       | (Float, Int) -> true
       | (Bool, Bool) -> true
       | (String, String) -> true
       | (Player, Player) -> true
       | (Piece, Piece) -> true
       | (_,_) -> false
let match_var_type env v t = 
        let(name, ty, value) = find_var env v in
        if(t <> ty) then false else true
       
let check_rule_return env = 
        (if(false = env.return_seen && env.return_type = Datatype(Bool)) then
                raise(Error("Missing return statement in rule")));
         true
let empty_table_initialization = {parent = None; variables = [];}
let empty_rule_table_initialization = { rules = []}

let empty_environment = {return_type = Datatype(String); return_seen = false;
                location = "Setup"; global_scope = empty_table_initialization; local_scope =
                empty_table_initialization; rule_scope = empty_rule_table_initialization}

let empty_environment = {return_type = Datatype(String); return_seen = false;
                location = "Rules&Play"; global_scope = empty_table_initialization; local_scope =
                empty_table_initialization; rule_scope = empty_rule_table_initialization}

let find_global_variable env name = 
        try List.find(fun (s,_,_) -> s=name) env.global_scope.variables with 
        Not_found -> raise Not_found
(*let initialize_globals (globals, env) decl = 
        let(name, ty) = get_name_type_from_decl decl in 
                let((_,dt,_), found) = try(fun f -> ((f env name), true))
                find_global_variable with Not_found -> ((name, Datatype(ty), None), false) in
                let ret = if(found = false) then match decl with
                        Decl(datatype, Id(s)) -> let(name, ty, _) =
                                get_name_type_from_var env decl in
                                let new_env = add_to_global_table env name
                                Datatype(ty)
                                None in
                                (SDecl(datatype,Id(s), Global) :: globals,
                                new_env)
                        | Decl(datatype, Assign(v,e)) -> let t1 =
                                get_type_from_datatype (datatype) and t2 =
                                        get_type_from_datatype
                                        (check_expr env e) in
                                        if(t1 = t2) then let (n,t,v) =
                                                get_name_type_val_from_decl decl
                                                 in let new_env =
                                                         add_to_global_table env
                                                         n t v in
                                                 (SDecl(datatype, get_sexpr env,
                                                 Assign(v,e) , Global) ::
                                                         globals, new_env)
                                        else raise (Error("Mismatched type on
                                        variable declaration"))
                                    else raise (Error("Multiple Declarations"))
                in ret *)
let find_local_variable env name = try List.find (fun (s,_,_) -> s=name)
                env.local_scope.variables with Not_found -> raise Not_found   
let rec check_stmt env stmt = match stmt with 
        Block(stmt_list) -> let new_env = env in let getter(env, acc) s = let
                        (st, ne) = check_stmt env s in (ne, st::acc) in let (ls, st) =
                                 List.fold_left(fun e s -> getter e s) (new_env, []) stmt_list in
                                        let revst = List.rev st in (SBlock(revst), ls)
        | Expr (e) -> let _  = check_expr env e in
                                (SExpr(get_sexpr env e), env)
        | Return(e) -> let t1 = check_expr env e in (if not((t1 =
                                 env.return_type)) then raise(Error("Incompatible Return
                                Type")));
                                let new_env = {env with return_seen = true} in
                                (SReturn(get_sexpr env e), new_env)
       | If(e,s1,s2) -> let t = get_type(check_expr env e) in (if
                                not(t = Bool) then raise(Error("If Statement must be of type
                                Bool")));
                                let (st1, new_env1) = check_stmt env s1
                                and (st2, new_env2) = check_stmt env s2 in
                                let ret_seen = (new_env1.return_seen &&
                                new_env2.return_seen) in 
                                let new_env = {env with return_seen = ret_seen}
                                in (SIf((get_sexpr env e), st1, st2), new_env)
      | Loop(e1, s1) -> let t=get_type(check_expr env e1) in (if
              not(t = Bool) then raise(Error("Improper loop statement")));
              let (st, new_env) = check_stmt env s1 in (SLoop((get_sexpr env e1),
              st), new_env)
      | Decl(datatype, e) ->  let decl = Decl(datatype, e) in let (name, ty ) = get_name_type_from_decl
                                 (Decl(datatype,e)) in 
                                let ((_,dt,_), found) = try(fun f -> ((f env
                                name), true)) find_global_variable with Not_found
                                -> (*let ((_,dtg,_), foundg) =
                                        try(fun fg -> ((fg env
                                name), true)) find_global_variable with
                                Not_found ->*) ((name, Datatype(ty), None) , false) in 
                                let ret = if(found = false) then
                                        match e with 
                                        Id(s) -> let sdecl = get_sdecl env decl
                                                in let (n, t, v) =
                                                get_name_type_value_from_decl decl
                                                in let new_env = add_to_global_table env
                                                n (Datatype(t)) None in (sdecl,
                                                new_env)
                                        | Assign(s1, e1)  -> 
                                        let t1 =
                                                datatype 
                                        and t2 =
                                                get_type(check_expr
                                                env e1)  in
                                        
                                        if(t1 = t2) then let sdecl =
                                                get_sdecl env decl
                                        in let (n,t,v) =
                                                get_name_type_value_from_decl
                                                decl in
                                        let new_env = add_to_global_table env n
                                        (Datatype(t)) None in
                                        (sdecl, new_env) 
                                        else raise(Error("Type mismatch"))
                                        | _-> raise(Error("Bad variable
                                        declaration"))
                                else 
                                        raise(Error("Already Declared this
                                variable")) in ret                               
      | NextPlayer -> (SNextPlayer, env)       
let get_sstmt_list env stmt_list = List.fold_left (fun (sstmt_list, env) stmt ->
        let(sstmt, new_env) = check_stmt env stmt in (sstmt::sstmt_list,
        new_env)) ([],env) stmt_list

let add_rule env srule_decl = 
        let r_table = env.rule_scope in 
        let old_rules = r_table.rules in match 
                srule_decl with
                      SRules_Decl(srules_t, datatype) -> let rule_name =
                              srules_t.srname  in let
                              rule_body = srules_t.srbody in let new_rules =
                                      (rule_name, rule_body) :: old_rules in
                              let new_rule_scope = {rules = new_rules} in
                              let final_env = {env with rule_scope =
                                      new_rule_scope} in final_env

(*semantic checking on rule *)
let check_rule env rule_declaration = 
        let new_local_scope = {parent = Some(env.local_scope); variables = [];}
        in
        let new_env = {return_type = Datatype(Bool); return_seen = false;
        location = "in_rule"; global_scope = env.global_scope; local_scope =
                new_local_scope; rule_scope = env.rule_scope} in
        let (typed_statements, final_env) = get_sstmt_list new_env
        rule_declaration.rbody in     
        let _ = check_rule_return final_env  in
        let sruledecl = ({ srname = rule_declaration.rname; srbody =
                typed_statements}) in
        (SRules_Decl(sruledecl, Datatype(Bool)), env)
let initialize_rules env rule_list = let (typed_rules, last_env) =
        List.fold_left (fun( sruledecl_list, env) rule -> let (sruledecl, _) =
                check_rule env rule in let final_env = add_rule env sruledecl in
                (sruledecl::sruledecl_list, final_env))([], env) rule_list in
(typed_rules, last_env)


let check_ssetup_decl env e = match e with
        Setbd (m) -> let sm = {srows = m.rows; scols = m.cols;} in
                        (SSetbd(sm), env)
        |Setpc(p) -> let sp = {sowner = p.owner; sname = p.name; snum = p.num;
                         sptval = p.ptval;scloc ={ sxc = p.cloc.xc; syc =
                                 p.cloc.yc}} in (SSetpc(sp), env)
        |Setplr(p) -> let sp = {splrname = p.plrname} in (SSetplr(sp), env)
        | Stmt(s) -> let (typed_stmt, final_env) = check_stmt env s in
                        (SStmt(typed_stmt), final_env)

 (* goes to check setup section *)       
let get_ssetup_decl_list env sdecl_list =
        List.fold_left ( fun (ssetup_decl_list, env) stmt -> let(sstmt, new_env)
        = check_ssetup_decl env stmt in (sstmt::ssetup_decl_list, new_env))([], env)
        sdecl_list

let get_rules_names env rules_list = 
        let new_env2 = 
        List.fold_left (fun  env rule -> add_to_global_table
        env rule.rname (Datatype(Bool)) None) env rules_list in
        new_env2

let check_program program = 
        
        let (setup, rules, play) = program in
        let env = empty_environment in
        let (typed_setup, new_env) = get_ssetup_decl_list env setup in (* should
        check setup section completely *)
        let env2 = empty_environment in
        let env2 = add_to_global_table env2 "Board" (Datatype(Tile)) None in
        let env2 = add_to_global_table env2 "Player" (Datatype(Player)) None in
        let env2 = add_to_global_table env2 "Pieces" (Datatype(Piece)) None in
        let new_env2 = get_rules_names env2 rules in (* should add rules to 
        global table in the env*)
        let (typed_play, new_env3) =  get_sstmt_list new_env2 play in (* should
        type check play section *)
        let (typed_rules, new_env4) = initialize_rules new_env3 rules in
        Prog(typed_setup, typed_rules, typed_play)


