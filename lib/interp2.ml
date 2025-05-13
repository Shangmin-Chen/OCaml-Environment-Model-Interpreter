include Utils
module Env = Map.Make(String)

type value =
  Utils.value =
    VUnit
  | VBool of bool
  | VNum of int
  | VClos of { arg : string; body : expr; env : dyn_env;
      name : string option;
    }

and dyn_env = value Env.t

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let desugar (p : prog) : expr =
  match p with
  (* LPAREN; RPAREN { SUnit } , if empty derive into () The empty program is equivalent to the unit expression () *)
  | [] -> Unit 
  (* if not empty then its toplet *)
  | _ -> 
    (* let [rec] x1 <arg> : τ1 = e1
    let [rec] x2 <arg> : τ2 = e2
    ...
    let [rec] xk <arg> : τk = ek 
    
    desugars to
    
    let [rec] x1 <arg> : τ1 = e1 in
    let [rec] x2 <arg> : τ2 = e2 in
    ...
    let [rec] xk <arg> : τk = ek in
    xk *)
    let last_name = (List.rev p |> List.hd).name in
      List.fold_right
        (fun (t : toplet) acc ->
          (* each toplet turns into a Let expr *)
          (* type sfexpr =
          | SUnit
          | SBool of bool
          | SNum of int
          | SVar of string
          | SFun of {
              args : (string * ty) list;
              body : sfexpr;
            }
          | SApp of sfexpr list
          | SLet of {
              is_rec : bool;
              name : string;
              args : (string * ty) list;
              ty : ty;
              binding : sfexpr;
              body : sfexpr;
            }
          | SIf of sfexpr * sfexpr * sfexpr
          | SBop of bop * sfexpr * sfexpr
          | SAssert of sfexpr *)
          let rec aux (e : sfexpr) : expr =
            match e with
            | SUnit -> Unit
            | SBool b -> Bool b
            | SNum n -> Num n
            | SVar x -> Var x
            | SIf (e1, e2, e3) -> If (aux e1, aux e2, aux e3)
            | SBop (op, e1, e2) -> Bop (op, aux e1, aux e2)
            | SAssert e -> Assert (aux e)
            (* we sitll need to do fun app and let *)
            (* List.fold_right to create nested Fun expressions for each argument in args *)
            | SLet { is_rec; name; args; ty; binding; body } ->
              let binding' = aux binding in
              let body' = aux body in
              let binding_with_args = List.fold_right (fun (arg_name, arg_ty) acc -> Fun (arg_name, arg_ty, acc)) args binding'
              in
              let binding_ty = List.fold_right (fun (_, arg_ty) acc -> FunTy (arg_ty, acc)) args ty in
              Let { is_rec; name; ty = binding_ty; binding = binding_with_args; body = body' }
            | SFun { args; body } ->
              let body' = aux body in
              List.fold_right (fun (arg_name, arg_ty) acc -> Fun (arg_name, arg_ty, acc)) args body'
            | SApp es ->
              match es with
              | [] -> Unit
              | e :: rest -> List.fold_left (fun acc e' -> App (acc, aux e')) (aux e) rest
          in
          let binding = aux t.binding in
          let bindings_args = List.fold_right (fun (arg_name, arg_ty) acc -> Fun (arg_name, arg_ty, acc)) t.args binding in
          let bindings_ty = List.fold_right (fun (_, arg_ty) acc -> FunTy (arg_ty, acc)) t.args t.ty in
          Let { is_rec = t.is_rec; name = t.name; ty = bindings_ty; binding = bindings_args (* need to implement binding*); body = acc })
        p
        (Var last_name)


let type_of (e : expr) : (ty, error) result =
  let rec type_of (ctxt : ty Env.t) (e : expr) : (ty, error) result =
    let rec go e =
      match e with
      (* ------------------------ *)
      (* ctxt |- () : unit *)
      | Unit -> Ok UnitTy
      (* -------------------------- *)
      (* ctxt |- true : bool *)
      (* and *)
      (* ----------------------- *)
      (* ctxt |- false : bool *)
      | Bool _ -> Ok BoolTy
      (* --------------------- *)
      (* ctxt |- n : int *)
      | Num _ -> Ok IntTy
      (* x : ty in ctxt *)
      (* ------------------- *)
      (* ctxt |- x : ty *)
      | Var x ->
          (match Env.find_opt x ctxt with
            | Some ty -> Ok ty
            | None -> Error (UnknownVar x))
      (* ctxt, x : ty |- e : ty' *)
      (* ------------------------------------------- *)
      (* ctxt |- fun x : ty -> e : ty -> ty' *)
      | Fun (x, ty, e) ->
          let ctxt = Env.add x ty ctxt in
          (match type_of ctxt e with
            | Ok ty' -> Ok (FunTy (ty, ty'))
            | Error err -> Error err)
      (* ctxt |- e1 : ty -> ty'    ctxt |- e2 : ty *)
      (* ---------------------------------------------- *)
      (* ctxt |- e1 e2 : ty' *)
      | App (e1, e2) ->
          (match go e1 with
            | Ok (FunTy (ty, ty')) ->
                (match go e2 with
                | Ok ty'' ->
                    if ty'' = ty then Ok ty'
                    else Error (FunArgTyErr (ty, ty''))
                | Error err -> Error err)
            | Ok ty_a -> Error (FunAppTyErr ty_a)
            | Error err -> Error err)
      (* ctxt |- e1 : int    ctxt |- e2 : int *)
      (* --------------------------------------------- *) (* for Add, Sub, Mul, Div, Mod *)
      (* ctxt |- e1 op e2 : int *)

      (* and *)

      (* ctxt |- e1 : int    ctxt |- e2 : int *)
      (* -------------------------------------------- *) (* for Lt, Lte, Gt, Gte, Eq, Neq *)
      (* ctxt |- e1 op e2 : bool *)

      (* and *)

      (* ctxt |- e1 : bool    ctxt |- e2 : bool *)
      (* ---------------------------------------------------- *) (* for And, Or *)
      (* ctxt |- e1 op e2 : bool *)
      | Bop (bop, e1, e2) ->
          (match go e1 with
            | Error err -> Error err
            | Ok ty1 ->
                let expected_ty = match bop with
                  | Add | Sub | Mul | Div | Mod | Lt | Lte | Gt | Gte | Eq | Neq -> IntTy
                  | And | Or -> BoolTy
                in
                if ty1 <> expected_ty then Error (OpTyErrL (bop, expected_ty, ty1))
                else
                  (match go e2 with
                  | Error err -> Error err
                  | Ok ty2 ->
                      if ty2 <> expected_ty then Error (OpTyErrR (bop, expected_ty, ty2))
                      else
                        match bop with
                        | Add | Sub | Mul | Div | Mod -> Ok IntTy
                        | Lt | Lte | Gt | Gte | Eq | Neq -> Ok BoolTy
                        | And | Or -> Ok BoolTy))

      (* ctxt |- e1 : bool    ctxt |- e2 : ty    ctxt |- e3 : ty *)
      (* --------------------------------------------------------------- *)
      (* ctxt |- if e1 then e2 else e3 : ty *)
    
      | If (e1, e2, e3) ->
          (match go e1 with
            | Error err -> Error err
            | Ok ty1 ->
                if ty1 <> BoolTy then Error (IfCondTyErr ty1)
                else
                  (match go e2 with
                  | Error err -> Error err
                  | Ok t1 ->
                      (match go e3 with
                        | Error err -> Error err
                        | Ok t2 ->
                            if t1 = t2 then Ok t1
                            else Error (IfTyErr (t1, t2)))))
      (* recursive case *)
      (* ctxt, f : ty1 -> ty2 |- e1 : ty2    ty = ty1 -> ty2    e1 = fun x : ty1 -> e *)
      (* -------------------------------------------------------------------------------- *)
      (* ctxt |- let rec f : ty = e1 in e2 : ty' *)
      | Let { is_rec; name; ty = ty_ann; binding; body } ->
          if is_rec then
            (* Recursive let: binding must be a function *)
            (match ty_ann with
              | FunTy (arg_ty, ret_ty) ->
                  let ctxt = Env.add name ty_ann ctxt in
                  (match binding with
                  | Fun (arg, arg_ty', binding_body) ->
                      if arg_ty = arg_ty' then
                        let ctxt' = Env.add arg arg_ty ctxt in
                        (match type_of ctxt' binding_body with
                          | Ok ty ->
                              if ty = ret_ty then type_of ctxt body
                              else Error (LetTyErr (ret_ty, ty))
                          | Error err -> Error err)
                      else Error (LetTyErr (arg_ty, arg_ty'))
                  | _ -> Error (LetRecErr name))
              | _ -> Error (LetRecErr name))
          else
            (* non recursive case *)
            (* ctxt |- e1 : ty    ctxt, x : ty |- e2 : ty' *)
            (* ------------------------------------------- *)
            (* ctxt |- let x : ty = e1 in e2 : ty' *)
            (match go binding with
              | Error err -> Error err
              | Ok ty ->
                  if ty = ty_ann then
                    let ctxt = Env.add name ty_ann ctxt in
                    type_of ctxt body
                  else Error (LetTyErr (ty_ann, ty)))
      | Assert e ->
          (match go e with
            | Error err -> Error err
            | Ok ty ->
                if ty = BoolTy then Ok UnitTy
                else Error (AssertTyErr ty))
    in
    go e
  in
  type_of Env.empty e

exception AssertFail
exception DivByZero

let eval (e : expr) :  value =
  let rec eval (env : dyn_env) (e : expr) : value =
    match e with
    (* FOLLOW THE SPEC.pdf semantics *)
    (* Literals *)
    | Unit -> VUnit
    | Bool b -> VBool b
    | Num n -> VNum n 
    (* Variables *)
    | Var x ->
      (match Env.find_opt x env with
        | Some v -> v
        | None -> raise (Failure (err_msg (UnknownVar x))))
    (* Conditionals *)
    | If (e1, e2, e3) ->
      (match eval env e1 with
        | VBool true -> eval env e2
        | VBool false -> eval env e3
        | _ -> raise (Failure (err_msg (IfCondTyErr BoolTy))))
    (* Operators *)
    | Bop (op, e1, e2) ->
      if op = And then
        (* adding short circuit for and *)
        (match eval env e1 with
         | VBool false -> VBool false
         | VBool true ->
             (match eval env e2 with
              | VBool b -> VBool b
              | _ -> raise (Failure (err_msg (OpTyErrR (And, BoolTy, IntTy)))))
         | _ -> raise (Failure (err_msg (OpTyErrL (And, BoolTy, IntTy)))))
      else if op = Or then
        (* ading short circuit for or *)
        (match eval env e1 with
         | VBool true -> VBool true
         | VBool false ->
             (match eval env e2 with
              | VBool b -> VBool b
              | _ -> raise (Failure (err_msg (OpTyErrR (Or, BoolTy, IntTy)))))
         | _ -> raise (Failure (err_msg (OpTyErrL (Or, BoolTy, IntTy)))))
      else
        (* everything else stays same *)
        let v1 = eval env e1 in
        let v2 = eval env e2 in
        (match op, v1, v2 with
         | Add, VNum m, VNum n -> VNum (m + n)
         | Sub, VNum m, VNum n -> VNum (m - n)
         | Mul, VNum m, VNum n -> VNum (m * n)
         | Div, VNum m, VNum n ->
             if n = 0 then raise DivByZero
             else VNum (m / n)
         | Mod, VNum m, VNum n ->
             if n = 0 then raise DivByZero
             else VNum (m mod n)
         | Lt, VNum m, VNum n -> VBool (m < n)
         | Lte, VNum m, VNum n -> VBool (m <= n)
         | Gt, VNum m, VNum n -> VBool (m > n)
         | Gte, VNum m, VNum n -> VBool (m >= n)
         | Eq, VNum m, VNum n -> VBool (m = n)
         | Neq, VNum m, VNum n -> VBool (m <> n)
         | _, _, _ -> (* Error *)
             let ty1 = match v1 with
               | VNum _ -> IntTy
               | VBool _ -> BoolTy
               | VUnit -> UnitTy
               | VClos _ -> FunTy (IntTy, IntTy)
             in
             raise (Failure (err_msg (OpTyErrL (op, IntTy, ty1)))))
    (* Functions *)
    | Fun (x, _, e) -> VClos { arg = x; body = e; env; name = None }
    | App (e1, e2) ->
      (match eval env e1 with
        | VClos { arg; body; env = closure_env; name = None } ->
          let arg_val = eval env e2 in
          let new_env = Env.add arg arg_val closure_env in
          eval new_env body
        | _ -> raise (Failure (err_msg (FunAppTyErr (FunTy (IntTy, IntTy))))))
    (* Let-Expressions *)
    | Let { is_rec; name; ty = _; binding; body } ->
      (* Recursive case *)
      if is_rec then
        (match binding with
         | Fun (arg, _, fun_body) ->
            (* First create the closure with a placeholder environment *)
            let clos = VClos { arg; body = fun_body; env = env; name = Some name } in
            (* Update the environment to include the closure *)
            let new_env = Env.add name clos env in
            (* Update the closure's environment to include itself *)
            let clos = VClos { arg; body = fun_body; env = new_env; name = Some name } in
            (* Update the environment again with the final closure *)
            let final_env = Env.add name clos new_env in
            eval final_env body
         | _ -> raise (Failure (err_msg (LetRecErr name))))
      else
        (* non recursive case, same as before *)
        let v1 = eval env binding in
        eval (Env.add name v1 env) body
    (* Assert *)
    | Assert e ->
      (match eval env e with
       | VBool true -> VUnit
       | VBool false -> raise AssertFail
       | _ -> raise (Failure (err_msg (AssertTyErr BoolTy))))
  in
  eval Env.empty e


let interp (s : string) : (value, error) result =
  match parse s with
  (* string -> prog option *)
  | Some prog -> (
    let expr = desugar prog in
    match type_of expr with
    (* expr -> (ty, error) result *)
    | Ok _ -> Ok (eval expr)
    | Error err -> Error err
  )
  | None -> Error ParseErr
  

