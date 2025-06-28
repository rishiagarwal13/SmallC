open SmallCTypes
open Utils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
  match t with
  | Int n -> Int_Val n
  | Bool b -> Bool_Val b
  | ID s ->
      (try List.assoc s env
       with Not_found -> raise (DeclareError ("Variable " ^ s ^ " not declared")))
  | Add (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Int_Val (n1 + n2)
      | _ -> raise (TypeError "Addition error"))
  | Sub (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Int_Val (n1 - n2)
      | _ -> raise (TypeError "Subtraction error"))
  | Mult (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Int_Val (n1 * n2)
      | _ -> raise (TypeError "Multiplication error"))
  | Div (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) ->
          if n2 = 0 then raise DivByZeroError else Int_Val (n1 / n2)
      | _ -> raise (TypeError "Division error"))
  | Pow (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) ->
          let result =
            if n2 < 0 then 
              int_of_float (floor ((float_of_int n1) ** (float_of_int n2)))
            else
              let rec pow a b = 
                if b = 0 then 1 
                else a * pow a (b - 1) 
              in
              pow n1 n2
          in 
          Int_Val result
      | _ -> raise (TypeError "Exponentiation error"))
  | Greater (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Bool_Val (n1 > n2)
      | _ -> raise (TypeError "Greater than error"))
  | Less (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Bool_Val (n1 < n2)
      | _ -> raise (TypeError "Less than error"))
  | GreaterEqual (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Bool_Val (n1 >= n2)
      | _ -> raise (TypeError "Greater or equal error"))
  | LessEqual (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Bool_Val (n1 <= n2)
      | _ -> raise (TypeError "Less or equal error"))
  | Equal (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Bool_Val (n1 = n2)
      | (Bool_Val b1, Bool_Val b2) -> Bool_Val (b1 = b2)
      | _ -> raise (TypeError "Equality error"))
  | NotEqual (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Int_Val n1, Int_Val n2) -> Bool_Val (n1 <> n2)
      | (Bool_Val b1, Bool_Val b2) -> Bool_Val (b1 <> b2)
      | _ -> raise (TypeError "Inequality error"))
  | Or (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Bool_Val b1, Bool_Val b2) -> Bool_Val (b1 || b2)
      | _ -> raise (TypeError "Logical or error"))
  | And (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (v1, v2) with
      | (Bool_Val b1, Bool_Val b2) -> Bool_Val (b1 && b2)
      | _ -> raise (TypeError "Logical and error"))
  | Not e ->
      let v = eval_expr env e in
      (match v with
      | Bool_Val b -> Bool_Val (not b)
      | _ -> raise (TypeError "Logical not error"))

let rec eval_stmt env s =
  match s with
  | NoOp -> 
      env  
      
  | Seq (s1, s2) ->
      let env' = eval_stmt env s1 in  
      eval_stmt env' s2  
      
  | Declare (t, id) ->
      if List.mem_assoc id env then
        raise (DeclareError ("Variable " ^ id ^ " already declared"))
      else
        let default_val = match t with
          | Int_Type -> Int_Val 0
          | Bool_Type -> Bool_Val false
        in
        (id, default_val) :: env  
        
  | Assign (id, expr) ->
      let value = eval_expr env expr in
      (match List.assoc_opt id env with
        | None -> 
            raise (DeclareError ("Variable " ^ id ^ " not declared"))
        | Some (Int_Val _) ->
            (match value with
            | Int_Val _ -> 
                (id, value) :: List.remove_assoc id env
            | _ -> 
                raise (TypeError "Can't assign int"))
        | Some (Bool_Val _) ->
            (match value with
            | Bool_Val _ -> 
                (id, value) :: List.remove_assoc id env
            | _ -> 
                raise (TypeError "Can't assign bool")))
                
  | If (cond, s_then, s_else) ->
      (match eval_expr env cond with
        | Bool_Val true -> 
            eval_stmt env s_then
        | Bool_Val false -> 
            eval_stmt env s_else
        | _ -> 
            raise (TypeError "If condition error"))
            
  | While (cond, body) ->
      let rec loop env =
        match eval_expr env cond with
        | Bool_Val true ->
            let env' = eval_stmt env body in
            loop env'
        | Bool_Val false -> 
            env
        | _ -> 
            raise (TypeError "While condition error")
      in
      loop env
      
  | For (id, start_expr, end_expr, body) ->
      (match (eval_expr env start_expr, eval_expr env end_expr) with
        | (Int_Val start_val, Int_Val end_val) ->
            let env_with_var = (id, Int_Val start_val) :: List.remove_assoc id env in
            
            let rec loop env current_val =
              if current_val > end_val then 
                env
              else
                let env_after_body = eval_stmt env body in
                
                let updated_val = 
                  match List.assoc_opt id env_after_body with
                  | Some (Int_Val n) -> n + 1  
                  | _ -> raise (TypeError "Loop variable error")
                in
                
                let env_next = (id, Int_Val updated_val) :: 
                              List.remove_assoc id env_after_body in
                
                loop env_next updated_val
            in
            
            loop env_with_var start_val
        | _ -> 
            raise (TypeError "For loop bounds error"))
            
  | Print expr ->
    let value = eval_expr env expr in
    let _ = 
      match value with
      | Int_Val i ->
          let _ = print_output_int i in
          print_output_newline ()
      | Bool_Val b ->
          let _ = print_output_bool b in
          print_output_newline ()
    in
    env