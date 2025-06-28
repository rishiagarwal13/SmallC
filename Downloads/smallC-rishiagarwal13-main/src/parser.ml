open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let rec parse_expr tokens : expr_result =
  parse_logical_or tokens

and parse_logical_or tokens =
  let (remaining_tokens, left_subexpr) = parse_logical_and tokens in
  match remaining_tokens with
  | Tok_Or :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_logical_or rest_tokens in
      (final_tokens, Or (left_subexpr, right_subexpr))
  | _ -> (remaining_tokens, left_subexpr)

and parse_logical_and tokens =
  let (remaining_tokens, left_subexpr) = parse_equality tokens in
  match remaining_tokens with
  | Tok_And :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_logical_and rest_tokens in
      (final_tokens, And (left_subexpr, right_subexpr))
  | _ -> (remaining_tokens, left_subexpr)

and parse_equality tokens =
  let (remaining_tokens, left_subexpr) = parse_relational tokens in
  match remaining_tokens with
  | Tok_Equal :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_equality rest_tokens in
      (final_tokens, Equal (left_subexpr, right_subexpr))
  | Tok_NotEqual :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_equality rest_tokens in
      (final_tokens, NotEqual (left_subexpr, right_subexpr))
  | _ -> (remaining_tokens, left_subexpr)

and parse_relational tokens =
  let (remaining_tokens, left_subexpr) = parse_additive tokens in
  match remaining_tokens with
  | Tok_Less :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_relational rest_tokens in
      (final_tokens, Less (left_subexpr, right_subexpr))
  | Tok_Greater :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_relational rest_tokens in
      (final_tokens, Greater (left_subexpr, right_subexpr))
  | Tok_LessEqual :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_relational rest_tokens in
      (final_tokens, LessEqual (left_subexpr, right_subexpr))
  | Tok_GreaterEqual :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_relational rest_tokens in
      (final_tokens, GreaterEqual (left_subexpr, right_subexpr))
  | _ -> (remaining_tokens, left_subexpr)

and parse_additive tokens =
  let (remaining_tokens, left_subexpr) = parse_multiplicative tokens in
  match remaining_tokens with
  | Tok_Add :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_additive rest_tokens in
      (final_tokens, Add (left_subexpr, right_subexpr))
  | Tok_Sub :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_additive rest_tokens in
      (final_tokens, Sub (left_subexpr, right_subexpr))
  | _ -> (remaining_tokens, left_subexpr)

and parse_multiplicative tokens =
  let (remaining_tokens, left_subexpr) = parse_power tokens in
  match remaining_tokens with
  | Tok_Mult :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_multiplicative rest_tokens in
      (final_tokens, Mult (left_subexpr, right_subexpr))
  | Tok_Div :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_multiplicative rest_tokens in
      (final_tokens, Div (left_subexpr, right_subexpr))
  | _ -> (remaining_tokens, left_subexpr)

and parse_power tokens =
  let (remaining_tokens, left_subexpr) = parse_unary tokens in
  match remaining_tokens with
  | Tok_Pow :: rest_tokens ->
      let (final_tokens, right_subexpr) = parse_power rest_tokens in
      (final_tokens, Pow (left_subexpr, right_subexpr))
  | _ -> (remaining_tokens, left_subexpr)

and parse_unary tokens =
  match lookahead tokens with
  | Tok_Not ->
      let tokens = match_token tokens Tok_Not in
      let (final_tokens, negated_expr) = parse_unary tokens in
      (final_tokens, Not negated_expr)
  | _ -> parse_primary tokens

and parse_primary tokens =
  match lookahead tokens with
  | Tok_Int n ->
      let tokens = match_token tokens (Tok_Int n) in
      (tokens, Int n)
  | Tok_Bool b ->
      let tokens = match_token tokens (Tok_Bool b) in
      (tokens, Bool b)
  | Tok_ID s ->
      let tokens = match_token tokens (Tok_ID s) in
      (tokens, ID s)
  | Tok_LParen ->
      let tokens = match_token tokens Tok_LParen in
      let (tokens, inner_expr) = parse_expr tokens in
      let tokens = match_token tokens Tok_RParen in
      (tokens, inner_expr)
  | _ -> raise (InvalidInputException "Invalid expression")





let rec parse_stmt tokens : stmt_result =
  match lookahead tokens with
  | Tok_RBrace | EOF -> (tokens, NoOp)
  | _ ->
      let (remaining_tokens, current_statement) = parse_statement_option tokens in
      let (final_tokens, subsequent_statements) = parse_stmt remaining_tokens in
      (final_tokens, Seq (current_statement, subsequent_statements))

and parse_statement_option tokens =
  match lookahead tokens with
  | Tok_Int_Type ->
      let tokens = match_token tokens Tok_Int_Type in
      (match lookahead tokens with
        | Tok_ID variable_name ->
            let tokens = match_token tokens (Tok_ID variable_name) in
            let tokens = match_token tokens Tok_Semi in
            (tokens, Declare (Int_Type, variable_name))
        | _ -> raise (InvalidInputException "Invalid int"))
  | Tok_Bool_Type ->
      let tokens = match_token tokens Tok_Bool_Type in
      (match lookahead tokens with
        | Tok_ID variable_name ->
            let tokens = match_token tokens (Tok_ID variable_name) in
            let tokens = match_token tokens Tok_Semi in
            (tokens, Declare (Bool_Type, variable_name))
        | _ -> raise (InvalidInputException "Invalid bool"))
  | Tok_ID variable_name ->
      let tokens = match_token tokens (Tok_ID variable_name) in
      let tokens = match_token tokens Tok_Assign in
      let (tokens, assigned_expression) = parse_expr tokens in
      let tokens = match_token tokens Tok_Semi in
      (tokens, Assign (variable_name, assigned_expression))
  | Tok_Print ->
      let tokens = match_token tokens Tok_Print in
      let tokens = match_token tokens Tok_LParen in
      let (tokens, print_expression) = parse_expr tokens in
      let tokens = match_token tokens Tok_RParen in
      let tokens = match_token tokens Tok_Semi in
      (tokens, Print print_expression)
  | Tok_If ->
      let tokens = match_token tokens Tok_If in
      let tokens = match_token tokens Tok_LParen in
      let (tokens, condition_expr) = parse_expr tokens in
      let tokens = match_token tokens Tok_RParen in
      let tokens = match_token tokens Tok_LBrace in
      let (tokens, if_body_stmt) = parse_stmt tokens in
      let tokens = match_token tokens Tok_RBrace in
      let (tokens, else_body_stmt) = parse_else_branch tokens in
      (tokens, If (condition_expr, if_body_stmt, else_body_stmt))
  | Tok_For ->
      let tokens = match_token tokens Tok_For in
      let tokens = match_token tokens Tok_LParen in
      (match lookahead tokens with
        | Tok_ID loop_variable ->
            let tokens = match_token tokens (Tok_ID loop_variable) in
            let tokens = match_token tokens Tok_From in
            let (tokens, start_expression) = parse_expr tokens in
            let tokens = match_token tokens Tok_To in
            let (tokens, end_expression) = parse_expr tokens in
            let tokens = match_token tokens Tok_RParen in
            let tokens = match_token tokens Tok_LBrace in
            let (tokens, loop_body_stmt) = parse_stmt tokens in
            let tokens = match_token tokens Tok_RBrace in
            (tokens, For (loop_variable, start_expression, end_expression, loop_body_stmt))
        | _ -> raise (InvalidInputException "Invalid for loop"))
  | Tok_While ->
      let tokens = match_token tokens Tok_While in
      let tokens = match_token tokens Tok_LParen in
      let (tokens, condition_expr) = parse_expr tokens in
      let tokens = match_token tokens Tok_RParen in
      let tokens = match_token tokens Tok_LBrace in
      let (tokens, loop_body_stmt) = parse_stmt tokens in
      let tokens = match_token tokens Tok_RBrace in
      (tokens, While (condition_expr, loop_body_stmt))
  | _ -> raise (InvalidInputException "Invalid statement")

and parse_else_branch tokens =
  match lookahead tokens with
  | Tok_Else ->
      let tokens = match_token tokens Tok_Else in
      let tokens = match_token tokens Tok_LBrace in
      let (tokens, else_body_stmt) = parse_stmt tokens in
      let tokens = match_token tokens Tok_RBrace in
      (tokens, else_body_stmt)
  | _ -> (tokens, NoOp)

let parse_main toks : stmt =
  let toks = match_token toks Tok_Int_Type in
  let toks = match_token toks Tok_Main in
  let toks = match_token toks Tok_LParen in
  let toks = match_token toks Tok_RParen in
  let toks = match_token toks Tok_LBrace in
  let (toks, stmt) = parse_stmt toks in
  let toks = match_token toks Tok_RBrace in
  match toks with
  | [EOF] -> stmt
  | _ -> raise (InvalidInputException "Extra tokens")
