open TokenTypes

let tokenize input =
  let len = String.length input in

  let is_whitespace c = c = ' ' || c = '\t' || c = '\n' in
  let is_digit c = c >= '0' && c <= '9' in
  let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in

  let peek2 pos =
    if pos + 1 < len then Some (input.[pos], input.[pos + 1]) else None
  in

  let rec consume_digits i =
    if i < len && is_digit input.[i] then consume_digits (i + 1)
    else i
  in

  let rec consume_ident i =
    if i < len && (is_letter input.[i] || is_digit input.[i]) then consume_ident (i + 1)
    else i
  in

  let rec lex pos =
    if pos >= len then [EOF]
    else if is_whitespace input.[pos] then
      lex (pos + 1)
    else
      match peek2 pos with
      | Some ('=', '=') -> Tok_Equal :: lex (pos + 2)
      | Some ('!', '=') -> Tok_NotEqual :: lex (pos + 2)
      | Some ('>', '=') -> Tok_GreaterEqual :: lex (pos + 2)
      | Some ('<', '=') -> Tok_LessEqual :: lex (pos + 2)
      | Some ('|', '|') -> Tok_Or :: lex (pos + 2)
      | Some ('&', '&') -> Tok_And :: lex (pos + 2)
      | _ ->
          let c = input.[pos] in
          match c with
          | '(' -> Tok_LParen :: lex (pos + 1)
          | ')' -> Tok_RParen :: lex (pos + 1)
          | '{' -> Tok_LBrace :: lex (pos + 1)
          | '}' -> Tok_RBrace :: lex (pos + 1)
          | ';' -> Tok_Semi :: lex (pos + 1)
          | '+' -> Tok_Add :: lex (pos + 1)
          | '^' -> Tok_Pow :: lex (pos + 1)
          | '*' -> Tok_Mult :: lex (pos + 1)
          | '/' -> Tok_Div :: lex (pos + 1)
          | '>' -> Tok_Greater :: lex (pos + 1)
          | '<' -> Tok_Less :: lex (pos + 1)
          | '=' -> Tok_Assign :: lex (pos + 1)
          | '!' -> Tok_Not :: lex (pos + 1)

          | '-' ->
              if pos + 1 < len && is_digit input.[pos + 1] then
                let end_pos = consume_digits (pos + 1) in
                let num = String.sub input pos (end_pos - pos) in
                Tok_Int (int_of_string num) :: lex end_pos
              else Tok_Sub :: lex (pos + 1)

          | c when is_digit c ->
              let end_pos = consume_digits pos in
              let num = String.sub input pos (end_pos - pos) in
              Tok_Int (int_of_string num) :: lex end_pos

          | c when is_letter c ->
              let end_pos = consume_ident pos in
              let word = String.sub input pos (end_pos - pos) in
              let token =
                match word with
                | "int" -> Tok_Int_Type
                | "bool" -> Tok_Bool_Type
                | "while" -> Tok_While
                | "for" -> Tok_For
                | "from" -> Tok_From
                | "to" -> Tok_To
                | "if" -> Tok_If
                | "else" -> Tok_Else
                | "printf" -> Tok_Print
                | "main" -> Tok_Main
                | "true" -> Tok_Bool true
                | "false" -> Tok_Bool false
                | _ -> Tok_ID word
              in
              token :: lex end_pos

          | _ ->
              let msg = Printf.sprintf "Unexpected character '%c' at position %d" c pos in
              raise (InvalidInputException msg)
  in

  lex 0
