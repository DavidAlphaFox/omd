(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013-2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>         *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Ast

type delim =
  | Ws
  | Punct
  | Other

type t =
  | Bang_left_bracket
  | Left_bracket
  | Emph of delim * delim * Ast.emph_style * int
  | R of Ast.inline

let left_flanking = function
  | Emph (_, Other, _, _) | Emph ((Ws | Punct), Punct, _, _) -> true
  | _ -> false

let right_flanking = function
  | Emph (Other, _, _, _) | Emph (Punct, (Ws | Punct), _, _) -> true
  | _ -> false

let is_opener = function
  | Emph (pre, _, Underscore, _) as x ->
      left_flanking x && (not (right_flanking x) || pre = Punct)
  | Emph (_, _, Star, _) as x ->
      left_flanking x
  | _ ->
      false

let is_closer = function
  | Emph (_, post, Underscore, _) as x ->
      right_flanking x && (not (left_flanking x) || post = Punct)
  | Emph (_, _, Star, _) as x ->
      right_flanking x
  | _ ->
      false

let classify_delim = function
  | '!' | '"' | '#' | '$' | '%'
  | '&' | '\'' | '(' | ')' | '*' | '+'
  | ',' | '-' | '.' | '/' | ':' | ';'
  | '<' | '=' | '>' | '?' | '@' | '['
  | '\\' | ']' | '^' | '_' | '`' | '{'
  | '|' | '}' | '~' -> Punct
  | ' ' | '\t' | '\010'..'\013' -> Ws
  | _ -> Other

let to_r : _ -> Ast.inline = function
  | Bang_left_bracket -> Text "!["
  | Left_bracket -> Text "["
  | Emph (_, _, Star, n) -> Text (String.make n '*')
  | Emph (_, _, Underscore, n) -> Text (String.make n '_')
  | R x -> x

let rec parse_emph = function
  | Emph (pre, _, q1, n1) as x :: xs when is_opener x ->
      let rec loop acc = function
        | Emph (_, post, q2, n2) as x :: xs when is_closer x && q1 = q2 ->
            let xs =
              if n1 >= 2 && n2 >= 2 then
                if n2 > 2 then Emph (Punct, post, q2, n2-2) :: xs else xs
              else
                if n2 > 1 then Emph (Punct, post, q2, n2-1) :: xs else xs
            in
            let r =
              let kind = if n1 >= 2 && n2 >= 2 then Strong else Normal in
              R (Emph (kind, q1, Ast.concat (parse_emph (List.rev acc)))) :: xs
            in
            let r =
              if n1 >= 2 && n2 >= 2 then
                if n1 > 2 then Emph (pre, Punct, q1, n1-2) :: r else r
              else
                if n1 > 1 then Emph (pre, Punct, q1, n1-1) :: r else r
            in
            parse_emph r
        | x :: xs ->
            loop (x :: acc) xs
        | [] ->
            to_r x :: List.rev_map to_r acc
      in
      loop [] xs
  | x :: xs ->
      to_r x :: parse_emph xs
  | [] ->
      []

let escape_markdown_characters s =
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '.' as c ->
        if i > 0 &&
           match s.[i-1] with
           | '0' .. '9' -> i+1 < String.length s && s.[i+1] = ' '
           | _ -> false
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '-' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false) &&
           (i+1 < String.length s && (s.[i+1] = ' '||s.[i+1] = '-'))
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '+' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false) &&
           (i+1 < String.length s && s.[i+1] = ' ')
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '!' as c ->
        if i+1 < String.length s && s.[i+1] = '[' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | '<' as c ->
        if i <> String.length s - 1 &&
           (match s.[i+1] with 'a' .. 'z' | 'A' .. 'Z' -> false | _ -> true)
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '>' as c ->
        if i = 0 || (match s.[i-1] with ' ' | '\n' -> false | _ -> true) then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '#' as c ->
        if i = 0 || s.[i-1] = '\n' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | '\\' | '[' | ']' | '(' | ')' | '`' | '*' as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
    | c ->
        Buffer.add_char b c
  done;
  Buffer.contents b

let rec markdown_of_md md =
  let b = Buffer.create 64 in
  let rec loop = function
    | Concat l ->
        List.iter loop l
    | Img (alt, src, title) ->
        Printf.bprintf b "![%s](%s \"%s\")" alt src title
    | Text t ->
        Printf.bprintf b "%s" (escape_markdown_characters t)
    | Emph (Normal, q, md) ->
        let delim = String.make 2 (match q with Star -> '*' | Underscore -> '_') in
        Buffer.add_string b delim;
        loop md;
        Buffer.add_string b delim
    | Emph (Strong, q, md) ->
        let delim = String.make 2 (match q with Star -> '*' | Underscore -> '_') in
        Buffer.add_string b delim;
        loop md;
        Buffer.add_string b delim;
    | Code c ->
        let n = (* compute how many backquotes we need to use *)
          let filter (n:int) (s:int list) =
            if n > 0 && n < 10 then
              List.filter (fun e -> e <> n) s
            else
              s
          in
          let l = String.length c in
          let rec loop s x b i =
            if i = l then begin
              match filter b s with hd :: _ -> hd | [] -> x+1
            end else begin
              match c.[i] with
              | '`' ->
                  loop s x (succ b) (succ i)
              | _ ->
                  loop (filter b s) (max b x) 0 (succ i)
            end
          in
          loop [1;2;3;4;5;6;7;8;9;10] 0 0 0
        in
        Printf.bprintf b "%s" (String.make n '`');
        if c.[0] = '`' then Buffer.add_char b ' ';
        Printf.bprintf b "%s" c;
        if c.[String.length c - 1] = '`' then Buffer.add_char b ' ';
        Printf.bprintf b "%s" (String.make n '`')
    | Hard_break ->
        Buffer.add_string b "<br />"
    | Html body ->
        Buffer.add_string b body
    | Url (s, href, None) ->
        Printf.bprintf b "[%s](%s)" (markdown_of_md s) href
    | Url (s, href, Some title) ->
        Printf.bprintf b "[%s](%s \"%s\")" (markdown_of_md s) href title
    | Soft_break ->
        if Buffer.length b = 1 ||
           (Buffer.length b > 1 &&
            not(Buffer.nth b (Buffer.length b - 1) = '\n' &&
                Buffer.nth b (Buffer.length b - 2) = '\n'))
        then
          Buffer.add_string b "\n"
  in
  loop md;
  let res = Buffer.contents b in
  res

let () = ignore [markdown_of_md]
