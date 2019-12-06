type attributes =
  {
    id: string option;
    classes: string list;
    attributes: (string * string) list;
  }

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
    attributes: attributes;
  }

type code_block_kind =
  | Tilde
  | Backtick

type emph_kind =
  | Normal
  | Strong

type emph_style =
  | Star
  | Underscore

type link_kind =
  | Img
  | Url

module Block_list = struct
  type kind =
    | Ordered of int * char
    | Unordered of char

  type style =
    | Loose
    | Tight
end

type 'a elt =
  {
    term: 'a;
    defs: 'a list;
  }

type inline =
  | Concat of inline list
  | Text of string
  | Emph of
      {
        style: emph_style;
        kind: emph_kind;
        content: inline;
      }
  | Code of
      {
        level: int;
        content: string;
        attributes: attributes;
      }
  | Hard_break
  | Soft_break
  | Link of
      {
        kind: link_kind;
        def: inline link_def;
      }
  | Ref of
      {
        kind: link_kind;
        label: inline;
        def: string link_def;
      }
  | Html of string
  | Tag of
      {
        tag: string;
        content: inline;
        attributes: attributes;
      }

type block =
  | Paragraph of inline
  | List of
      {
        kind: Block_list.kind;
        style: Block_list.style;
        blocks: block list list;
      }
  | Blockquote of block list
  | Thematic_break
  | Heading of
      {
        level: int;
        text: inline;
        attributes: attributes;
      }
  | Code_block of
      {
        kind: code_block_kind option;
        label: string option;
        other: string option;
        code: string option;
        attributes: attributes;
      }
  | Html_block of string
  | Link_def of string link_def
  | Def_list of
      {
        content: inline elt list
      }
  | Tag_block of
      {
        tag: string;
        content: block list;
        attributes: attributes;
      }

let rec map f = function
  | Paragraph x ->
      Paragraph (f x)
  | List l ->
      List  {l with blocks = List.map (List.map (map f)) l.blocks}
  | Blockquote xs ->
      Blockquote (List.map (map f) xs)
  | Heading {level; text; attributes} ->
      Heading {level; text = f text; attributes}
  | Def_list {content} ->
      let content =
        List.map (fun elt -> {term = f elt.term; defs = List.map f elt.defs}) content
      in
      Def_list {content}
  | Tag_block t ->
      Tag_block {t with content = List.map (map f) t.content}
  | Thematic_break
  | Code_block _
  | Html_block _
  | Link_def _ as x -> x

let defs ast =
  let rec loop acc = function
    | List l -> List.fold_left (List.fold_left loop) acc l.blocks
    | Blockquote l | Tag_block {content = l; _} -> List.fold_left loop acc l
    | Paragraph _ | Thematic_break | Heading _
    | Def_list _ | Code_block _ | Html_block _ -> acc
    | Link_def def -> def :: acc
  in
  List.rev (List.fold_left loop [] ast)
