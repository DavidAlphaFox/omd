(** A markdown parser in OCaml. *)

module Ast = Ast
module Html = Html

val of_channel: in_channel -> Ast.t

val of_string: string -> Ast.t

val default_printer: Html.printer

val to_html: ?printer:Html.printer -> Ast.t -> string

val to_sexp: Ast.t -> string
