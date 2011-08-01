
(** Transform a Coq file with Breacetax comments to Bracetax *)
val coq_brtx : ?fmt:[ `html | `latex ] -> string -> string

(** Transform a OCaml file with Breacetax comments to Bracetax *)
val ocaml_brtx : ?fmt:[ `html | `latex ] -> string -> string
