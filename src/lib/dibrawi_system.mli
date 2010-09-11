(** create a directory but doesn't raise an exception if the
    directory * already exist *)
val mkdir_safe:  string -> Unix.file_perm -> unit

(** create a directory, and create parent if doesn't exist
    i.e. mkdir -p *)
val mkdir_p:  ?perm:Unix.file_perm -> string -> unit

val with_new_out : string -> (out_channel -> 'a) -> 'a

val with_file_in: string -> (in_channel -> 'a) -> 'a

val with_new_tmp: 
  ?suffix:string ->
  ?prefix:string ->
  (out_channel -> string -> 'a) -> 'a
    
val copy: string -> string -> unit

val slurp_command : string -> string
val is_newer : string -> string -> bool
val run_command : string -> unit
val feed: cmd:string -> input:string -> string

module Date: sig
  val rfc_822: unit -> string
end
