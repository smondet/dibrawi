open Dibrawi_std

(** create a directory but doesn't raise an exception if the
    directory * already exist *)
let mkdir_safe dir perm =
  try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** create a directory, and create parent if doesn't exist
    i.e. mkdir -p *)
let mkdir_p ?(perm=0o700) dir =
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    if p_name <$> "/" && p_name <$> "."
    then p_mkdir p_name;
    mkdir_safe dir perm in
  p_mkdir dir 

let with_new_out filename f = 
  let o = open_out filename in
  try let r = f o in close_out o; r with e -> close_out o; raise e

let with_file_in filename f = 
  let i = open_in filename in
  try let r = f i in close_in i; r with e -> close_in i; raise e

let with_new_tmp ?(suffix=".tmp") ?(prefix="dbw_") f =
  let name, o = Filename.open_temp_file prefix suffix in
  try let r = f o name in close_out o; r with e -> close_out o; raise e

    
let copy src dest =
  with_file_in src 
    (fun i ->
      with_new_out dest
        (fun o ->
          try while true do output_char o (input_char i) done;
          with End_of_file -> ()))


let read_command_output f s =
  let ic = Unix.open_process_in s in
  (try
     while true do
       f (input_char ic)
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> ()
  | _ -> invalid_arg ("read_command_output: " ^ s)

let slurp_command s =
  let buf = Buffer.create 100 in
  read_command_output (Buffer.add_char buf) s;
  Buffer.contents buf

let is_newer filea fileb =
  let stata = Unix.stat filea and statb = Unix.stat fileb in
  stata.Unix.st_mtime > statb.Unix.st_mtime
        
let run_command c =
  match Unix.system c with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith (sprintf "Command exited with non-zero: %s" c)


module Feed = struct
      (* Warning: the following function crashes in OCaml 3.09.2,
         because of that bug: http://caml.inria.fr/mantis/view.php?id=4062
         (close_out is applied a second time during Unix.close_process) *)
  let kfeed f command data =
    let (ic, oc) as channels = Unix.open_process command in
    output_string oc data;
    close_out oc;
    let exn = ref None in
    begin 
      try
        while true do
          f (input_char ic)
        done
      with
      | End_of_file -> ()
      | e -> exn := Some e
    end;
    begin match Unix.close_process channels with
    | Unix.WEXITED 0 -> ()
    | _ -> invalid_arg ("feed_command: " ^ command)
    end;
    (match !exn with Some e -> raise e | None -> ())

  let feed = kfeed print_char
    
  let ffeed oc command data = kfeed (output_char oc) command data
    
  let bfeed buf command data = kfeed (Buffer.add_char buf) command data
    
  let str command data = 
    let buf = Buffer.create (2 * String.length data) in
    bfeed buf command data;
    Buffer.contents buf
end

let feed ~cmd ~input = Feed.str cmd input


