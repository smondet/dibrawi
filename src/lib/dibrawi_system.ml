open Dibrawi_std

(** create a directory but doesn't raise an exception if the
    directory * already exist *)
let mkdir_safe dir perm =
  try Unix.mkdir dir ~perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** create a directory, and create parent if doesn't exist
    i.e. mkdir -p *)
let mkdir_p ?(perm=0o700) dir =
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    if p_name <$> "/" && p_name <$> "."
    then p_mkdir p_name;
    mkdir_safe dir perm in
  p_mkdir dir 
    
let copy src dest =
  In_channel.with_file src ~f:(fun i ->
    Out_channel.with_file dest ~f:(fun o ->
      let rec loop () =
        match In_channel.input_byte i with
        | Some c -> Out_channel.output_byte o c; loop ()
        | None -> () in
      loop ())) 
    
let read_command_output f s =
  let ic = Unix.open_process_in s in
  (try
     while true do
       f (input_char ic)
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Ok () -> ()
  | Error _ -> invalid_arg ("read_command_output: " ^ s)
  (* type error = [ `Exit_non_zero of int | `Signal of Core.Signal.t ] *)

let slurp_command s =
  let buf = Buffer.create 100 in
  read_command_output (Buffer.add_char buf) s;
  Buffer.contents buf

let is_newer filea fileb =
  let stata = Unix.stat filea and statb = Unix.stat fileb in
  stata.Unix.st_mtime > statb.Unix.st_mtime
        
let run_command c =
  match Unix.system c with
  | Ok () -> ()
  | _ -> failwith (sprintf "Command exited with non-zero: %s" c)


module Feed = struct
      (* Warning: the following function crashes in OCaml 3.09.2,
         because of that bug: http://caml.inria.fr/mantis/view.php?id=4062
         (close_out is applied a second time during Unix.close_process) *)
  let kfeed f command data =
    let (ic, oc) as channels = Unix.open_process command in
    output_string oc data;
    Out_channel.close oc;
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
    | Ok () -> ()
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


module Date = struct
  let rfc_822 () = 

    let module U = Unix in
    let t = U.time () in
    let st = U.localtime t in
    let sec   = st.U.tm_sec in (* : int;*)
    let min   = st.U.tm_min in (* : int;*)
    let hour  = st.U.tm_hour in (* : int;*)
    let mday  = st.U.tm_mday in (* : int;*)
    let mon   = st.U.tm_mon in (* : int;*)
    let year  = st.U.tm_year in (* : int;*)
    let wday  = st.U.tm_wday in (* : int;*)
    let yday  = st.U.tm_yday in (* : int;
                                   let isdst = st.U.tm_isdst in (* : bool;*)
                                *)
    let diff = 
      if yday =(U.gmtime t).U.tm_yday then
        hour - (U.gmtime t).U.tm_hour 
      else
        if yday > (U.gmtime t).U.tm_yday then
          (24 + hour) - (U.gmtime t).U.tm_hour 
        else
          hour - (24 + (U.gmtime t).U.tm_hour )
    in
    let sdiff =
      if diff >= 0 then
        Printf.sprintf "+%02d00" diff
      else
        Printf.sprintf "-%02d00" (- diff)
    in
    let dday = 
      match wday with
      | 0 -> "Sun" 
      | 1 -> "Mon"
      | 2 -> "Tue"
      | 3 -> "Wed"
      | 4 -> "Thu"
      | 5 -> "Fri"
      | 6 -> "Sat"
      | _ -> "???"
    in
    let mmon = 
      match mon with
      | 0  -> "Jan"
      | 1  -> "Feb"
      | 2  -> "Mar"
      | 3  -> "Apr"
      | 4  -> "May"
      | 5  -> "Jun"
      | 6  -> "Jul"
      | 7  -> "Aug"
      | 8  -> "Sep"
      | 9  -> "Oct"
      | 10 -> "Nov"
      | 11 -> "Dec"
      | _ -> "???"
    in
    let s =
      Printf.sprintf
        "%s, %d %s %d %02d:%02d:%02d %s"
        dday mday mmon (1900 + year) hour min sec sdiff in
      (* yday  *)
      (* isdst *)
    s
      
end

