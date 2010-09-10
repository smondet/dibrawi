

module type CAMLMIX =
  sig
    val source : string ref
    val line : int ref
    val char : int ref  
    val printer : (string -> unit) ref 
    val print_with : (string -> unit) -> unit
    val print_if : bool -> unit
  end

module Make (Camlmix_input: CAMLMIX) = struct

  include Dibrawi_std

  let pr s = !Camlmix_input.printer s

  module Params: sig
    type output
    (* val global_output : output ref *)
    val set_output :
      [ `LaTeX | `PDF | `html | `latex | `pdf ] -> unit
    val dibrawi_output : unit -> [ `html | `pdf ]
    val map_output : html:(unit -> 'a) -> latex:(unit -> 'a) -> 'a
  end = struct
    type output =
      | Out_html
      | Out_latex

    let global_output = ref Out_html

    let set_output = function
      | `html -> global_output := Out_html
      | `latex | `LaTeX | `PDF | `pdf -> global_output := Out_latex
        
    let dibrawi_output () = 
      match !global_output with
      | Out_html -> `html
      | Out_latex -> `pdf

    let map_output ~html ~latex =
      match !global_output with
      | Out_html -> html ()
      | Out_latex -> latex ()

  end

  module Dbw: sig
    val prepro : ?mix_output:[ `camlmix | `wiki ] -> string -> string
    val brtx :
      ?do_prepro:bool ->
      ?separate_header:(string * string * string) ref ->
      string -> string

  end = struct

    let prepro =
      let module DP = Dibrawi.Preprocessor in
      let output = Params.dibrawi_output () in
      let todo_list = None in
      DP.brtx2brtx
        ?todo_list
        ~html_cite:(DP.default_html_cite "")
        ~output ~from:["CamlMixedDocument"]

    let brtx ?(do_prepro=false) ?separate_header str =
      let doc = false in
      let url_hook =
        Dibrawi.Special_paths.rewrite_url ~from:[ "SomeCamlMix" ] in
      let brtx = if do_prepro then prepro str else str in
      let res, errors =
        Params.map_output
          ~html:(fun () ->
            Bracetax.Transform.str_to_html ?separate_header ~doc ~url_hook brtx)
          ~latex:(fun () ->
            Bracetax.Transform.str_to_latex 
              ?separate_header ~doc ~url_hook brtx)
      in
      match errors with
      | [] -> res
      | l ->
        let f = function 
          | `message m -> Bracetax.Error.to_string m | `undefined s -> s in
        failwith ("brtx compilation errors: " ^ (Str.concat "\n" (Ls.map ~f l)))

  end


  module System: sig
    val slurp_command : string -> string
    val is_newer : string -> string -> bool
    val run_command : string -> unit
    val feed: cmd:string -> input:string -> string
  end = struct

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

  end


  module Alt = struct
    let start text =
      !Camlmix_input.printer text;
      Camlmix_input.print_with ignore
    let stop = ()
    let b = start
    let e = stop
  end


  module Sebibrtx = struct
    let url_pdf_doi = 
     "@{if (or (has url) (has pdfurl) (has doi))} \
      [@{if (has url)}{t|{link @{url}|URL}}\
      @{if (or (has pdfurl) (has doi))}, @{endif}@{endif}\
      @{if (has pdfurl)}{t|{link @{pdfurl}|PDF}}\
      @{if (has doi)}, @{endif}@{endif}\
      @{if (has doi)}{t|{link @{doi}|DOI}}@{endif}]@{endif}"

    let label output =
      Params.map_output
        ~html:(fun () ->
          "{bypass endbypass}<a id=\"@{id}\"></a>{endbypass}")
        ~latex:(fun () ->
          "{bypass endbypass}\\phantomsection\\label{@{id}}{endbypass}")

  end


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
          "%s, %d %s %d  %d:%02d:%02d  %s"
          dday mday mmon (1900 + year) hour min sec sdiff in
      (* yday  *)
      (* isdst *)
      s
        
  end



end


