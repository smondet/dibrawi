
open Dibrawi_std

module Make = Dibrawi_make

module System = Dibrawi_system

module Anyweb = Dibrawi_anyweb

module HTML = struct
  module Template = Dibrawi_html_template
end


module Info = struct
  let version = 0
  let version_string = sprintf "The Dibrawi library, v %d" version
end

module File_tree = struct

  type file_tree_item = 
    | Dir of string * string * file_tree
    | File of string * string
  and
    file_tree = file_tree_item list

      
  let fold_tree
      ?(dir=fun path name a -> a) ?(file=fun path name a -> ())
      init tree =
    let rec parse current = function
      | Dir (path, name, l) ->
        let next = dir path name current in
        List.iter ~f:(parse next) l
      | File (path, name) ->
        file path name current
    in
    List.iter ~f:(parse init) tree
      

  let print_tree =
    fold_tree
      ~dir:(fun path name indent -> 
        printf "%s%s\n" (String.make indent ' ') name;
        indent + 2)
      ~file:(fun path name indent ->
        printf "%s%s\n" (String.make indent ' ') name;)
      0

  (** Filter the file tree. *)
  let rec filter_tree tree ~f =
    let new_tree = ref [] in
    let filter = function
      | Dir (path, name, subdir) ->
        if f path name then
          new_tree := Dir (path, name, filter_tree ~f subdir) :: !new_tree
      | File (path, name) as file ->
        if f path name then
          new_tree := file :: !new_tree
    in
    List.iter ~f:filter tree;
    List.rev !new_tree

  (** Exclude files and directories matching [pattern]. *)
  let exclude_from_tree pattern tree =
    let rex = Pcre.regexp pattern in
    filter_tree tree ~f:(fun path name -> not (pcre_matches rex (path ^ name)))

  (** Keep only the files and directories matching [pattern]. *)
  let filter_tree_with_pattern pattern tree  =
    let rex = Pcre.regexp pattern in
    filter_tree tree ~f:(fun path name -> (pcre_matches rex (path ^ name)))

  let string_path_list
      ?(filter="\\.brtx$") ?(exclude=".*\\.svn.*") ?(url_prefix="") tree =
    let filt = Pcre.regexp filter in
    let excl = Pcre.regexp exclude in
    let paths = ref [] in
    fold_tree
      ~file:(fun p n () ->
        let full = p ^ "/" ^ n in
        if (pcre_matches filt full) && not (pcre_matches excl full)
        then (paths := (url_prefix ^ full) :: !paths);) () tree;
    (List.rev !paths)
      
      
  (* Render a path compatible with Future.Path in batteries *)
  let path_list 
      ?(filter="\\.brtx$") ?(exclude=".*\\.svn.*") ?(prefix=[]) tree =
    let filt = Pcre.regexp filter in
    let excl = Pcre.regexp exclude in
    let paths = ref [] in
    (* let current = ref prefix in *)
    fold_tree
      ~dir:(fun p n c ->
        Option.bind c (fun cc -> 
          if not (pcre_matches excl n) then Some (n :: cc) else None)) 
      ~file:(fun p n c ->
        Option.iter c (fun c ->
          if (pcre_matches filt n)
          then (paths := (n :: c) :: !paths);)
      )
      (Some prefix) tree;
    (List.rev !paths)
  
  let str_and_path_list 
      ?(filter="\\.brtx$") ?(exclude=".*\\.svn.*") ?(prefix=("", [])) tree =
    let filt = Pcre.regexp filter in
    let excl = Pcre.regexp exclude in
    let paths = ref [] in
    (* let current = ref prefix in *)
    fold_tree
      ~dir:(fun p n c ->
        Option.bind c (fun cc -> 
          if not (pcre_matches excl n)
          then Some (n :: cc) else None)) 
      ~file:(fun p n c ->
        Option.iter c (fun c ->
          if (pcre_matches filt n)
          then (
            let to_add = 
              ((fst prefix) ^ p ^ "/" ^ n, n :: c) in
            paths := to_add :: !paths;
          );))
      (Some (snd prefix)) tree;
    (List.rev !paths)
end

module Data_source = struct
    (* Future: 
        - is_valid_zipimg: path -> bool

    *)

  let get_file_tree ?(data_root="./data/") () =
    let module FT = File_tree in
    let ls dir =
      let sort a = Array.sort String.compare a; a in
      Sys.readdir dir |! sort |! Array.to_list in 
    if Sys.is_directory data_root = `Yes then (
      let rec explore path name =
        let next_path = path ^ "/" ^ name in
        let real_path = data_root ^ "/" ^ next_path in
        try 
          if Sys.is_directory real_path = `Yes then
            Some (FT.Dir (path, name, 
                          List.filter_map ~f:(explore next_path) (ls real_path)))
          else
            Some (FT.File (path, name))
        with
          Sys_error msg ->
            eprintf "Warning: Ignoring path %s\n" real_path;
            None
      in
      List.filter_map ~f:(explore ".")  (ls data_root)
    ) else (
      invalid_arg (sprintf "%s is not a directory" data_root)
    )
  
  let get_file path =
    In_channel.with_file path ~f:In_channel.input_all  

  let get_page path = get_file path

end


module Todo_list = struct

  type todo = [
  | `copy of (string * (string list))
  ]
  type t = todo list ref

  let empty () = ref []

  let is_empty t = !t =@= []

  let to_string ?(sep="; ") tl =
    let strpath = String.concat ~sep:"/" in
    String.concat ~sep (List.map !tl ~f:(function
    | `copy (path, from) ->
      sprintf "Copy File: %s from %s" path (strpath from)))
      
  let iter t ~f = List.iter !t ~f
    
  let do_things t ~(f:todo -> todo list) =
    t := List.concat (List.map !t ~f)

  let simplify t = t := List.dedup !t

end

module Special_paths = struct

  let parent_directories_path from =
    let depth = List.length from - 1 in
    if depth > 0 then
      (String.concat ~sep:"/" (List.init depth (fun _ -> "..")))
    else
      "."

  let relativize from path =
    try
      match path.[0] with
      | '/' -> (parent_directories_path from) ^ path
      | '#' -> (Filename.chop_extension (List.hd_exn from)) ^ path
      | _ -> path
    with _ -> (Filename.chop_extension (List.hd_exn from)) (* the string is empty*)
      
  let typify url extension =
    match String.rindex url '#', String.rindex url '/' with
    | Some h , None ->
      let head, tail = String.(prefix url h, suffix url (length url - h)) in
      head ^ extension ^ tail
    | Some h, Some l when h > l ->
      let head, tail = String.(prefix url h, suffix url (length url - h)) in
      head ^ extension ^ tail
    | _, _ ->
      url ^ extension
        
  let compute_path from url extension =
    typify (relativize from url) extension

  let rec rewrite_url ?todo_list ?(output=`html)  ~from url =
    match url with
    | s when String.is_prefix s "pdf:" ->
      let pdfpath = 
        compute_path from (String.drop_prefix s 4) ".pdf" in
      pdfpath 
    | s when String.is_prefix s "fig:" ->
      let path =
        compute_path from (String.drop_prefix s 4)
          (match output with `html -> ".png" | `pdf -> ".pdf") in
      Option.iter todo_list ~f:(fun rl -> rl := (`copy (path, from)) :: !rl;);
      path
    | s when String.is_prefix s "page:" ->
      (compute_path from (String.drop_prefix s 5) ".html")
    | s when String.is_prefix s "media:" ->
      let path = compute_path from (String.drop_prefix s 6) "" in
      Option.iter todo_list ~f:(fun rl -> rl := (`copy (path, from)) :: !rl;);
      path
    | s -> s
      
end

module Light_syntax = struct

let is_whitespace s =
  String.fold ~f:(fun already -> function
  | ' ' | '\n' | '\t' | '\r' -> already
  | c -> false ) ~init:true s
let indentation s =
  String.fold s ~f:(fun (continue, result) -> function
  | ' ' | '\n' | '\t' | '\r' ->
    if continue then (true, result + 1) else (false, result)
  | c -> (false, result)) ~init:(true, 0)

let todo_item s =
  let indentation = snd (indentation s) / 2 + 1 in
  let without = String.strip s in
  let try_prefix without prefix tag () =
    if String.is_prefix without prefix
    then Some (tag, indentation, String.chop_prefix_exn without ~prefix)
    else None in
  let (==<) x f = if x = None then f () else x in
  try_prefix without "*" `important ()
  ==< try_prefix without "[]"   `to_do
  ==< try_prefix without "[x]"  `to_do_failed
  ==< try_prefix without "[v]"  `to_do_done
  ==< try_prefix without "()"   `to_delegate
  ==< try_prefix without "?"    `to_research
  ==< try_prefix without "-"    `simple_item
    
let to_brtx s =
  let lines = String.split s ~on:'\n' in
  let result = Buffer.create 42 in
  let transform (paragraph_to_end, indentation) = function
    | s when is_whitespace s ->
      if paragraph_to_end then begin
        for i = 1 to indentation do
          Buffer.add_string result "{end}\n";
        done;
        Buffer.add_string result "{p}\n";
      end;
      Buffer.add_string result "\n";
      (false, 0)
    | s ->
      let next_indent =
        match todo_item s with
        | None -> Buffer.add_string result (s ^ "\n"); indentation
        | Some (tag, line_indentation, line) ->
          for i = indentation - 1 downto line_indentation do
            Buffer.add_string result "{end}\n";
          done;
          for i = indentation + 1 to line_indentation do
            Buffer.add_string result "{begin list}\n"
          done;
          let string_tag =
            match tag with
            | `important -> "⚑"
            | `to_do -> "☐"
            | `to_do_failed -> "☒"
            | `to_do_done -> "☑"
            | `to_delegate -> "☛"
            | `to_research -> "�"
            | `simple_item -> "•"
          in
          Buffer.add_string result (sprintf  "{*} %s %s \n" string_tag line);
          line_indentation
      in
      (true, next_indent)
  in
  Pervasives.ignore (List.fold lines ~f:transform ~init:(false, 0));
  Buffer.contents result
end
    
module Preprocessor = struct

  let default_html_biblio_page = "page:/bibliography"

  let default_html_cite =
    fun html_biblio_page cites ->
      "[" ^ (String.concat ~sep:", "
               (List.map cites ~f:(fun cite ->
                 sprintf "{link %s#%s|%s}" html_biblio_page cite cite))) ^ "]"

  let sanitize_brtx_command =
    let f =
      String.Escaping.escape ~escapeworthy:['\\'; ' '; '|'; '{'; '}']
        ~escape_char:'\\' |! Core.Staged.unstage in
    f
      
    (* String.replace_chars (function *)
    (*   | '\\' -> "\\\\"  *)
    (*   | ' ' -> "\\ " *)
    (*   | '|' -> "\\|"  *)
    (*   | '{' -> "\\{" *)
    (*   | '}' -> "\\}" *)
    (*   | c -> Str.of_char c) s *)

  let sanitize_brtx_content s =
    let b = Buffer.create 42 in
    String.iter s (function
    | '#' -> Buffer.add_string b "{#}" 
    | '{' -> Buffer.add_string b "{{}"
    | '}' -> Buffer.add_string b "{}}"
    | c -> Buffer.add_char b c);
    Buffer.contents b

  let rec make
      ?(html_cite=default_html_cite default_html_biblio_page)
      ?(output=`html) ?(mix_output=`wiki) () =
    let buf = Buffer.create 42 in
    let pr = Buffer.add_string buf in
    let brtx_ploc l str =
      match String.lsplit2 str ~on:' ' with
      | Some (before, after) ->
        sprintf "%s\n#line %d %S\n%s" before l.Bracetax.Error.l_line
          l.Bracetax.Error.l_file after
      | None -> str
    in
    let caml_ploc l str =
      match String.lsplit2 str ~on:' ' with
      | Some (before, after) ->
        sprintf "%s\n# %d %S ;;\n%s" before l.Bracetax.Error.l_line
          l.Bracetax.Error.l_file after
      | None -> str
    in
    let default_raw_end = Bracetax.Commands.Raw.default_raw_end () in
    let is_old_pp_raw_2 s = List.exists ~f:((=) s) ["mc"; "mi"; "me"] in
    let is_old_pp_raw_n s =
      List.exists ~f:((=) s) ["mix:code"; "mix:ignore"; "mix:end"; "light"] in
    let is_old_pp_raw s = is_old_pp_raw_2 s || is_old_pp_raw_n s in
    let cmd_stack = Stack.create () in
    let pop_loc stack loc =
      try Stack.pop stack with Stack.Empty ->
        eprintf "[[%s:%d]] Trying to close a non-existing command.\n"
          loc.Bracetax.Error.l_file loc.Bracetax.Error.l_line;
        failwith "Preprocessor Error" in
    let bypass s = 
      let endtag = "dibrawipreprocessorendtag" in
      (sprintf "{bypass %s}%s{%s}" endtag s endtag) in
    let html_or_latex h l = match output with `html -> h | `pdf -> l in
    let clean_cite s =
      let b = Buffer.create 42 in
      String.iter s (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
      | ':' | '.' | '-' | '_' as ok -> Buffer.add_char b ok
      | _ -> ());
      Buffer.contents b
    in
    let split_cites s = String.split s ~on:',' in
    let light_syntax_buffer = ref None in
    let brtx_printer = 
      { Bracetax.Signatures.
        print_comment = (fun _ _ -> ());
        print_text = (fun loc s -> pr (brtx_ploc loc s));
        enter_cmd = (fun loc cmd args ->
          Stack.push cmd_stack (cmd, args);
          match cmd with
          | "cmt" ->
            pr (bypass 
                  (html_or_latex
                     "<span class=\"dibrawicomment\">" "\\dbwcmt{"));
            pr (String.concat ~sep:" " (List.map ~f:sanitize_brtx_content args))
          | "cite" | "=" | "@" | "mix:include" -> ()
          | _ ->
            pr (sprintf "{%s%s|" cmd
                  (String.concat ~sep:"" (List.map ~f:(fun s -> 
                    sprintf " %s" (sanitize_brtx_command s)) args))));
        leave_cmd = (fun loc ->
          match Option.value_exn (pop_loc cmd_stack loc)  with
          | ("cmt", _) -> pr (bypass (html_or_latex "</span>" "}"))
          | ("cite", args) ->
            let cites =
              List.map ~f:clean_cite
                (List.concat (List.map args ~f:split_cites)) in
            pr (html_or_latex (html_cite cites)
                  (bypass (sprintf "\\cite{%s}" (String.concat ~sep:"," cites))))
          | ("mix:include", args) ->
            begin match mix_output with
            | `wiki ->
              pr "{bypass endfordiv}<span class=\"dbwmixcode\">{endfordiv}";
              pr (sprintf "{t|{utf 0x22b2}mix:include {text mixspecialend}");
              pr (String.concat ~sep:" " args);
              pr "{mixspecialend}{utf 0x22b3}}";
              pr "{bypass endfordiv}</span>{endfordiv}";
            | `camlmix ->
              let filename = 
                try List.hd_exn args with
                  e ->
                    eprintf "[[%s:%d]] {mix:include} has no argument.\n"
                      loc.Bracetax.Error.l_file loc.Bracetax.Error.l_line;
                    failwith "Preprocessor Error" in
              let contents = 
                Data_source.get_file filename in
              pr (sprintf "# Inserting %s\n" filename);
              let future_prepro = make ~html_cite ~output ~mix_output () in
              pr (future_prepro ~filename contents);
            end
          | (cmd, args) when cmd = "=" || cmd = "@" ->
            begin match mix_output with
            | `wiki ->
              pr "{bypass endfordiv}<span class=\"dbwmixcode\">{endfordiv}";
              pr (sprintf "{t|{utf 0x22b2}%s {text mixspecialend}" cmd);
              pr (String.concat ~sep:" " args);
              pr "{mixspecialend}{utf 0x22b3}}";
              pr "{bypass endfordiv}</span>{endfordiv}";
            | `camlmix -> 
              pr "##";
              pr (if cmd = "=" then "= " else " ");
              pr (String.concat ~sep:" " args);
              pr " ##";
            end
          | _ -> pr "}");
        terminate = (fun loc -> ());
        is_raw = (fun s -> 
          (Bracetax.Commands.Raw.is_raw_cmd s) || (is_old_pp_raw s));
        default_raw_end = (function
          | s when Bracetax.Commands.Raw.is_raw_cmd s -> "end"
          | s when is_old_pp_raw_2 s -> "me"
          | "light" -> "end"
          | s -> "mix:end");
        enter_raw = (fun loc cmd args -> 
          Stack.push cmd_stack (cmd, args);
          match cmd with
          | "mix:ignore" | "mi" ->
            begin match mix_output with
            | `wiki -> pr "{ignore mixspecialend}"
            | `camlmix -> pr (caml_ploc loc "## ")
            end
          | "mix:code" | "mc" ->
            begin match mix_output with
            | `wiki ->
              pr "{bypass endfordiv}<div class=\"dbwmixcode\">{endfordiv}\
                  {code mixspecialend}"
            | `camlmix -> pr (caml_ploc loc "## ")
            end
          | "light" ->
            light_syntax_buffer := Some (Buffer.create 42);
          | s ->
            pr (sprintf "{%s%s}" cmd
                  (String.concat ~sep:"" (List.map ~f:(fun s -> 
                    sprintf " %s" (sanitize_brtx_command s)) args))));
        print_raw = (fun loc line ->
          begin match !light_syntax_buffer with
          | None -> 
            if mix_output = `camlmix
            then pr (More_string.replace_all line ~sub:"##" ~by:"###")
            else pr line
          | Some buf -> Buffer.add_string buf line
          end);
        leave_raw = (fun loc -> 
          match Option.value_exn (pop_loc cmd_stack loc) with
          | ("mix:ignore", _) | ("mi", _) ->
            begin match mix_output with
            | `wiki -> pr "{mixspecialend}"
            | `camlmix -> pr "##"
            end
          | ("mix:code", _) | ("mc", _) ->
            begin match mix_output with
            | `wiki -> 
              pr "{mixspecialend}";
              pr "{bypass endfordiv}</div>{endfordiv}";
            | `camlmix -> pr "##"
            end
          | ("light", _) ->
            begin match output with
            | `html ->
              pr "{bypass}\
                 <style>ul {list-style-type: inherit}</style>\
                 <div style=\"list-style-type: none;\"><div>{end}{p}"
            | `pdf -> 
              pr "{bypass} {
                  \ \\renewcommand{\\labelitemi}{}
                  \ \\renewcommand{\\labelitemii}{}
                  \ \\renewcommand{\\labelitemiii}{}
                  \ \\renewcommand{\\labelitemiv}{}
                  \ {end}"
            end;
            Option.iter !light_syntax_buffer ~f:(fun buf ->
              pr (Light_syntax.to_brtx (Buffer.contents buf));
            );
            light_syntax_buffer := None;
            begin match output with
            | `html ->
              pr "{bypass}</div></div>{end}"
            | `pdf -> pr "{bypass}}{end}\n"
            end
          | (s, []) -> pr (sprintf "{%s}" default_raw_end)
          | (s, endtag :: _) -> pr (sprintf "{%s}" endtag));
        error = (function
          | `undefined s -> eprintf "%s\n" s;
          | `message ((_, gravity, _) as msg) -> 
            eprintf "%s\n" (Bracetax.Error.to_string msg));} in
    let do_prepro ~filename str =
      let read_char_opt =
        let cpt = ref (-1) in
        (fun () -> try Some (incr cpt; str.[!cpt]) with e -> None) in
      Stack.clear cmd_stack;
      Buffer.clear buf; (* `clear' keeps the internal string, `reset'
                           deallocates it. *)
      Bracetax.Parser.do_transformation ~deny_bypass:false brtx_printer 
        read_char_opt filename;
      Buffer.contents buf in
    do_prepro

        

  let brtx2brtx ?todo_list 
      ?(html_cite=default_html_cite default_html_biblio_page)
      ?(output=`html) ?(mix_output=`wiki) ?from brtx = 
    let future = make ~html_cite ~output ~mix_output () in
    let brtx =
      future (String.concat ~sep:"/"
                (List.rev (Option.value ~default:["?NoFile?"] from))) brtx in
    brtx

      
end

(******************************************************************************)
module Bibliography = struct

  (* str_list is a list S-Expressions (already loaded in memory) *)
  let load str_list =
    let cmp = Sebib.Biblio.compare_by_field `id in
    Sebib.Biblio.sort (Sebib.Biblio.unique ~cmp
                         (Sebib.Parsing.parse (String.concat ~sep:" " str_list)))
  
  let to_brtx biblio =
    let pattern = "
            {section 1 @{id}|{t|@{id}}}\
            {cite @{id}}{br}\
            {b|@{title}}{br} \
            @{if (has authors)}@{authors-and}\
                @{else}{i|-- no authors --}@{endif}{br}\
            @{year} - {i|@{how}}{br} \
            @{if (or (has url) (has pdfurl) (has doi))} {b|Links:} \
            @{if (has url)}{t|{link @{url}|URL}}@{endif} \
            @{if (has pdfurl)}{t|{link @{pdfurl}|PDF}}@{endif} \
            @{if (has doi)}{t|{link @{doi}|doi}}@{endif}{br}@{endif} \
            {b|Tags:} {i|@{tags}} {br} \
            @{if (has keywords)}{b|Keywords:} {i|@{keywords}} {br}@{endif} \
            @{if (has abstract)}{b|Abstract:} {br} @{abstract} {br}@{endif} \
            @{if (has comment-short)}{b|Description:}  \
                 @{comment-short}{br}@{endif}\
            @{if (has comment-main)}{b|Comments:} {br} @{comment}@{endif}" in
    "{header|{title|Bibliography}}" ^ (Sebib.Format.str ~pattern biblio)

  let bibtex = Sebib.BibTeX.str

end


module Brtx_transform = struct

    (* TODO handle errors better *)
  let to_html ?todo_list ?class_hook ?filename ~from brtx =
    let html_buffer = Buffer.create 1024 in
    let err_buffer = Buffer.create 512 in
    let writer, input_char =
      Bracetax.Transform.string_io brtx html_buffer err_buffer in
    let url_hook =
      Special_paths.rewrite_url ?todo_list ~from in
    Bracetax.Transform.brtx_to_html
      ~writer ?filename ?class_hook ~make_section_links:`always
      ~img_hook:url_hook ~url_hook ~input_char ();
    (html_buffer, err_buffer)

  let html_toc ?filename brtx =
    let brtx_buffer = Buffer.create 1024 in
    let err_buffer = Buffer.create 512 in
    let writer, input_char =
      Bracetax.Transform.string_io brtx brtx_buffer err_buffer in
    Bracetax.Transform.get_TOC
      ~make_links:`always ~writer ~input_char ?filename ();
    let h, e =
      to_html ~class_hook:"dbwtoc" ~from:[""]
        (Buffer.contents brtx_buffer) in
    (Buffer.contents h)

end


module HTML_menu = struct

  open Data_source
  open File_tree

  let brtx_menu
      ?(url_prefix="")
      ?(filter="\\.brtx$") ?(exclude_dir=".*\\.svn.*")
      ?(chop_filter=true) ?(replace=".html") tree =
    let buf = Buffer.create 1024 in
    let buffer = Buffer.add_string buf in
    let filt = Pcre.regexp filter in
    let excl = Pcre.regexp exclude_dir in
    let file_of_name l name =
      let p =
        List.partition_tf l ~f:(function
        | File (_, n) when n = name ^ ".brtx" -> true
        | _ -> false) in
      match p with
      | ([ File (p, n) ], filtered) ->
        Some (File (p, n), filtered)
      | _ -> None
    in
    let presort l =
      let dirs, files =
        List.partition_tf ~f:(function Dir _ -> false | _ -> true) l in
      dirs @ files in
    let rec to_brtx ?(with_item=true) = function
      | Dir (path, name, l) ->
        if not (pcre_matches excl name) then (
          begin match file_of_name l name with
          | Some (f, l) ->
            ksprintf buffer "{*} {bypass}<div class=\"dibrawimenudir\">\
                                  <span class=\"dibrawimenudirname\">{end}";
            to_brtx ~with_item:false f;
            ksprintf buffer "{bypass}</span>{end}{begin list}";
            List.iter ~f:to_brtx (presort l);
          | None ->
            ksprintf buffer "{*} {bypass}<div class=\"dibrawimenudir\">\
                            <span class=\"dibrawimenudirname\">{end}\
                             %s\n{bypass}</span>{end}{begin list}\n" name;
            List.iter ~f:to_brtx (presort l);
          end;
          ksprintf buffer "{end}{bypass}</div>{end} # %s\n" name;
        ) else (
          ksprintf buffer "# ignore: %s %s\n" path name;
        );
      | File (path, name) ->
        (* eprintf "?? path: %s, name: %s\n" path name; *)
        if pcre_matches filt name then (
          let rex = filt in
          let link =
            path ^ "/" ^ (Pcre.replace ~rex ~templ:replace name) in
          let official_name =
            if chop_filter
            then Pcre.replace ~rex ~templ:"" name
            else name in
          ksprintf buffer "%s{link %s%s|%s}\n"
            (if with_item then "{*} " else "") url_prefix link official_name;
        );
    in
    Buffer.add_string buf (sprintf "{begin list}\n");
    List.iter ~f:to_brtx (presort tree);
    Buffer.add_string buf (sprintf "{end} # Root\n");
    (Buffer.contents buf)

  let html_menu 
      ?(url_prefix="page:/")
      ?(filter="\\.brtx$") ?(exclude_dir=".*\\.svn.*")
      ?(chop_filter=true) ?(replace="") ~from tree =
    let brtx =
      brtx_menu ~url_prefix ~filter ~exclude_dir ~replace ~chop_filter tree in
    let buf, err =
      Brtx_transform.to_html (* Should not need preprocessing. *)
        ~filename:"BRTX MENU" ~class_hook:"dibrawi_menu" ~from brtx in
    if (Buffer.contents err <$> "") then (
      eprintf "Errors in the bracetax: \n%s\n------------%s\n"
        brtx (Buffer.contents err);
      failwith "brtx ended with errors";
    );
    (Buffer.contents buf)


  type menu_factory = {
    mutable cache: string Int.Map.t;
    source: File_tree.file_tree;
  }
  let make_menu_factory source_menu =
    {cache = Int.Map.empty; source = source_menu; }

  let get_menu factory ~from =
    let depth = List.length from - 1 in
    match Map.find factory.cache depth with
    | Some s -> s
    | None ->
      let new_one = html_menu ~from factory.source in
      factory.cache <- Map.add factory.cache depth new_one;
      new_one
end

module Address_book = struct
  (* When grown up, this Adbose module is expected to become a 
   * standalone library 
   * *)
  
  module Adbose = struct
    module Sx = Sexplib.Sexp
      
    type field = string list
    type kind = [ `person | `group | `organisation ]
    type entry = kind * string * (field list)
        
    type address_book = entry list
    exception Parse_error of string
      
    let address_book_of_string str = 
      let fail msg =
        raise (Parse_error (sprintf "Address Book Syntax Error: %s" msg)) in
      let parse_field =
        function 
        | Sx.Atom s -> fail (sprintf "Unexpected atom: %s" s)
        | Sx.List l as sx->
          List.map l ~f:(function Sx.Atom s -> s
          | _ -> 
            fail (sprintf "Expecting list of atoms: %s" (Sx.to_string sx))) in
      let kind_of_string =
        function
        | "person" -> `person
        | "group" -> `group
        | "organisation" -> `organisation
        | s -> fail (sprintf "Unknown kind of entry: %s" s) in
      let parse_entry =
        function
        | (Sx.Atom k) :: (Sx.Atom id) :: fields ->
          (kind_of_string k, id, List.map ~f:parse_field fields)
        | sx -> 
          fail (sprintf "Can't understand: %s" (Sx.to_string (Sx.List sx))) in
      let sexp = 
        try Sx.of_string (sprintf "(%s)" str) 
        with Failure msg ->
          raise (Parse_error (sprintf "Request Syntax Error (sexplib): %s" msg))
      in
      let fail_atom s = fail (sprintf "Unexpected atom: %s" s) in
      match sexp with
      | Sx.Atom s -> fail_atom s
      | Sx.List l ->
        List.map l ~f:(function
        | Sx.Atom s -> fail_atom s
        | Sx.List l -> parse_entry l)

    let get_one field_name (k, i, ff) =
      List.find ff ~f:(function
      | fn :: _ when fn =$= field_name -> true
      | _ -> false)
    let get_all field_name (k, i, f) =
      List.filter f ~f:(function
      | f :: _ when f =$= field_name -> true
      | _ -> false)
        
    let sort_by_family ab = 
      List.sort ab
        ~cmp:(fun e1 e2 ->
          match (get_one "name" e1), (get_one "name" e2) with
          | Some [_; _; l1], Some [_; _; l2] -> String.compare l1 l2
          | Some [_; l1], Some [_; l2] -> String.compare l1 l2
          | Some [_; _; l1], Some [_; l2] -> String.compare l1 l2
          | Some [_; l1], Some [_; _; l2] -> String.compare l1 l2
          | _, Some [_; _; l2] -> -1
          | _, Some [_; l2] -> -1
          | Some [_; _; l1], _ -> 1
          | Some [_; l1], _ -> 1
          | _, _ -> 0)

  end

  let load str_list =
    Adbose.address_book_of_string (String.concat ~sep:" " str_list)

  let to_brtx abook =
    let get_needed ((kind, id, fields) as entry) = 
      let kind_str = 
        match kind with
        | `person -> "Person"
        | `group -> "Group"
        | `organisation -> "Organisation" in
      let name_str =
        match Adbose.get_one "name" entry with
        | Some (_ :: f :: l :: _) -> sprintf "%s, %s" l f
        | Some (_ :: f :: []) -> f
        | _ -> "___NO__VALID_NAME___" in
      (kind_str, id, name_str)
    in
    let header =
      "{header|{title|Address Book}}\n\n" in
    let sections =
      let convert_std = function
        | [_; n] -> (sprintf "{i|%s}" n)
        | [_; t; n] -> (sprintf "{b|[%s]} {i|%s}" t n)
        | _ -> "___NOT_A_VALID_STD_FIELD___" in
      let convert_link = function
        | [_; t] -> (sprintf "{link %s}" t)
        | [_; t; n] -> (sprintf "{link %s|%s}" t n)
        | [_; t; n; c] -> (sprintf "{link %s|%s} ({i|%s})" t n c)
        | _ -> "___NOT_A_VALID_LINK_FIELD___" in
      let if_something m f = match m with [] -> "" | l -> f l in
      let get_if_one_or_more fild etri ~one ~more =
        match Adbose.get_all fild etri with
          [] -> "" | [x] -> one x | l -> more l
      in
      let make_list ?(plural=fun x -> x ^ "s")
          field_name convert_entry entry_name entry =
        get_if_one_or_more field_name entry
          ~one:(fun x ->
            sprintf "\n{b|%s:} %s{p}" entry_name (convert_entry x))
          ~more:(fun l -> 
            let f = convert_entry in
            (sprintf "\n{b|%s:}{list|\n{*} %s\n}{p}\n" (plural entry_name)
               (String.concat ~sep:"\n{*}" (List.map l ~f))))
      in

      List.map (Adbose.sort_by_family abook)
        ~f:(fun entry ->
          let kstr, id, name = get_needed entry in
          let birthday =
            if_something (Adbose.get_all "birthday" entry)
              (fun l ->
                (sprintf "{b|Birthday}: %s{br}\n"
                   (String.concat ~sep:", " (List.tl_exn (List.hd_exn l))))) in
          let phones =
            make_list "phone" convert_std "Phone number" entry in
          let addresses =
            let plural = fun x -> x ^ "es" in
            make_list ~plural "address" convert_std "Address" entry in
          let emails =
            make_list "email" convert_std "E-Mail" entry in
          let links =
            make_list "link" convert_link "Link" entry in
          let tags =
            if_something (Adbose.get_all "tags" entry)
              (fun l ->
                (sprintf "{b|Tags}: %s{br}\n"
                   (String.concat ~sep:", " (List.tl_exn (List.hd_exn l))))) in
          let comments =
            if_something (Adbose.get_all "comments" entry) 
              (fun l ->
                (sprintf "{b|Comments}:{br}\n%s{br}\n"
                   (String.concat ~sep:"{br}\n" (List.tl_exn (List.hd_exn l))))) in
          
          (sprintf "{section 1 %s|%s (%s)}%s%s%s%s%s%s%s"
             id name kstr birthday phones addresses emails links
             tags comments)) in
    (header ^ (String.concat ~sep:"\n\n" sections))
      
      
end


module Persistence = struct

  type build_persistence = {
    mutable file_hashes: (string * Make.MD5.file_content) list;
    menu_hash: string;
  }

  let empty ?(menu_hash="") () = { file_hashes = []; menu_hash = menu_hash }

  let from_file pf =
    try 
      let i = open_in pf in
      let str_contents = (Marshal.from_channel i : build_persistence) in
      In_channel.close i;
      Some str_contents 
    with _ -> None (* when the file does not exist, or Marshal fails *)

  let find_file_opt bp str =
    match List.find bp.file_hashes ~f:(fun (s, _) -> s =$= str) with
    | None -> None
    | Some (_, c) -> Some c

  let save (str_contents: build_persistence) pf =
    let o = open_out pf in
    Marshal.to_channel o str_contents [];
    Out_channel.close o;
    ()

  let menu_hash bp = bp.menu_hash

  let filter_files ~f bp = List.filter ~f bp.file_hashes

  let set_files bp files = bp.file_hashes <- files

end


  
