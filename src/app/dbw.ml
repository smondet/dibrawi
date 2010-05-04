

open Dibrawi_std

module Dbw_unix = struct
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
  let with_new_tmp ?(suffix=".tmp") ?(prefix="dbw_") f =
    let name, o = Filename.open_temp_file prefix suffix in
    try let r = f o name in close_out o; r with e -> close_out o; raise e

end

let output_buffers
    ~(templ_fun:string -> string) file_name content_buffer err_buffer =
  Dbw_unix.mkdir_p (Filename.dirname file_name);
  Dbw_unix.with_new_out file_name
    (fun o ->
       let content_content = templ_fun (Buffer.contents content_buffer) in
       fprintf o "%s" content_content;
       fprintf o "%!";);
  let s = Buffer.contents err_buffer in
  if s <$> "" then (eprintf "Errors for %s:\n%s\n" file_name s;);
  ()


open Dibrawi

let transform ?(html_template="") ?persistence_file data_root build =
  let the_source_tree = 
    Data_source.get_file_tree ~data_root () in
  let todo_list = Todo_list.empty () in

  let menu_factory =
    let source_menu =
      ((File_tree.File ("", "bibliography.brtx"))
       :: (File_tree.File ("", "address_book.brtx"))
       :: the_source_tree) in
    HTML_menu.make_menu_factory source_menu
  in
  let html_templ_fun = 
    if html_template <$> "" then 
      Templating.load_html (Data_source.get_file html_template)
    else
      Templating.html_default
  in
  let list_sebibs =
    File_tree.str_and_path_list ~filter:"\\.sebib$" the_source_tree in
  let () = 
    let bib =
      Bibliography.load (Ls.map list_sebibs 
                           ~f:(fun (str, path) ->
                                 Data_source.get_page (data_root ^ "/" ^ str)))
    in
    let menu = HTML_menu.get_menu ~from:["bibliography"] menu_factory in
    let brtx = Bibliography.to_brtx bib in
    let toc = Brtx_transform.html_toc ~filename:"Bibliography" brtx in
    let html = build ^ "/bibliography.html" in
    let from =  ["bibliography.html"] in
    let html_buffer, err_buffer = 
      Brtx_transform.to_html ~todo_list ~from brtx in
    output_buffers
      ~templ_fun:(html_templ_fun ~menu ~toc ~title:"Bibliography")
      html html_buffer err_buffer;
  in

  let list_abs =
    File_tree.str_and_path_list ~filter:"\\.abs$" the_source_tree in
  let () =
    let ab =
      Address_book.load
        (Ls.map list_abs ~f:(fun (str, path) ->
                               Data_source.get_page (data_root ^ "/" ^ str))) in
    let menu = HTML_menu.get_menu ~from:["address_book"] menu_factory in
    let brtx = Address_book.to_brtx ab in
    let toc = Brtx_transform.html_toc ~filename:"address_book" brtx in
    let html = build ^ "/address_book.html" in
    let from =  ["address_book.html"] in
    let html_buffer, err_buffer = 
      Brtx_transform.to_html ~todo_list ~from brtx in
    output_buffers
      ~templ_fun:(html_templ_fun ~menu ~toc ~title:"Address Book")
      html html_buffer err_buffer;
  in


  let list_brtxes = File_tree.str_and_path_list the_source_tree in

  let list_with_targets =
    let previous_content =
      match persistence_file with
      | None -> []
      | Some pf ->
          try 
            let i = open_in pf in
            let str_contents =
              (Marshal.from_channel i
                 : (string * Make.MD5.file_content * Make.MD5.file_content) list)
            in
            close_in i;
            str_contents 
          with _ -> [] (* when the file does not exist, or Marshal fails *)
    in
    let previous_file_content html str =
      match Ls.find_opt previous_content ~f:(fun (s, _, _) -> s =$= str) with
      | None -> None
      | Some (_, cs, ch) ->
          if html then Some ch else Some cs in

    let make_target_source (str, path) =
      let filename = data_root ^ "/" ^ str in
      let build_cmd () = printf "build_cmd: %s\n" filename in
      let initial_content = previous_file_content false str in
      Make.MD5.make_file_target ?initial_content ~filename ~build_cmd [] in
    
    let make_target_html (str, path) =
      let filename = build ^ "/" ^ (Filename.chop_extension str) ^ ".html" in
      let build_cmd () = 
        printf "build_cmd: %s\n" filename;
        let brtx = data_root ^ "/" ^ str in
        let title = Filename.chop_extension str in
        let from = path in
        let menu = HTML_menu.get_menu ~from menu_factory in
        let page = Data_source.get_page brtx in
        let toc = Brtx_transform.html_toc ~filename:str page in
        let html_buffer, err_buffer = 
          Brtx_transform.to_html ~todo_list ~filename:str ~from page in
        output_buffers ~templ_fun:(html_templ_fun ~menu ~toc ~title)
          filename html_buffer err_buffer; 
      in
      let target_source, content_source = make_target_source (str, path) in
      let initial_content = previous_file_content true str in
      let target_html, content_html =
        Make.MD5.make_file_target ?initial_content 
          ~filename ~build_cmd [target_source] in
      (str, path, target_source, content_source, target_html, content_html) in
    Ls.map list_brtxes ~f:make_target_html in

  let main_target, _ =
    let build_cmd () = printf "Making the main target\n" in
    Make.MD5.make_phony_target ~name:"Main" ~build_cmd 
      (Ls.map list_with_targets ~f:(fun (_,_, _,_, th,_) -> th)) in

  if Make.make main_target then (
    printf "End of build\n";
  ) else (
    printf "Nothing to be done.\n";
  );

  begin match persistence_file with
  | None -> ()
  | Some pf ->
      let str_contents =
        Ls.map list_with_targets ~f:(fun (s,_, _,cs, _,ch) -> (s, cs, ch)) in
      let o = open_out pf in
      Marshal.to_channel o str_contents [];
      close_out o;
  end;

  Todo_list.simplify todo_list;
  Todo_list.do_things todo_list
    ~f:(function 
        | `copy (path, from) ->
            let from_path = String.concat "/" (Ls.rev (Ls.tl from)) in
            let origin = data_root ^ "/" ^ from_path ^ "/" ^ path in
            let dest = build ^ "/" ^ from_path ^ "/" ^ path in
            Dbw_unix.mkdir_p (Filename.dirname dest);
            ignore (Unix.system (sprintf "cp %s %s" origin dest));
            (* printf "Should copy: %s -> %s\n" origin dest; *)
            []
       );
  if not (Todo_list.is_empty todo_list) then (
    printf "Still TODO: %s\n" (Todo_list.to_string todo_list)
  );
  ()


let () =
  let print_version = ref false in
  let html_tmpl = ref "" in
  let persistance = ref "" in

  let arg_cmd ~doc key spec = (key, spec, doc) in
  let usage = "usage: dbw [OPTIONS] <input-dir> <output-dir>" in
  let commands = [
    arg_cmd
      ~doc:"\n\tPrint version informations and exit"
      "-version"
      (Arg.Set print_version);
    arg_cmd
      ~doc:"<path>\n\tSet an HTML template file"
      "-html-template"
      (Arg.Set_string html_tmpl);
    arg_cmd
      ~doc:"<path>\n\tUse a file for build persistance."
      "-persist-with"
      (Arg.Set_string persistance);
  ] in 
  let anonymous_arguments =
    let anons = ref [] in
    let anon_fun s = anons := s :: !anons in
    Arg.parse commands anon_fun usage;
    Ls.rev !anons   in

  if !print_version then (
    printf "dbw v. 0 (%s)\n" Dibrawi.Info.version_string;
    printf
      "OCaml: %s, PCRE: %s, Bracetax: %s, SeBib: %s\n"
      Sys.ocaml_version  Pcre.version
      Bracetax.Info.version Sebib.Info.version;
  ) else (
    begin match anonymous_arguments with
    | [i; o] ->
        let persistence_file =
          if !persistance =$= "" then None else Some !persistance in
        transform ~html_template:!html_tmpl ?persistence_file i o 
    | _ -> 
        printf "Wrong number of arguments: %d\n" 
          (Ls.length anonymous_arguments);
        printf "%s\n" usage;
    end;
  );
  ()


