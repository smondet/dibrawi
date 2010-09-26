

open Dibrawi_std

module Dbw_sys = Dibrawi.System

module Persistence = struct

  type build_persistence = {
    mutable file_hashes: (string * Dibrawi.Make.MD5.file_content) list;
    menu_hash: string;
  }

  let empty ?(menu_hash="") () = { file_hashes = []; menu_hash = menu_hash }

  let from_file pf =
    try 
      let i = open_in pf in
      let str_contents = (Marshal.from_channel i : build_persistence) in
      close_in i;
      Some str_contents 
    with _ -> None (* when the file does not exist, or Marshal fails *)

  let find_file_opt bp str =
    match Ls.find_opt bp.file_hashes ~f:(fun (s, _) -> s =$= str) with
    | None -> None
    | Some (_, c) -> Some c

  let save (str_contents: build_persistence) pf =
    let o = open_out pf in
    Marshal.to_channel o str_contents [];
    close_out o;
    ()

  let menu_hash bp = bp.menu_hash

  let filter_files ~f bp = Ls.filter ~f bp.file_hashes

  let set_files bp files = bp.file_hashes <- files

end


let output_buffers
    ~(output_content:'a Io.output -> unit) file_name err_buffer =
  Dbw_sys.mkdir_p (Filename.dirname file_name);
  Io.with_file_out file_name
    (fun o ->
      output_content o;
      Io.printf o "%!";);
  let s = Buffer.contents err_buffer in
  if s <$> "" then (eprintf "Errors for %s:\n%s\n" file_name s;false)
  else true


open Dibrawi

let transform ?(html_template=`none) ?persistence_file data_root build =

  let the_source_tree = Data_source.get_file_tree ~data_root () in
  let todo_list = Todo_list.empty () in


  let menu_factory =
    let source_menu =
      ((File_tree.File ("", "bibliography.brtx"))
       :: (File_tree.File ("", "address_book.brtx"))
       :: the_source_tree) in
    HTML_menu.make_menu_factory source_menu
  in



  let previous_content =
    match Opt.bind Persistence.from_file persistence_file with
    | None -> Persistence.empty ()
    | Some pc ->
      let menu = HTML_menu.get_menu ~from:["level0"] menu_factory in
      let current_menu_hash = Digest.string menu in
      let previous_menu_hash = Persistence.menu_hash pc in
      if previous_menu_hash =$= current_menu_hash then
        pc
      else (
        printf "The File Tree has changed, everything must be rebuilt\n";
        Persistence.empty ~menu_hash:current_menu_hash ()
      )
 in
  
  let previous_file_content str = 
    Persistence.find_file_opt previous_content str in
  
  let make_target_source ?(prefix="") str =
    let filename = data_root ^ "/" ^ str in
    let build_cmd () =
      printf "Source '%s%s' has changed [%s]\n" prefix str filename;
      Some filename in
    let initial_content = previous_file_content (prefix ^ str) in
    Make.MD5.make_file_target ?initial_content ~filename ~build_cmd [] in
  




  let html_templ_fun =
    let module Templating = Dibrawi.HTML.Template in
    match html_template with
    | `file f -> Templating.File.load_html (Data_source.get_file f)
    | `named "three_columns_greenish" ->
      Dibrawi.HTML.Template.Full.three_columns_greenish ()
    | `named "three_columns_greenish-linked" ->
      Dibrawi.HTML.Template.Full.three_columns_greenish ()
        ~top_right:(fun from ->
          let buf, _ =
            Brtx_transform.to_html 
              ~from:(Opt.default ["NONE"] from) 
              "{link page:/Main|Main}{~}{link #|Top}{~}{link #pagefoot}{br}\n\
              {link http://bracetax.berlios.de/bracetax_syntax.html|Bracetax Syntax}"
          in
          (Buffer.contents buf))
    | `named s ->
      failwith (sprintf "Template \"%s\" not found." s)
    | `none -> Templating.html_default
  in

  let str_trg_ctt_biblio, str_trg_ctt_sebib_list = 
    let list_sebibs =
      File_tree.str_and_path_list ~filter:"\\.sebib$" the_source_tree in
    let sebib_targets_and_contents =
      (Ls.map list_sebibs 
         ~f:(fun (str, path) ->
               let target_source, content_source = make_target_source str in
               (str, target_source, content_source))) in
    let html = build ^ "/bibliography.html" in
    let build_cmd () =
      printf "Build %s\n" html;
      let bib =
        Bibliography.load
          (Ls.map list_sebibs 
             ~f:(fun (str, path) ->
                   Data_source.get_page (data_root ^ "/" ^ str)))
      in
      let menu = HTML_menu.get_menu ~from:["bibliography"] menu_factory in
      let brtx = Bibliography.to_brtx bib in
      let toc = Brtx_transform.html_toc ~filename:"Bibliography" brtx in
      let from =  ["bibliography.html"] in
      let html_buffer, err_buffer = 
        Brtx_transform.to_html ~todo_list ~from brtx in
      let is_ok =
        output_buffers
          ~output_content:(fun out ->
            let whole = 
              html_templ_fun ~from ~menu ~toc ~title:"Bibliography" 
                (Buffer.contents html_buffer) in
            String_tree.print ~out whole;)
          html err_buffer in
      if not is_ok then None else (Some html)
    in
    let initial_content = previous_file_content html in
    let target_html, content_html =
      let deps =
        Ls.map sebib_targets_and_contents ~f:(fun (_, t, _) -> t) in
      Make.MD5.make_file_target ?initial_content 
        ~filename:html ~build_cmd deps in
    (html, target_html, content_html), sebib_targets_and_contents in 
  
  let str_trg_ctt_addbook, str_trg_ctt_addbook_list = 
    let list_abs =
      File_tree.str_and_path_list ~filter:"\\.abs$" the_source_tree in
    let abs_targets_and_contents =
      (Ls.map list_abs
         ~f:(fun (str, path) ->
               let target_source, content_source = make_target_source str in
               (str, target_source, content_source))) in
    let html = build ^ "/address_book.html" in
    let build_cmd () =
      printf "Build %s\n" html;
      let ab =
        Address_book.load
          (Ls.map list_abs ~f:(fun (str, path) ->
                                 Data_source.get_page (data_root ^ "/" ^ str)))
      in
      let menu = HTML_menu.get_menu ~from:["address_book"] menu_factory in
      let brtx = Address_book.to_brtx ab in
      let toc = Brtx_transform.html_toc ~filename:"address_book" brtx in
      let from =  ["address_book.html"] in
      let html_buffer, err_buffer = 
        Brtx_transform.to_html ~todo_list ~from brtx in
      let is_ok =
        output_buffers
          ~output_content:(fun out ->
            let whole = 
              html_templ_fun ~from ~menu ~toc ~title:"Address Book"
                (Buffer.contents html_buffer) in
            String_tree.print ~out whole;)
          html err_buffer in
      if not is_ok then None else (Some html)
    in
    let initial_content = previous_file_content html in
    let target_html, content_html =
      let deps =
        Ls.map abs_targets_and_contents ~f:(fun (_, t, _) -> t) in
      Make.MD5.make_file_target ?initial_content 
        ~filename:html ~build_cmd deps in
    (html, target_html, content_html), abs_targets_and_contents in 


  let list_with_targets =
    let list_brtxes = File_tree.str_and_path_list the_source_tree in
    
    let make_target_html (str, path) =
      let filename = build ^ "/" ^ (Filename.chop_extension str) ^ ".html" in
      let build_cmd () = 
        printf "Building %s\n" filename;
        let brtx = data_root ^ "/" ^ str in
        let title = Filename.chop_extension str in
        let from = path in
        let menu = HTML_menu.get_menu ~from menu_factory in
        let page = Data_source.get_page brtx in
        let toc = Brtx_transform.html_toc ~filename:str page in
        let html_buffer, err_buffer = 
          Brtx_transform.to_html ~todo_list ~filename:str ~from page in
        let is_ok =
        output_buffers
          ~output_content:(fun out ->
            let whole = 
              html_templ_fun ~from ~menu ~toc ~title
                (Buffer.contents html_buffer) in
            String_tree.print ~out whole;)
          filename err_buffer in

        if not is_ok then None else (Some filename)
      in
      let target_source, content_source = make_target_source str in
      let initial_content = previous_file_content filename in
      let target_html, content_html =
        Make.MD5.make_file_target ?initial_content 
          ~filename ~build_cmd [target_source] in
      (str, target_source, content_source,
       filename, target_html, content_html) in
    Ls.map list_brtxes ~f:make_target_html in

  let main_target, _ =
    let build_cmd () = Some () in (* printf "Built the HTML.\n" in *)
    let deps =
      (let _, tbib, _ = str_trg_ctt_biblio in tbib)
      :: (let _, tadb, _ = str_trg_ctt_addbook in tadb)
      :: (Ls.map list_with_targets ~f:(fun (_,_, _,_, th,_) -> th))
    in
    Make.MD5.make_phony_target ~name:"Main" ~build_cmd deps
  in

  if not (Make.make main_target) then (
    failwith "Phony target returns false??";
  );


  Todo_list.simplify todo_list;
  let previous_todo_content = 
    ref (Persistence.filter_files previous_content
           ~f:(fun (s, _) -> "todo:" =$= (Str.sub s 0 5))) in
  let remove_previous_todo str =
    previous_todo_content :=
      Ls.remove_if (fun (s, _) -> s =$= str) !previous_todo_content in
  let todo_targets_contents =
    Ls.map !todo_list
      ~f:(function 
          | `copy (path, from) ->
              let from_path = String.concat "/" (Ls.rev (Ls.tl from)) in
              let origin_str = "todo:" ^ from_path ^ "/" ^ path in
              let origin = from_path ^ "/" ^ path in
              let dest = build ^ "/" ^ from_path ^ "/" ^ path in
              let dest_str = "todo:" ^ dest in
              let build_cmd () =
                Dbw_sys.mkdir_p (Filename.dirname dest);
                try 
                  Dbw_sys.copy (sprintf "%s/%s" data_root origin) dest;
                  printf "Copied %s/%s to %s\n" data_root origin dest;
                  (Some dest)
                with e ->
                  printf "ERROR copying %s/%s to %s: %s\n"
                    data_root origin dest (Printexc.to_string e);
                  None
              in
              let target_source, content_source =
                make_target_source  ~prefix:"todo:" origin in
              let initial_content = previous_file_content dest_str in
              let target, content =
                Make.MD5.make_file_target ?initial_content 
                  ~filename:dest ~build_cmd [target_source] in
              remove_previous_todo origin_str;
              remove_previous_todo dest_str;
              (origin_str, target_source, content_source, 
               dest_str, target, content)
         ) in
  let todo_target, _ =
    let build_cmd () = Some () in
    let deps =
      Ls.map todo_targets_contents ~f:(fun (_,_,_,_,t,_) -> t) in
    Make.MD5.make_phony_target ~name:"Todos" ~build_cmd deps
  in
  if not (Make.make todo_target) then (
    failwith "Phony target returns false???";
  );

  begin match persistence_file with
  | None -> ()
  | Some pf ->
      let str_contents =
        List.concat [
          Ls.map todo_targets_contents ~f:(fun (s,_,c,_,_,_) -> (s, c));
          Ls.map todo_targets_contents ~f:(fun (_,_,_,s,_,c) -> (s, c));
          !previous_todo_content;
          [let s, _, c = str_trg_ctt_biblio in (s, c)];
          Ls.map str_trg_ctt_sebib_list ~f:(fun (s,_,c) -> (s, c));
          [let s, _, c = str_trg_ctt_addbook in (s, c)];
          Ls.map str_trg_ctt_addbook_list ~f:(fun (s,_,c) -> (s, c));
          Ls.map list_with_targets ~f:(fun (s,_,c,_,_,_) -> (s, c));
          Ls.map list_with_targets ~f:(fun (_,_,_,s,_,c) -> (s, c));
        ] in
      (* List.iter (fun (s,_) -> printf "str: %s\n" s) str_contents; *)
      Persistence.set_files previous_content str_contents;
      Persistence.save previous_content pf;
  end;

  ()


let () =
  let print_version = ref false in
  let html_tmpl = ref `none in
  let persistence = ref "" in

  let arg_cmd ~doc key spec = (key, spec, doc) in
  let usage = "usage: dbw [OPTIONS] <input-dir> <output-dir>" in
  let commands = [
    arg_cmd
      ~doc:"\n\tPrint version informations and exit"
      "-version"
      (Arg.Set print_version);
    arg_cmd
      ~doc:"<path>\n\tSet an HTML template file"
      "-template-file"
      (Arg.String (fun s -> html_tmpl := `file s));
    arg_cmd
      ~doc:"<name>\n\tSet an HTML `internal' template"
      "-named-template"
      (Arg.String (fun s -> html_tmpl := `named s));
    arg_cmd
      ~doc:"<path>\n\tUse a file for build persistence."
      "-persist-with"
      (Arg.Set_string persistence);
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
          if !persistence =$= "" then None else Some !persistence in
        transform ~html_template:!html_tmpl ?persistence_file i o 
    | _ -> 
        printf "Wrong number of arguments: %d\n" 
          (Ls.length anonymous_arguments);
        printf "%s\n" usage;
    end;
  );
  ()


