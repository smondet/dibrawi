

open Dibrawi_std

module Dbw_sys = Dibrawi.System


let output_buffers
    ~(output_content:Out_channel.t -> unit) file_name err_buffer =
  Dbw_sys.mkdir_p (Filename.dirname file_name);
  Out_channel.with_file file_name ~f:(fun o ->
    output_content o;
    fprintf o "%!";);
  let s = Buffer.contents err_buffer in
  if s <$> "" then (eprintf "Errors for %s:\n%s\n" file_name s;false)
  else true


open Dibrawi

let named_templates: (string * (unit ->  Dibrawi.HTML.Template.html_template)) list = 
  let default_insertion =
    fun from ->
      let buf, _ =
        Brtx_transform.to_html 
          ~from:(Option.value ~default:["NONE"] from) 
          "{link page:/Main|Main}{~}{link #|Top}{~}{link #pagefoot|Bottom}{br}\n\
              {link http://bracetax.berlios.de/bracetax_syntax.html|Bracetax Syntax}"
      in
      (Buffer.contents buf) in
  [
    "three_columns_greenish",
    (fun () -> Dibrawi.HTML.Template.Full.three_columns_greenish ());
    "three_columns_greenish-linked",
    (fun () -> Dibrawi.HTML.Template.Full.three_columns_greenish
      ~top_right:default_insertion ());
    "right_pane_redish",
    (fun () -> Dibrawi.HTML.Template.Full.with_sidepane_redish
      ~add_section_numbers:false ~side:`right ());
    "right_pane_redish-linked",
    (fun () -> Dibrawi.HTML.Template.Full.with_sidepane_redish
      ~add_section_numbers:false ~side:`right ~insertion:default_insertion ());
  ]


let transform ?(html_template=`none) ?(wiki_name="DBW")
    ?exclude ?filter ?persistence_file ?(verbosity=1) data_root build =

  let logf level = 
    if level <= verbosity then printf else ifprintf stdout in 

  let title_prefix = sprintf "[%s]" wiki_name in

  let the_source_tree =
    let may_apply o f t = match o with None -> t | Some s -> f s t in
    Data_source.get_file_tree ~data_root ()
    |! may_apply exclude File_tree.exclude_from_tree 
    |! may_apply filter File_tree.filter_tree_with_pattern in

  let todo_list = Todo_list.empty () in
  let list_sebibs =
    File_tree.str_and_path_list ~filter:"\\.sebib$"  the_source_tree in
  let list_abs =
    File_tree.str_and_path_list ~filter:"\\.abs$"  the_source_tree in
  

  let menu_factory =
    let cons_if c a b = if c then a :: b else b in
    let bib = 
      cons_if (List.length list_sebibs > 0) 
        (File_tree.File ("", "bibliography.brtx")) in
    let adb =
      cons_if (List.length list_abs > 0)
        (File_tree.File ("", "address_book.brtx")) in
    let source_menu = bib (adb the_source_tree) in
    HTML_menu.make_menu_factory source_menu
  in



  let previous_content =
    match Option.bind persistence_file Persistence.from_file with
    | None -> Persistence.empty ()
    | Some pc ->
      let menu = HTML_menu.get_menu ~from:["level0"] menu_factory in
      let current_menu_hash = Digest.string menu in
      let previous_menu_hash = Persistence.menu_hash pc in
      if previous_menu_hash =$= current_menu_hash then
        pc
      else (
        logf 0 "The File Tree has changed, everything must be rebuilt\n";
        Persistence.empty ~menu_hash:current_menu_hash ()
      )
 in
  
  let previous_file_content str = 
    Persistence.find_file_opt previous_content str in
  
  let make_target_source ?(prefix="") str =
    let filename = data_root ^ "/" ^ str in
    let build_cmd () =
      logf 1 "Source '%s%s' has changed [%s]\n" prefix str filename;
      Some filename in
    let initial_content = previous_file_content (prefix ^ str) in
    Make.MD5.make_file_target ?initial_content ~filename ~build_cmd [] in
  




  let html_templ_fun =
    let module Templating = Dibrawi.HTML.Template in
    match html_template with
    | `file f -> Templating.File.load_html (Data_source.get_file f)
    | `named name ->
      begin match List.find named_templates ~f:(fun (s, _) -> s = name) with
      | Some (_, t) -> t ()
      | None ->
        failwith (sprintf "Template \"%s\" not found." name)
      end
    | `none -> Templating.html_default
  in

  let dbw_prepro = Preprocessor.make () in

  let str_trg_ctt_biblio, str_trg_ctt_sebib_list = 
    let sebib_targets_and_contents =
      (List.map list_sebibs ~f:(fun (str, path) ->
        let target_source, content_source = make_target_source str in
        (str, target_source, content_source))) in
    let html = build ^ "/bibliography.html" in
    let build_cmd () =
      logf 1 "Build %s\n" html;
      let bib =
        Bibliography.load
          (List.map list_sebibs ~f:(fun (str, path) ->
            Data_source.get_page (data_root ^ "/" ^ str))) in
      let menu = HTML_menu.get_menu ~from:["bibliography"] menu_factory in
      let brtx = 
        dbw_prepro ~filename:"Bibliography" (Bibliography.to_brtx bib) in
      let toc = Brtx_transform.html_toc ~filename:"Bibliography" brtx in
      let from =  ["bibliography.html"] in
      let html_buffer, err_buffer = 
        Brtx_transform.to_html ~todo_list ~from brtx in
      let is_ok =
        output_buffers
          ~output_content:(fun out ->
            let whole = 
              html_templ_fun ~from ~menu ~toc 
                ~title:(title_prefix ^ "Bibliography")
                (Buffer.contents html_buffer) in
            String_tree.print ~out whole;)
          html err_buffer in
      if not is_ok then None else (Some html)
    in
    let initial_content = previous_file_content html in
    let target_html, content_html =
      let deps = List.map sebib_targets_and_contents ~f:(fun (_, t, _) -> t) in
      Make.MD5.make_file_target ?initial_content 
        ~filename:html ~build_cmd deps in
    (html, target_html, content_html), sebib_targets_and_contents in 
  
  let str_trg_ctt_addbook, str_trg_ctt_addbook_list = 
    let abs_targets_and_contents =
      (List.map list_abs ~f:(fun (str, path) ->
        let target_source, content_source = make_target_source str in
        (str, target_source, content_source))) in
    let html = build ^ "/address_book.html" in
    let build_cmd () =
      logf 1 "Build %s\n" html;
      let ab =
        Address_book.load
          (List.map list_abs ~f:(fun (str, path) ->
            Data_source.get_page (data_root ^ "/" ^ str))) in
      let menu = HTML_menu.get_menu ~from:["address_book"] menu_factory in
      let brtx =
        dbw_prepro ~filename:"Address Book" (Address_book.to_brtx ab) in
      let toc = Brtx_transform.html_toc ~filename:"address_book" brtx in
      let from =  ["address_book.html"] in
      let html_buffer, err_buffer = 
        Brtx_transform.to_html ~todo_list ~from brtx in
      let is_ok =
        output_buffers
          ~output_content:(fun out ->
            let whole = 
              html_templ_fun ~from ~menu ~toc 
                ~title:(title_prefix ^ "Address Book")
                (Buffer.contents html_buffer) in
            String_tree.print ~out whole;)
          html err_buffer in
      if not is_ok then None else (Some html)
    in
    let initial_content = previous_file_content html in
    let target_html, content_html =
      let deps = List.map abs_targets_and_contents ~f:(fun (_, t, _) -> t) in
      Make.MD5.make_file_target ?initial_content 
        ~filename:html ~build_cmd deps in
    (html, target_html, content_html), abs_targets_and_contents in 


  let list_with_targets =
    let list_brtxes = File_tree.str_and_path_list  the_source_tree in
    
    let make_target_html (str, path) =
      let filename = build ^ "/" ^ (Filename.chop_extension str) ^ ".html" in
      let build_cmd () = 
        logf 1 "Building %s\n" filename;
        let brtx = data_root ^ "/" ^ str in
        let title = 
          let s = Filename.chop_extension str in
          let pretty = 
            if s.[0] =@= '.' then String.(sub s 1 (length s - 1)) else s in
          title_prefix ^ pretty in
        let from = path in
        let menu = HTML_menu.get_menu ~from menu_factory in
        let page = dbw_prepro ~filename:str (Data_source.get_page brtx) in
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
    List.map list_brtxes ~f:make_target_html in

  let main_target, _ =
    let build_cmd () = Some () in (* printf "Built the HTML.\n" in *)
    let deps =
      (let _, tbib, _ = str_trg_ctt_biblio in tbib)
      :: (let _, tadb, _ = str_trg_ctt_addbook in tadb)
      :: (List.map list_with_targets ~f:(fun (_,_, _,_, th,_) -> th))
    in
    Make.MD5.make_phony_target ~name:"Main" ~build_cmd deps
  in

  if not (Make.make main_target) then (
    failwith "Phony target returns false??";
  );


  Todo_list.simplify todo_list;
  let previous_todo_content = 
    ref (Persistence.filter_files previous_content
           ~f:(fun (s, _) -> "todo:" =$= (String.sub s 0 5))) in
  let remove_previous_todo str =
    previous_todo_content :=
      List.filter ~f:(fun (s, _) -> s <> str) !previous_todo_content in
  let todo_targets_contents =
    List.map !todo_list ~f:(function 
    | `copy (path, from) ->
      let from_path = String.concat ~sep:"/" (List.rev (List.tl_exn from)) in
      let origin_str = "todo:" ^ from_path ^ "/" ^ path in
      let origin = from_path ^ "/" ^ path in
      let dest = build ^ "/" ^ from_path ^ "/" ^ path in
      let dest_str = "todo:" ^ dest in
      let build_cmd () =
        Dbw_sys.mkdir_p (Filename.dirname dest);
        try 
          Dbw_sys.copy (sprintf "%s/%s" data_root origin) dest;
          logf 1 "Copied %s/%s to %s\n" data_root origin dest;
          (Some dest)
        with e ->
          printf "ERROR copying %s/%s to %s: %s\n"
            data_root origin dest (Exn.to_string e);
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
    let deps = List.map todo_targets_contents ~f:(fun (_,_,_,_,t,_) -> t) in
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
          List.map todo_targets_contents ~f:(fun (s,_,c,_,_,_) -> (s, c));
          List.map todo_targets_contents ~f:(fun (_,_,_,s,_,c) -> (s, c));
          !previous_todo_content;
          [let s, _, c = str_trg_ctt_biblio in (s, c)];
          List.map str_trg_ctt_sebib_list ~f:(fun (s,_,c) -> (s, c));
          [let s, _, c = str_trg_ctt_addbook in (s, c)];
          List.map str_trg_ctt_addbook_list ~f:(fun (s,_,c) -> (s, c));
          List.map list_with_targets ~f:(fun (s,_,c,_,_,_) -> (s, c));
          List.map list_with_targets ~f:(fun (_,_,_,s,_,c) -> (s, c));
        ] in
      (* List.iter (fun (s,_) -> printf "str: %s\n" s) str_contents; *)
      Persistence.set_files previous_content str_contents;
      Persistence.save previous_content pf;
  end;

  ()

let print_version () =
  printf "dbw v. 0 (%s)\n" Dibrawi.Info.version_string;
  printf
    "OCaml: %s, PCRE: %s, Bracetax: %s, SeBib: %s\n"
    Sys.ocaml_version  Pcre.version
    Bracetax.Info.version Sebib.Info.version;
  ()



let transform_wiki name argv =
  let print_v = ref false in
  let html_tmpl = ref `none in
  let persistence = ref "" in
  let print_named_templates = ref false in
  let exclude = ref None in
  let filter = ref None in
  let wiki_name = ref None in
  let verbosity = ref None in

  let usage = 
    sprintf "Usage: dbw %s [OPTIONS] <input-dir> <output-dir>" name in 

  let arg_cmd ~doc key spec = (key, spec, doc) in
  let commands = [
    arg_cmd
      ~doc:"\n\tPrint version informations and exit"
      "-version"
      (Arg.Set print_v);
    arg_cmd
      ~doc:"<int>\n\tSet the verbosity level (default: 1)"
      "-verbosity"
      (Arg.Int (fun i -> verbosity := Some i));
    arg_cmd
      ~doc:"<path>\n\tSet an HTML template file"
      "-template-file"
      (Arg.String (fun s -> html_tmpl := `file s));
    arg_cmd
      ~doc:"<path>\n\tSet a name for the wiki"
      "-name"
      (Arg.String (fun s -> wiki_name := Some s));
    arg_cmd
      ~doc:"<name>\n\tSet an HTML `internal' template"
      "-named-template"
      (Arg.String (fun s -> html_tmpl := `named s));
    arg_cmd 
      ~doc:"\n\tList the available named templates"
      "-list-templates"
      (Arg.Set print_named_templates);
    arg_cmd
      ~doc:"<path>\n\tUse a file for build persistence."
      "-persist-with"
      (Arg.Set_string persistence);
    arg_cmd
      ~doc:"<regexp>\n\tExclude paths matching <regexp>"
      "-exclude"
      (Arg.String (fun s -> exclude := Some s));
    arg_cmd
      ~doc:"<regexp>\n\tKeep only paths matching <regexp>"
      "-filter"
      (Arg.String (fun s -> filter := Some s));
  ] in 
  let anonymous_arguments =
    let anons = ref [] in
    let anon_fun s = anons := s :: !anons in
    Arg.parse_argv argv commands anon_fun usage;
    List.rev !anons   in

  if !print_v then (
    print_version ()
  ) else (
    if !print_named_templates then (
      printf "Named templates:\n";
      List.iter named_templates ~f:(fun (s, _) ->
        printf "  %s\n" s;
      );
    ) else (
      begin match anonymous_arguments with
      | [i; o] ->
        let persistence_file =
          if !persistence =$= "" then None else Some !persistence in
        transform ?exclude:!exclude ?filter:!filter
          ?wiki_name:!wiki_name
          ~html_template:!html_tmpl ?verbosity:!verbosity ?persistence_file i o 
      | _ -> 
        printf "Wrong number of arguments: %d\n" 
          (List.length anonymous_arguments);
        printf "%s\n" usage;
      end;
    );
  );
  ()

let do_the_running mix_output output biblio_html_prefix to_preprocess mix_args =
  let output_file = "/tmp/dbwpp_camlmix.mlx" in
  let output_chan = open_out output_file in
  let citations = ref [] in
  let html_cite =
    let prefix = 
      Option.value ~default:Dibrawi.Preprocessor.default_html_biblio_page
        biblio_html_prefix in
    fun cites ->
      citations := cites :: !citations;
      Dibrawi.Preprocessor.default_html_cite prefix cites
  in
  List.iter to_preprocess ~f:(fun filename ->
    let page = Dibrawi.Data_source.get_file filename in
    let preprocessed =
      Dibrawi.Preprocessor.brtx2brtx ~mix_output
        ~html_cite ~output ~from:[filename] page in
    fprintf output_chan "%s\n" preprocessed;
  );
  close_out output_chan;
  let cites = List.concat !citations in
  
  Dibrawi.System.run_command 
    (sprintf "camlmix \
              -insert 'module Mix = Dibrawi_mix.Make(Camlmix) \
                let () = (\
                Mix.Params.set_output `%s; \
                Mix.Params.citations := [%s])\
              ' \
              -c -co /tmp/dbwpp_camlmix_out.ml %s"
       (match output with `html -> "html" | `pdf -> "pdf")
       (String.concat ~sep:";" (List.map ~f:(sprintf "%S") (List.rev cites)))
       output_file);
  Dibrawi.System.run_command 
    (sprintf "ocamlfind ocamlopt -package dibrawi -thread -linkpkg \
                /tmp/dbwpp_camlmix_out.ml -o /tmp/dbwpp_camlmix_out ");
  Dibrawi.System.run_command 
    (sprintf "/tmp/dbwpp_camlmix_out %s"
       (String.concat ~sep:" " (List.map ~f:(sprintf "'%s'") mix_args)));
  ()

let run_mix name argv =
  let biblio_html_prefix = ref None in
  let to_preprocess = ref [] in
  let out_format = ref `html in
  let mix_args = ref [] in
  let usage = sprintf "dbw %s [OPTIONS] file1 file2 ..." name in

  Arg.parse_argv argv [
    ("-version",
     Arg.Unit (print_version),
     "\n\tprint version");
    ("-biblio-html-prefix",
     Arg.String (fun s -> biblio_html_prefix := Some s),
     "<str>\n\tSet the prefix for HTML biblio-links to <str>");
    ("-html",
     Arg.Unit (fun () -> out_format := `html),
     "\n\tOutput for HTML format (default)");
    ("-latex",
     Arg.Unit (fun () -> out_format := `pdf),
     "\n\tOutput for LaTeX format");
    ("-arg", Arg.String (fun s -> mix_args := s :: !mix_args),
     "\n\tAdd argument for the mix-ed executable");
    ("-args", Arg.Rest (fun s ->  mix_args := s :: !mix_args),
     "\n\tAdd all following arguments for the mix-ed executable");
  ] (fun s -> to_preprocess := s :: !to_preprocess) usage;
  do_the_running `camlmix !out_format !biblio_html_prefix
    (List.rev !to_preprocess) (List.rev !mix_args)
    
let magic name argv =
  let arglist = Array.to_list argv in
  begin match List.tl_exn arglist with
  | [] -> eprintf "Easy guess … nothing to do.\n"
  | args ->
    let out_format = ref `html in
    let to_preprocess = ref [] in
    let mix_args = ref [] in
    let is_to_preprocess s = String.is_suffix s ~suffix:".brtx" in
    let implies_latex s =
      String.is_suffix s ".pdf" || String.is_prefix s "pdf"
      || String.is_suffix s ".tex" || String.is_prefix s "tex"
      || String.is_suffix s ".ltx" || String.is_prefix s "latex" in 
    if List.exists args ~f:implies_latex then (
      out_format := `pdf;
    );
    List.iter args ~f:(fun s ->
      if is_to_preprocess s then (
        to_preprocess := s :: !to_preprocess;
      ) else (
        mix_args := s :: !mix_args;
      )
    );
    do_the_running `camlmix !out_format (Some "")
      (List.rev !to_preprocess) (List.rev !mix_args)
  end

let prepro name argv =
  let citations_file = ref "" in
  let biblio_html_prefix = ref None in
  let to_preprocess = ref [] in
  let output_file = ref "" in
  let out_format = ref `html in
  let mix_output = ref `wiki in
  let usage = sprintf "dbw %s [OPTIONS] file1 file2 ..." name in

  Arg.parse_argv argv [
    ("-version",
     Arg.Unit (print_version),
     "\n\tprint version");
    ("-citations",
     Arg.Set_string citations_file,
     "<file> \n\tOutput citations to <file>");
    ("-output",
     Arg.Set_string output_file,
     "<file>\n\tOutput to file <file>");
    ("-biblio-html-prefix",
     Arg.String (fun s -> biblio_html_prefix := Some s),
     "<str>\n\tSet the prefix for HTML biblio-links to <str>");
    ("-html",
     Arg.Unit (fun () -> out_format := `html),
     "\n\tOutput for HTML format (default)");
    ("-latex",
     Arg.Unit (fun () -> out_format := `pdf),
     "\n\tOutput for LaTeX format");
    ("-camlmix",
     Arg.Unit (fun () -> mix_output := `camlmix),
     "\n\tOutput mix:*'s for Camlmix");

  ] (fun s -> to_preprocess := s :: !to_preprocess) usage;
  let output_citations_chan = 
    if !citations_file =$= "" then None else Some (open_out !citations_file)
  in
  let output_chan =
    if !output_file =$= "" then stdout else (open_out !output_file) in
  let citations = ref [] in
  let html_cite =
    let prefix = 
      Option.value ~default:Dibrawi.Preprocessor.default_html_biblio_page
        !biblio_html_prefix in
    fun cites ->
      citations := cites :: !citations;
      Dibrawi.Preprocessor.default_html_cite prefix cites
  in
  List.iter (List.rev !to_preprocess) ~f:(fun filename ->
    let page = Dibrawi.Data_source.get_file filename in
    let preprocessed =
      Dibrawi.Preprocessor.brtx2brtx ~mix_output:!mix_output
        ~html_cite ~output:!out_format ~from:[filename] page in
    fprintf output_chan "%s\n" preprocessed;
  );
  let cites = List.concat !citations in
  Option.iter output_citations_chan ~f:(fun o ->
    fprintf o "%s\n" (String.concat ~sep:" " (List.rev cites));
  );
  close_out output_chan;
  ()


let usage = "Usage: dbw {<command>, help} [OPTIONS | -help]"
let () =
  if Array.length Sys.argv = 1 then (
    printf "%s\n" usage;
  ) else (
    let argv = (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
    try 
      begin match Sys.argv.(1) with
      | "wiki" | "w" -> transform_wiki Sys.argv.(1) argv
      | "prepro" | "pp" | "p" -> prepro Sys.argv.(1) argv
      | "version" | "-v" | "-version" | "--version" -> print_version ()
      | "run" | "mix" -> run_mix Sys.argv.(1) argv
      | "magic" | "m" -> magic Sys.argv.(1) argv
      | "help" | "h" | "-help" | "-h" | "--help" ->
        eprintf "%s\n" usage;
        eprintf "Available commands:\n\
              \  * wiki | w: Build the whole wiki.\n\
              \  * prepro | pp | p: Run the preprocessor.\n\
              \  * version | -v | -version | --version: Print version.\n\
              \  * run | mix: Preprocess files and run camlmix.\n\
              \  * magic | m: Try some guessing and call `dbw run':\n\
              \    - Arguments ending with .brtx will be treated as files \
              to preprocess\n    - All the other arguments will be passed to \
              the executable\n      (like with -arg)\n    - If at least one \
              argument starts with “pdf”, “latex”, or “tex” \
              \n      or ends with “.tex”, “.pdf”, or “.ltx”, \
              this will imply -latex \n\
              \  * help | h | -help | -h | --help: Show this help.\n\
               Try also: dbw <command> -help\n"
      | s -> 
        eprintf "Unknown command: \"%s\"\n" s;
        exit 1
      end
    with 
    | Arg.Help msg | Arg.Bad msg -> eprintf "%s" msg
    | exn -> eprintf "dbw ends with an exception:\n%s\n" (Exn.to_string exn)
  )
