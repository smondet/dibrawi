

open Dibrawi_std

module Dbw_unix = struct
    (** create a directory but doesn't raise an exception if the directory
      * already exist *)
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
~(templ_fun:string -> string)
file_name content_buffer err_buffer = (
    Dbw_unix.mkdir_p (Filename.dirname file_name);
    Dbw_unix.with_new_out file_name (fun o ->
        let content_content = templ_fun (Buffer.contents content_buffer) in
        fprintf o "%s" content_content;
        fprintf o "%!";
    );
    let s = Buffer.contents err_buffer in
    if s <$> "" then (eprintf "Errors for %s:\n%s\n" file_name s;);
)

let build_pdf ~latex_template texname = (
    let absolut s = 
        (Sys.getcwd ()) ^ "/" ^ (Filename.chop_extension texname) ^ s in
    let tmp =
        Dbw_unix.with_new_tmp ~suffix:".tex" ~prefix:"dbw_"
            (fun o name ->
                 fprintf o "%s" (latex_template (absolut ".tex"));
                 name
            ) in
    let name = (Filename.chop_extension tmp) in
    begin match Dibrawi.Latex.build tmp with
    | Unix.WEXITED 0 ->
        ignore (Unix.system ("mv " ^ name ^ ".pdf " ^ (absolut ".pdf")));
        ignore (Unix.system ("mv " ^ name ^ ".log " ^ (absolut ".log")));
    | Unix.WEXITED n ->
        printf "PDF: Compilation of %s failed with error code: %d\n\
            see %s\n" texname n (absolut ".log");
        ignore (Unix.system ("mv " ^ name ^ ".log " ^ (absolut ".log")));
    | _ ->
        printf "PDF: Compilation of %s got killed (?)\n\
            see %s\n" texname (absolut ".tex");
        ignore (Unix.system ("mv " ^ name ^ ".log " ^ (absolut ".log")));
    end;
)

open Dibrawi

let transform
?(make_all_pdfs=false) ?(dependent_pdfs=false)
?(biblio_pdf=false) ?(abook_pdf=false)
?(latex_template="") ?(html_template="")
?(href_is_footnote=false)
?things_to_build
data_root build = (
    let the_source_tree = 
        Data_source.get_file_tree ~data_root () in
    let todo_list = Todo_list.empty () in

    let menu_factory =
        let source_menu =
            (   (File_tree.File ("", "bibliography.brtx"))
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
    let latex_templ_fun = 
        if latex_template <$> "" then 
            Templating.load_latex (Data_source.get_file latex_template)
        else
            Templating.latex_default
    in
    let list_sebibs =
        File_tree.str_and_path_list ~filter:"\\.sebib$" the_source_tree in
    let bibtex_path = 
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
        let bibtex = Bibliography.bibtex bib in
        let dot_bib = (Sys.getcwd ()) ^ "/" ^ build ^ "/biblio.bib" in
        Dbw_unix.with_new_out dot_bib (fun o ->
            fprintf o "%s" bibtex);
        if make_all_pdfs || biblio_pdf then (
            let tex = build ^ "/bibliography.tex" in
            let latex_buffer, err_buffer, (title, authors, subtitle) = 
                Brtx_transform.to_latex
                    ~href_is_footnote ~todo_list ~from brtx in
            output_buffers ~templ_fun:(fun s -> s) tex latex_buffer err_buffer;
            let title, authors, subtitle = "Bibliography", "", "" in
            let latex_template = 
                (latex_templ_fun ~title ~authors ~subtitle ~bibtex_path:dot_bib)
            in
            build_pdf ~latex_template tex;
        );
        dot_bib
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
        if make_all_pdfs || abook_pdf then (
            let tex = build ^ "/address_book.tex" in
            let latex_buffer, err_buffer, (title, authors, subtitle) = 
                Brtx_transform.to_latex
                    ~href_is_footnote ~todo_list ~from brtx in
            output_buffers ~templ_fun:(fun s -> s) tex latex_buffer err_buffer;
            let title, authors, subtitle = 
                "Address Book", "Sebastien Mondet", "" in
            let latex_template = 
                (latex_templ_fun ~title ~authors ~subtitle ~bibtex_path) in
            build_pdf ~latex_template tex;
        );
    in




    let list_brtxes = File_tree.str_and_path_list the_source_tree in

    Ls.iter list_brtxes ~f:(fun (str, path) ->
        let brtx = data_root ^ "/" ^ str in
        let title = Filename.chop_extension str in
        let html = build ^ "/" ^ (Filename.chop_extension str) ^ ".html" in
        (* printf "%s -> %s\n" brtx html; *)
        (* printf "%{string list}\n" path; *)
        let from = path in
        let menu = HTML_menu.get_menu ~from menu_factory in
        let page = Data_source.get_page brtx in
        let toc = Brtx_transform.html_toc ~filename:str page in
        let html_buffer, err_buffer = 
            Brtx_transform.to_html ~todo_list ~filename:str ~from page in
        output_buffers ~templ_fun:(html_templ_fun ~menu ~toc ~title)
            html html_buffer err_buffer; 
        if make_all_pdfs then (
            (* let title = Filename.chop_extension str in *)
            let tex = build ^ "/" ^ (Filename.chop_extension str) ^ ".tex" in
            let from = path in
            let latex_buffer, err_buffer, (title, authors, subtitle) = 
                Brtx_transform.to_latex
                    ~href_is_footnote ~todo_list ~filename:str ~from page in
            output_buffers ~templ_fun:(fun s -> s) tex latex_buffer err_buffer;
            let latex_template = 
                (latex_templ_fun ~title ~authors ~subtitle ~bibtex_path) in
            build_pdf ~latex_template tex;
        );
    );

    Todo_list.simplify todo_list;
    Todo_list.do_things todo_list ~f:(function 
        | `copy (path, from) ->
            let from_path = String.concat "/" (Ls.rev (Ls.tl from)) in
            let origin = data_root ^ "/" ^ from_path ^ "/" ^ path in
            let dest = build ^ "/" ^ from_path ^ "/" ^ path in
            Dbw_unix.mkdir_p (Filename.dirname dest);
            ignore (Unix.system (sprintf "cp %s %s" origin dest));
            (* printf "Should copy: %s -> %s\n" origin dest; *)
            []
        | `pdf (path, from) when dependent_pdfs ->
            let from_path = String.concat "/" (Ls.rev (Ls.tl from)) in
            let pdf = from_path ^ "/" ^ path in
            let tex =
                build ^ "/" ^ (Filename.chop_extension pdf) ^ ".tex" in
            let brtx =
                data_root ^ "/" ^ (Filename.chop_extension pdf) ^ ".brtx"
            in
            let page = Data_source.get_page brtx in
            let latex_buffer, err_buffer, (title, authors, subtitle) = 
                Brtx_transform.to_latex
                    ~href_is_footnote ~filename:tex ~from page in
            output_buffers ~templ_fun:(fun s -> s) tex latex_buffer err_buffer;
            let latex_template = 
                (latex_templ_fun ~title ~authors ~subtitle ~bibtex_path) in
            build_pdf ~latex_template tex;
            []
        | s -> [s]
    );
    if not (Todo_list.is_empty todo_list) then (
        printf "Still TODO: %s\n" (Todo_list.to_string todo_list)
    );

)

let () = (
    let print_version = ref false in
    let html_tmpl = ref "" in
    let latex_tmpl = ref "" in
    let all_pdfs = ref false in
    let bib_pdf = ref false in
    let ab_pdf = ref false in
    let dependent_pdfs = ref false in
    let to_build = ref [] in
    let footnote_links = ref false in

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
            ~doc:"<path>\n\tSet a LaTeX template file"
            "-latex-template"
            (Arg.Set_string latex_tmpl);
        arg_cmd
            ~doc:"\n\tBuild (or try to) all the PDFs"
            "-all-pdfs"
            (Arg.Set all_pdfs);
        arg_cmd
            ~doc:"\n\tBuild the \"needed\" PDFs"
            "-pdfs"
            (Arg.Set dependent_pdfs);
        arg_cmd
            ~doc:"\n\tBuild the PDF of the bibliography"
            "-bib-pdf"
            (Arg.Set bib_pdf);
        arg_cmd
            ~doc:"\n\tIn PDFs, put all links as footnotes"
            "-footnote-links"
            (Arg.Set footnote_links);
        arg_cmd
            ~doc:"<dbw-path>\n\tBuild only <path>"
            "-build"
            (Arg.String (fun s -> to_build := s :: !to_build));
        arg_cmd
            ~doc:"\n\tBuild the PDF of the address_book"
            "-ab-pdf"
            (Arg.Set ab_pdf);
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
        let things_to_build = 
            match !to_build with
            | [] -> None
            | l -> 
                  let parse s =
                      let todo_list = ref [] in
                      let path =
                          Dibrawi.Special_paths.rewrite_url
                              ~todo_list ~from:["CmdLine"] s in
                      match !todo_list with
                      | (`pdf (p, f)) :: [] ->
                            printf "pdf: p: %s f: %s, path: %s\n" 
                                p (Str.concat "//" f) path;
                            (`pdf, path)
                      | _ -> (`html, path)
                  in
                  Some (Ls.rev_map l ~f:parse)
        in
        begin match anonymous_arguments with
        | [i; o] ->
              transform
                  ?things_to_build
                  ~make_all_pdfs:!all_pdfs ~html_template:!html_tmpl
                  ~dependent_pdfs:!dependent_pdfs
                  ~biblio_pdf:!bib_pdf
                  ~abook_pdf:!ab_pdf
                  ~href_is_footnote:!footnote_links
                  ~latex_template:!latex_tmpl i o
        | _ -> 
              printf "Wrong number of arguments: %d\n" 
                  (Ls.length anonymous_arguments);
              printf "%s\n" usage;
        end;
    );

)


