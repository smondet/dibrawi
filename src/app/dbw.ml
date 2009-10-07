

open Dibrawi_std


let output_buffers
~(templ_fun:string -> string)
file_name content_buffer err_buffer = (
    ignore (Unix.system ("mkdir -p " ^ (Filename.dirname file_name)));
    File.with_file_out ~mode:[`create] file_name (fun o ->
        let content_content = templ_fun (Buffer.contents content_buffer) in
        fprintf o p"%s" content_content;
        fprintf o p"%!";
    );
    let s = Buffer.contents err_buffer in
    if s <> "" then (eprintf p"Errors for %s:\n%s\n" file_name s;);
)

let build_pdf ~latex_template texname = (
    let absolut s = 
        (Shell.getcwd ()) ^ "/" ^ (Filename.chop_extension texname) ^ s in
    let tmp =
        File.with_temporary_out ~mode:[`create] ~suffix:".tex" ~prefix:"dbw_"
            (fun o name ->
                fprintf o p"%s" (latex_template (absolut ".tex"));
                name
            ) in
    begin match Dibrawi.Latex.build tmp with
    | Unix.WEXITED 0 ->
        let n = (Filename.chop_extension tmp) in
        ignore (Unix.system ("mv " ^ n ^ ".pdf " ^ (absolut ".pdf")));
        ignore (Unix.system ("mv " ^ n ^ ".log " ^ (absolut ".log")));
    | Unix.WEXITED n ->
        printf p"PDF: Compilation of %s failed with error code: %d\n" texname n;
    | _ ->
        printf p"PDF: Compilation of %s got killed (?)\n" texname;
    end;
)


let transform
?(make_all_pdfs=false) ?(latex_template="") ?(html_template="")
data_root build = (
    open Dibrawi in
    let the_source_tree = 
        Data_source.get_file_tree ~data_root () in
    let todo_list = Todo_list.empty () in

    let html_templ_fun = 
        if html_template <> "" then 
            Templating.load_html (Data_source.get_file html_template)
        else
            Templating.html_default
    in
    let latex_templ_fun = 
        if latex_template <> "" then 
            Templating.load_latex (Data_source.get_file latex_template)
        else
            Templating.latex_default
    in
    let list_sebibs =
        File_tree.str_and_path_list ~filter:"\\.sebib$" the_source_tree in
    let () = 
        let bib =
            Bibliography.load (Ls.map list_sebibs 
                ~f:(fun (str, path) ->
                    Data_source.get_page (data_root ^ "/" ^ str)))
        in
        let menu = HTML_menu.html_menu ~from:["bibliography"] the_source_tree in
        let brtx = Bibliography.to_brtx bib in
        let toc = Brtx_transform.html_toc ~filename:"Bibliography" brtx in
        let html = build ^ "/bibliography.html" in
        let html_buffer, err_buffer = 
            Brtx_transform.to_html ~todo_list 
                ~from:["bibliography.html"] brtx in
        output_buffers
            ~templ_fun:(html_templ_fun ~menu ~toc ~title:"Bibliography")
            html html_buffer err_buffer;
    in




    let list_brtxes = File_tree.str_and_path_list the_source_tree in

    Ls.iter list_brtxes ~f:(fun (str, path) ->
        let brtx = data_root ^ "/" ^ str in
        let title = Filename.chop_extension str in
        let html = build ^ "/" ^ (Filename.chop_extension str) ^ ".html" in
        (* printf p"%s -> %s\n" brtx html; *)
        (* printf p"%{string list}\n" path; *)
        let from = path in
        let menu = HTML_menu.html_menu ~from the_source_tree in
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
                Brtx_transform.to_latex ~todo_list ~filename:str ~from page in
            output_buffers ~templ_fun:(fun s -> s) tex latex_buffer err_buffer;
            build_pdf
                ~latex_template:(latex_templ_fun ~title ~authors ~subtitle) tex;
        );
    );

    Todo_list.simplify todo_list;
    Todo_list.do_things todo_list ~f:(function 
        | `copy (path, from) ->
            let from_path = String.concat "/" (Ls.rev (Ls.tl from)) in
            let origin = data_root ^ "/" ^ from_path ^ "/" ^ path in
            let dest = build ^ "/" ^ from_path ^ "/" ^ path in
            ignore (Unix.system ("mkdir -p " ^ (Filename.dirname dest)));
            ignore (Unix.system (sprintf p"cp %s %s" origin dest));
            (* printf p"Should copy: %s -> %s\n" origin dest; *)
            []
        | s -> [s]
    );
    if not (Todo_list.is_empty todo_list) then (
        printf p"Still TODO: %s\n" (Todo_list.to_string todo_list)
    );

)

let () = (
    let print_version = ref false in
    let html_tmpl = ref "" in
    let latex_tmpl = ref "" in
    let all_pdfs = ref false in

    let usage = "usage: dbw [OPTIONS] <input-dir> <output-dir>" in
    let anon =
        Arg.handle ~usage [
            Arg.command
                ~doc:"\n\tPrint version informations and exit"
                "-version"
                (Arg.Set print_version);
            Arg.command
                ~doc:"\n\tSet an HTML template file"
                "-html-template"
                (Arg.Set_string html_tmpl);
            Arg.command
                ~doc:"\n\tSet a LaTeX template file"
                "-latex-template"
                (Arg.Set_string latex_tmpl);
            Arg.command
                ~doc:"\n\tBuild (or try to) all the PDFs"
                "-all-pdfs"
                (Arg.Set all_pdfs);
        ] in 

    if !print_version then (
        printf p"dbw v. 0 (%s)\n" Dibrawi.Info.version_string;
        printf
            p"OCaml: %s, Batteries: %s, PCRE: %s, Bracetax: %s, SeBib: %s\n"
            Shell.ocaml_version Batteries_config.version Pcre.version
            Bracetax.Info.version Sebib.Info.version;
    ) else (
        begin match anon with
        | [i; o] ->
            transform
                ~make_all_pdfs:!all_pdfs ~html_template:!html_tmpl
                ~latex_template:!latex_tmpl i o
        | _ -> 
            printf p"Wrong number of arguments: %d\n" (Ls.length anon);
            printf p"%s\n" usage;
        end;
    );

)


