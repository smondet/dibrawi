

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
    let name = (Filename.chop_extension tmp) in
    begin match Dibrawi.Latex.build tmp with
    | Unix.WEXITED 0 ->
        ignore (Unix.system ("mv " ^ name ^ ".pdf " ^ (absolut ".pdf")));
        ignore (Unix.system ("mv " ^ name ^ ".log " ^ (absolut ".log")));
    | Unix.WEXITED n ->
        printf p"PDF: Compilation of %s failed with error code: %d\n\
            see %s\n" texname n (absolut ".log");
        ignore (Unix.system ("mv " ^ name ^ ".log " ^ (absolut ".log")));
    | _ ->
        printf p"PDF: Compilation of %s got killed (?)\n\
            see %s\n" texname (absolut ".tex");
        ignore (Unix.system ("mv " ^ name ^ ".log " ^ (absolut ".log")));
    end;
)


let transform
?(make_all_pdfs=false) ?(dependent_pdfs=false)
?(biblio_pdf=false) ?(abook_pdf=false)
?(latex_template="") ?(html_template="")
data_root build = (
    open Dibrawi in
    let the_source_tree = 
        Data_source.get_file_tree ~data_root () in
    let todo_list = Todo_list.empty () in

    let source_menu =
        (   (File_tree.File ("", "bibliography.brtx"))
            :: (File_tree.File ("", "address_book.brtx"))
            :: the_source_tree) in
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
    let bibtex_path = 
        let bib =
            Bibliography.load (Ls.map list_sebibs 
                ~f:(fun (str, path) ->
                    Data_source.get_page (data_root ^ "/" ^ str)))
        in
        let menu = HTML_menu.html_menu ~from:["bibliography"] source_menu in
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
        let dot_bib = (Shell.getcwd ()) ^ "/" ^ build ^ "/biblio.bib" in
        File.with_file_out ~mode:[`create] dot_bib (fun o ->
            fprintf o p"%s" bibtex);
        if make_all_pdfs || biblio_pdf then (
            let tex = build ^ "/bibliography.tex" in
            let latex_buffer, err_buffer, (title, authors, subtitle) = 
                Brtx_transform.to_latex ~todo_list ~from brtx in
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
        let menu = HTML_menu.html_menu ~from:["address_book"] source_menu in
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
                Brtx_transform.to_latex ~todo_list ~from brtx in
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
        (* printf p"%s -> %s\n" brtx html; *)
        (* printf p"%{string list}\n" path; *)
        let from = path in
        let menu = HTML_menu.html_menu ~from source_menu in
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
            ignore (Unix.system ("mkdir -p " ^ (Filename.dirname dest)));
            ignore (Unix.system (sprintf p"cp %s %s" origin dest));
            (* printf p"Should copy: %s -> %s\n" origin dest; *)
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
                Brtx_transform.to_latex ~filename:tex ~from page in
            output_buffers ~templ_fun:(fun s -> s) tex latex_buffer err_buffer;
            let latex_template = 
                (latex_templ_fun ~title ~authors ~subtitle ~bibtex_path) in
            build_pdf ~latex_template tex;
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
    let bib_pdf = ref false in
    let ab_pdf = ref false in
    let dependent_pdfs = ref false in

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
            Arg.command
                ~doc:"\n\tBuild the \"needed\" PDFs"
                "-pdfs"
                (Arg.Set dependent_pdfs);
            Arg.command
                ~doc:"\n\tBuild the PDF of the bibliography"
                "-bib-pdf"
                (Arg.Set bib_pdf);
            Arg.command
                ~doc:"\n\tBuild the PDF of the address_book"
                "-ab-pdf"
                (Arg.Set ab_pdf);
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
                ~dependent_pdfs:!dependent_pdfs
                ~biblio_pdf:!bib_pdf
                ~abook_pdf:!ab_pdf
                ~latex_template:!latex_tmpl i o
        | _ -> 
            printf p"Wrong number of arguments: %d\n" (Ls.length anon);
            printf p"%s\n" usage;
        end;
    );

)


