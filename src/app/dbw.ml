

open Dibrawi_std


let output_buffers
~(templ_fun:Dibrawi.Templating.template)
?title ?toc ?menu html_name html_buffer err_buffer = (
    ignore (Unix.system ("mkdir -p " ^ (Filename.dirname html_name)));
    File.with_file_out ~mode:[`create] html_name (fun o ->
        let html_content =
            templ_fun ?toc ?menu ?title (Buffer.contents html_buffer) in
        fprintf o p"%s" html_content;
        fprintf o p"%!";
    );
    let s = Buffer.contents err_buffer in
    if s <> "" then (eprintf p"Errors for %s:\n%s\n" html_name s;);
)

let transform ?(html_template="") data_root build = (
    open Dibrawi in
    let the_source_tree = 
        Data_source.get_file_tree ~data_root () in
    let todo_list = Todo_list.empty () in

    let templ_fun = 
        if html_template <> "" then 
            Templating.load (Data_source.get_file html_template)
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
        let menu = HTML_menu.html_menu ~from:["bibliography"] the_source_tree in
        let brtx = Bibliography.to_brtx bib in
        let toc = Brtx_transform.html_toc ~filename:"Bibliography" brtx in
        let html = build ^ "/bibliography.html" in
        let html_buffer, err_buffer = 
            Brtx_transform.to_html ~todo_list 
                ~from:["bibliography.html"] brtx in
        output_buffers ~templ_fun ~menu ~toc ~title:"Bibliography"
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
        output_buffers ~templ_fun ~menu ~toc ~title html html_buffer err_buffer;
    );

    Todo_list.do_things todo_list ~f:(function 
        | `copy (path, from) ->
            let from_path = String.concat "/" (Ls.rev (Ls.tl from)) in
            let origin = data_root ^ "/" ^ from_path ^ "/" ^ path in
            let dest = build ^ "/" ^ from_path ^ "/" ^ path in
            ignore (Unix.system ("mkdir -p " ^ (Filename.dirname dest)));
            ignore (Unix.system (sprintf p"cp %s %s" origin dest));
            printf p"Should copy: %s -> %s\n" origin dest;
            []
        | s -> [s]
    );
    printf p"Still TODO: %s\n" (Todo_list.to_string todo_list)

)

let () = (
    let print_version = ref false in
    let html_tmpl = ref "" in

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
                (Arg.Set_string html_tmpl)
        ] in 

    if !print_version then (
        printf p"dbw v. 0 (%s)\n" Dibrawi.Info.version_string;
        printf
            p"OCaml: %s, Batteries: %s, PCRE: %s, Bracetax: %s, SeBib: %s\n"
            Shell.ocaml_version Batteries_config.version Pcre.version
            Bracetax.Info.version Sebib.Info.version;
    ) else (
        begin match anon with
        | [i; o] -> transform ~html_template:!html_tmpl i o
        | _ -> 
            printf p"Wrong number of arguments: %d\n" (Ls.length anon);
        end;
    );

)


