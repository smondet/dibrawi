

open Dibrawi_std

let output_buffers ?toc ?menu html_name html_buffer err_buffer = (
    ignore (Unix.system ("mkdir -p " ^ (Filename.dirname html_name)));
    File.with_file_out html_name (fun o ->
        let html_content = 
            Dibrawi.Templating.html_default
                ?toc ?menu
                ~title:html_name (Buffer.contents html_buffer) in
        output_string o html_content
    );
    let s = Buffer.contents err_buffer in
    if s <> "" then (eprintf p"Errors for %s:\n%s\n" html_name s;);
)

let transform data_root build = (
    open Dibrawi in
    let the_source_tree = 
        Data_source.get_file_tree ~data_root () in

    let list_sebibs =
        File_tree.str_and_path_list ~filter:"\\.sebib$" the_source_tree in
    let () = 
        let bib =
            Bibliography.load (Ls.map list_sebibs 
                ~f:(fun (str, path) -> Data_source.get_page (data_root ^ str)))
        in
        let menu = HTML_menu.html_menu ~from:["bibliography"] the_source_tree in
        let brtx = Bibliography.to_brtx bib in
        let html = build ^ "/bibliography.html" in
        let html_buffer, err_buffer = 
            Brtx_transform.to_html 
                ~from:["bibliography.html"] brtx in
        output_buffers ~menu html html_buffer err_buffer;
    in




    let list_brtxes = File_tree.str_and_path_list the_source_tree in

    Ls.iter list_brtxes ~f:(fun (str, path) ->
        let brtx = data_root ^ str in
        let html = build ^ "/" ^ (Filename.chop_extension str) ^ ".html" in
        (* printf p"%s -> %s\n" brtx html; *)
        (* printf p"%{string list}\n" path; *)
        let from = path in
        let menu = HTML_menu.html_menu ~from the_source_tree in
        let html_buffer, err_buffer = 
            Brtx_transform.to_html ~filename:str 
                ~from (Data_source.get_page brtx) in
        output_buffers ~menu html html_buffer err_buffer;


    );

)

let () = (
    let usage = "rtfm: dbw -help" in

    if Array.length Sys.argv = 1 then (
        printf p"%s\n" usage;
    ) else (
        match Sys.argv.(1) with
        | "-version" ->
            printf p"dbw v. 0 (%s)\n" Dibrawi.Info.version_string;
            printf
                p"OCaml: %s, Batteries: %s, PCRE: %s, Bracetax: %s, SeBib: %s\n"
                Shell.ocaml_version Batteries_config.version Pcre.version
                Bracetax.Info.version Sebib.Info.version;
        | "-help" ->
            printf p"dwb -help, or dwb -version, or dwb <datadir> <targetdir>\n";

        | s ->
            if Array.length Sys.argv = 3 then (
                transform Sys.argv.(1) Sys.argv.(2)
            ) else (
                printf p"%s\n" usage;
            )
    )
)


