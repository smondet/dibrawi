

open Dibrawi_std


let transform data_root build = (
    open Dibrawi in
    let the_source_tree = 
        Data_source.get_file_tree ~data_root () in

    let list_brtxes = File_tree.str_and_path_list the_source_tree in

    Ls.iter list_brtxes ~f:(fun (str, path) ->
        let brtx = data_root ^ str in
        let html = build ^ "/" ^ (Filename.chop_extension str) ^ ".html" in
        printf p"%s -> %s\n" brtx html;
        printf p"%{string list}\n" path;
        let from = path in
        let brtx_page =
            Data_source.get_page brtx |> Preprocessor.brtx2brtx ~from in
        let html_buffer = Buffer.create 1024 in
        let err_buffer = Buffer.create 512 in
        let writer, input_char =
            Bracetax.Transform.string_io brtx_page html_buffer err_buffer in
        let url_hook =
            Special_paths.rewrite_url ~from in
        Bracetax.Transform.brtx_to_html
            ~writer ~doc:true ~title:(Ls.hd path)
            ~filename:str ~img_hook:url_hook ~url_hook ~input_char ();

        ignore (Unix.system ("mkdir -p " ^ (Filename.dirname html)));
        File.with_file_out html (fun o ->
            Buffer.output_buffer o html_buffer
        );
        eprintf p"Errors for %s:\n%s\n" brtx (Buffer.contents err_buffer);

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
            printf p"OCaml: %s, Batteries: %s, PCRE: %s, Bracetax: %s\n"
                Shell.ocaml_version Batteries_config.version Pcre.version
                Bracetax.Info.version;
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


