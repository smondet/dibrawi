open Print


let test_data_src data_root = (
    let module D =  Dibrawi with File_tree, Data_source, HTML_menu in
    open D in
    let () =
        List.iter (printf p"-- %s<br/>\n")
            (string_path_list ~url_prefix:"prefix:"
                (get_file_tree ~data_root ())) in
    let () =
        List.iter (printf p"== %{string list}<br/>\n")
            (path_list ~prefix:["prefix" ; "list"]
                (get_file_tree ~data_root ())) in
    let url_prefix =
        Shell.getcwd () ^ "/" ^ data_root in
    get_file_tree ~data_root () |> html_menu ~url_prefix |> print_string;
)
let () = (
    match Sys.argv.(1) with
    | "-version" ->
        printf p"html_menu v. 0 (%s)\n" Dibrawi.Info.version_string;
        printf p"Batteries: %s, PCRE: %s, Bracetax: %s\n"
            Batteries_config.version Pcre.version Bracetax.Info.version;
    | s -> test_data_src s
    (* | s -> failwith (sprintf p"Unknown command: %s" s) *)
)
