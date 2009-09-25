open Print


let test_data_src data_root = (
    open Dibrawi.Data_source in
    open Dibrawi.HTML_menu in
    (* let () = *)
        (* get_file_tree ~data_root:"./" () |> print_tree in *)
    get_file_tree ~data_root () |> html_menu  |> print_string;
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
