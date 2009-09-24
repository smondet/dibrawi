open Print


let test_data_src () = (
    open Dibrawi.Data_source in
    (get_file_tree ~data_root:"./" ()) |> print_tree;
)
let () = (
    match Sys.argv.(1) with
    | "-version" ->
        printf p"html_menu v. 0 (%s)\n" Dibrawi.Info.version_string
    | "test" ->
        test_data_src ()
    | s -> failwith (sprintf p"Unknown command: %s" s)
)
