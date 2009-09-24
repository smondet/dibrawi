open Print

let () = (
    match Sys.argv.(1) with
    | "-version" ->
        printf p"html_menu v. 0 (%s)\n" Dibrawi.version_string
    | s -> failwith (sprintf p"Unknown command: %s" s)
)
