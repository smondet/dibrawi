open Print


let test_data_src data_root = (
    let module D =  Dibrawi with File_tree, Data_source, HTML_menu in
    open D in
    let () =
        List.iter (printf p"-- %s<br/>\n")
            (string_path_list ~url_prefix:"prefix:"
                (get_file_tree ~data_root ())) in
    let () =
        let path_l = 
            path_list ~prefix:["prefix" ; "list"] (get_file_tree ~data_root ())
        in
        let pr path = 
            printf p"== %{string list} -- %s<br/>\n" path 
                (Special_paths.relativize path "/absolute/path") in
        List.iter pr path_l in
    let () =
        let from = [ "one" ; "two" ; "three" ] in
        let test ?output path = 
            let todo_list = ref [] in
            let rew =
                Special_paths.rewrite_url ?output ~todo_list ~from path in
            let todostr tl =
                String.concat ", " 
                    (List.map (function `compile_pdf pdf ->
                        sprintf p"Compile: %s" pdf) tl) in
            printf p"%s -> %s (%s)<br/>\n" path rew (todostr !todo_list); in
        test "nothing/to.do";
        test "page:relative/page";
        test "page:../../relative/page";
        test "page:/absolute/page";
        test "page:#";
        test "nothing/to.do#brout";
        test "page:../../relative/page#bout";
        test "page:/absolute/page#brout";
        test "page:#brout";
        test "page:/path#with/sharps";
        test "page:";
        test "page:#brout";
        test "pdf:../../relative/page#bout";
        test "pdf:/absolute/page#brout";
        test "pdf:#brout";
        test "pdf:/path#with/sharps";
        test "pdf:";
        test "pdf:#brout";
        test               "img:relative/page";
        test ~output:`html "img:relative/page";
        test ~output:`pdf  "img:../../relative/page";
        test ~output:`html "img:/absolute/page";
        test ~output:`pdf  "img:/absolute/page";
        test ~output:`html "cite:id09title";
        test ~output:`pdf  "cite:id09title";
        test ~output:`html "cite:id09title,if08more";
        test ~output:`pdf  "cite:id09title,if08more";
        test ~output:`html "pdfinc:relative/page";
        test ~output:`pdf  "pdfinc:../../relative/page";
        test ~output:`html "pdfinc:/absolute/page";
        test ~output:`pdf  "pdfinc:/absolute/page";
    in
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
