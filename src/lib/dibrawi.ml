
open Dibrawi_std

module Info = struct
    let version = 0
    let version_string = sprintf p"The Dibrawi library, v %d" version
end

module File_tree = struct

    type file_tree_item = 
        | Dir of string * string * file_tree
        | File of string * string
    and
    file_tree = file_tree_item list

    
    let fold_tree
    ?(dir=fun path name a -> a) ?(file=fun path name a -> ())
    init tree = (
        let rec parse current = function
            | Dir (path, name, l) ->
                let next = dir path name current in
                Ls.iter (parse next) l
            | File (path, name) ->
                file path name current
        in
        Ls.iter (parse init) tree
    )

    let print_tree =
        fold_tree
            ~dir:(fun path name indent -> 
                printf p"%s%s\n" (String.make indent ' ') name;
                indent + 2)
            ~file:(fun path name indent ->
                printf p"%s%s\n" (String.make indent ' ') name;)
            0


    let string_path_list
    ?(filter="\\.brtx$") ?(exclude=".*\\.svn.*") ?(url_prefix="")
    tree = (
        let filt = Pcre.regexp filter in
        let excl = Pcre.regexp exclude in
        let paths = ref [] in
        fold_tree
            ~file:(fun p n () ->
                let full = p ^ "/" ^ n in
                if (pcre_matches filt full) && not (pcre_matches excl full)
                then (paths := (url_prefix ^ full) :: !paths);) () tree;
        Ls.rev !paths
    )

    (* Render a path compatible with Future.Path in batteries *)
    let path_list 
    ?(filter="\\.brtx$") ?(exclude=".*\\.svn.*") ?(prefix=[])
    tree = (
        let filt = Pcre.regexp filter in
        let excl = Pcre.regexp exclude in
        let paths = ref [] in
        (* let current = ref prefix in *)
        fold_tree
            ~dir:(fun p n c ->
                Opt.bind (fun cc -> 
                    if not (pcre_matches excl n) then Some (n :: cc) else None) c) 
            ~file:(fun p n c ->
                Opt.may c ~f:(fun c ->
                    if (pcre_matches filt n)
                    then (paths := (n :: c) :: !paths);)
            )
            (Some prefix) tree;
        Ls.rev !paths
    )
end

module Data_source = struct
    (* Future: 
        - get_page: path -> string
        - is_valid_zipimg: path -> bool

        *)



    let get_file_tree ?(data_root="./data/") () = (
        open Shell, File_tree in
        let ls dir =
            let sort a = Array.fast_sort String.compare a; a in
            Shell.readdir dir |> sort |> Array.to_list in 
        if is_directory data_root then (
            let rec explore path name =
                let next_path = path ^ "/" ^ name in
                let real_path = data_root ^ next_path in
                if is_directory real_path then
                    Dir (path, name, Ls.map (explore next_path)  (ls real_path))
                else
                    File (path, name) 
            in
            Ls.map (explore ".")  (ls data_root)
        ) else (
            invalid_arg (sprintf p"%s is not a directory" data_root)
        )
    )



end

module HTML_menu = struct

    open Data_source
    open File_tree

    let brtx_menu
    ?(url_prefix="")
    ?(filter="\\.brtx$") ?(exclude_dir=".*\\.svn.*")
    ?(chop_filter=true) ?(replace=".html") tree = (
        let buf = Buffer.create 1024 in
        let filt = Pcre.regexp filter in
        let excl = Pcre.regexp exclude_dir in
        let rec to_brtx  = function
            | Dir (path, name, l) ->
                (* eprintf p"path: %s, name: %s\n" path name; *)
                if not (pcre_matches excl name) then (
                    Buffer.add_string buf 
                        (sprintf p"{*} %s\n{begin list}\n" name);
                    Ls.iter ~f:to_brtx l;
                    Buffer.add_string buf (sprintf p"{end} # %s\n" name);
                ) else (
                    Buffer.add_string buf (sprintf p"# ignore: %s %s\n" path name);
                );
            | File (path, name) ->
                if pcre_matches filt name then (
                    let rex = filt in
                    let link =
                        path ^ "/" ^ (Pcre.replace ~rex ~templ:replace name) in
                    let official_name =
                        if chop_filter
                        then Pcre.replace ~rex ~templ:"" name
                        else name in
                    Buffer.add_string buf
                        (sprintf p"{*} {link %s%s|%s}\n" url_prefix link official_name);
                );
        in
        Buffer.add_string buf (sprintf p"{begin list}\n");
        Ls.iter to_brtx tree;
        Buffer.add_string buf (sprintf p"{end} # Root\n");
        Buffer.contents buf
    )

    let html_menu 
    ?(url_prefix="")
    ?(filter="\\.brtx$") ?(exclude_dir=".*\\.svn.*")
    ?(chop_filter=true) ?(replace=".html") tree = (
        let brtx =
            brtx_menu
                ~url_prefix ~filter ~exclude_dir ~replace ~chop_filter tree in
        let buf, err = Buffer.create 42, Buffer.create 42 in
        let writer, input_char = Bracetax.Transform.string_io brtx buf err in
        Bracetax.Transform.brtx_to_html
            ~writer ~filename:"BRTX MENU" ~class_hook:"dibrawi_menu" ~input_char
            ~deny_bypass:true ();
        if Buffer.contents err <> "" then (
            eprintf p"Errors in the bracetax: \n%s\n------------%s\n"
                brtx (Buffer.contents err);
            failwith "brtx ended with errors";
        );
        (Buffer.contents buf)
    )
end

