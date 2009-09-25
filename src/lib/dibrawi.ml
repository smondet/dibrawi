
open Dibrawi_std

module Info = struct
    let version = 0
    let version_string = sprintf p"The Dibrawi library, v %d" version
end

type path = string list

module Data_source = struct
    (* Future: 
        - get_page: path -> string
        - is_valid_zipimg: path -> bool

        *)

    type file_tree_item = 
        | Dir of string * string * file_tree
        | File of string * string
    and
    file_tree = file_tree_item list


    let get_file_tree ?(data_root="./data/") () = (
        open Shell in
        let ls dir =
            let sort a = Array.fast_sort String.compare a; a in
            Shell.readdir dir |> sort |> Array.to_list in 
        if is_directory data_root then (
            let rec explore path name =
                let next_path = path ^ "/" ^ name in
                if is_directory next_path then
                    Dir (path, name, Ls.map (explore next_path)  (ls next_path))
                else
                    File (path, name) 
            in
            Ls.map (explore data_root)  (ls data_root)
        ) else (
            invalid_arg (sprintf p"%s is not a directory" data_root)
        )
    )

    let print_tree tree = (
        let sw = 2 in
        let rec print indent = function
            | Dir (path, name, l) ->
                printf p"%s%s\n" (String.make indent ' ') name;
                Ls.iter (print (indent + sw)) l
            | File (path, name) ->
                printf p"%s%s\n" (String.make indent ' ') name;
        in
        Ls.iter (print 0) tree
    )


end

module HTML_menu = struct

    open Data_source

    let brtx_menu
    ?(filter="\\.brtx$") ?(exclude_dir=".*\\.svn.*")
    ?(chop_filter=true) ?(replace=".html") tree = (
        let buf = Buffer.create 1024 in
        let filt = Pcre.regexp filter in
        let excl = Pcre.regexp exclude_dir in
        let does_match rex str =
            try ignore (Pcre.exec ~rex str); true with Not_found ->false in
        let rec to_brtx  = function
            | Dir (path, name, l) ->
                (* eprintf p"path: %s, name: %s\n" path name; *)
                if not (does_match excl name) then (
                    Buffer.add_string buf 
                        (sprintf p"{*} %s\n{begin list}\n" name);
                    Ls.iter ~f:to_brtx l;
                    Buffer.add_string buf (sprintf p"{end} # %s\n" name);
                ) else (
                    Buffer.add_string buf (sprintf p"# ignore: %s %s\n" path name);
                );
            | File (path, name) ->
                if does_match filt name then (
                    let rex = filt in
                    let link =
                        path ^ "/" ^ (Pcre.replace ~rex ~templ:replace name) in
                    let official_name =
                        if chop_filter
                        then Pcre.replace ~rex ~templ:"" name
                        else name in
                    Buffer.add_string buf
                        (sprintf p"{*} {link %s|%s}\n" link official_name);
                );
        in
        Buffer.add_string buf (sprintf p"{begin list}\n");
        Ls.iter to_brtx tree;
        Buffer.add_string buf (sprintf p"{end} # Root\n");
        Buffer.contents buf
    )

    let html_menu 
    ?(filter="\\.brtx$") ?(exclude_dir=".*\\.svn.*")
    ?(chop_filter=true) ?(replace=".html") tree = (
        let brtx = brtx_menu ~filter ~exclude_dir ~replace ~chop_filter tree in
        brtx
    )
end
