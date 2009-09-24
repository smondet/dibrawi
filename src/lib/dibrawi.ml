
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

    type file_tree = 
        | Dir of string * string * file_tree list
        | File of string * string


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
            explore data_root ""
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
        print 0 tree
    )


end

module HTML_menu = struct

end
