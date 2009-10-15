TYPE_CONV_PATH "ModuleDibrawi"

open Dibrawi_std

module Templating = Dibrawi_templating

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
                printf p"%s%s\n" (Str.make indent ' ') name;
                indent + 2)
            ~file:(fun path name indent ->
                printf p"%s%s\n" (Str.make indent ' ') name;)
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

    let str_and_path_list 
    ?(filter="\\.brtx$") ?(exclude=".*\\.svn.*") ?(prefix=("", []))
    tree = (
        let filt = Pcre.regexp filter in
        let excl = Pcre.regexp exclude in
        let paths = ref [] in
        (* let current = ref prefix in *)
        fold_tree
            ~dir:(fun p n c ->
                Opt.bind (fun cc -> 
                    if not (pcre_matches excl n)
                    then Some (n :: cc) else None) c) 
            ~file:(fun p n c ->
                Opt.may c ~f:(fun c ->
                    if (pcre_matches filt n)
                    then (
                        let to_add = 
                            ((fst prefix) ^ p ^ "/" ^ n, n :: c) in
                        paths := to_add :: !paths;
                    );
                ))
            (Some (snd prefix)) tree;
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
            let sort a = Array.fast_sort Str.compare a; a in
            Shell.readdir dir |> sort |> Array.to_list in 
        if is_directory data_root then (
            let rec explore path name =
                let next_path = path ^ "/" ^ name in
                let real_path = data_root ^ "/" ^ next_path in
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

    let get_page path = (
        (IO.read_all (open_in path))
    )
    let get_file path = (
        (IO.read_all (open_in path))
    )


end


module Todo_list = struct

    type todo = [
        | `pdf  of (string * (string list))
        | `copy of (string * (string list))
        | `bibtex
    ]
    type t = todo list ref

    let empty () = ref []
    let is_empty t = !t = []

    let to_string ?(sep="; ") tl =
        String.concat sep (Ls.map !tl ~f:(function
            | `pdf (path, from) ->
                sprintf p"Build PDF: %s from %{string list}" path from
            | `copy (path, from) ->
                sprintf p"Copy File: %s from %{string list}" path from
            | `bibtex -> "Build the BibTeX"
        ))
    let iter t ~f = Ls.iter !t ~f
    let do_things t ~(f:todo -> todo list) =
        t := Ls.concat (Ls.map !t ~f)

    let simplify t = t := Ls.unique !t
end

module Special_paths = struct

    let relativize from path = (
        try match path.[0] with
            | '/' ->
                let depth = Ls.length from - 1 in
                "./" ^ (Str.concat "/" (Ls.init depth ~f:(fun _ -> ".."))) ^ path
            | '#' ->
                (Filename.chop_extension (Ls.hd from)) ^ path
            | _ -> path
        with _ -> (Filename.chop_extension (Ls.hd from)) (* the string is empty*)
    )
    let typify url extension = (
        match Str.rev_idx url '#', Str.rev_idx url '/' with
        | Some h , None ->
            (Str.head url h) ^ extension ^ (Str.tail url h)
        | Some h, Some l when h > l ->
            (Str.head url h) ^ extension ^ (Str.tail url h)
        | _, _ ->
            url ^ extension
    )
    let compute_path from url extension =
        typify (relativize from url) extension

    let rec rewrite_url
    ?todo_list ?(output=`html)  ~from url = (
        match url with
        | s when Str.length s < 4 -> s
        | s when Str.head s 4 = "pdf:" ->
            let pdfpath = 
                compute_path from (Str.tail s 4) ".pdf" in
            Opt.may todo_list ~f:(fun rl ->
                rl := (`pdf (pdfpath, from)) :: !rl;
            );
            pdfpath 
        | s when Str.head s 5 = "page:" ->
            (compute_path from (Str.tail s 5) ".html")
        | s when Str.head s 4 = "fig:" ->
            let path =
                compute_path from (Str.tail s 4)
                    (match output with `html -> ".png" | `pdf -> ".pdf") in
            Opt.may todo_list ~f:(fun rl -> rl := (`copy (path, from)) :: !rl;);
            path
        | s when Str.head s 6 = "media:" ->
            let path = compute_path from (Str.tail s 6) "" in
            Opt.may todo_list ~f:(fun rl -> rl := (`copy (path, from)) :: !rl;);
            path
        | s -> s
    )

end

module Preprocessor = struct

    let prepro_regexp =
        Pcre.regexp "\\{cite\\s+[^\\}]*\\}"

    let brtx2brtx
    ?todo_list
    ?(html_biblio_page="page:/bibliography") ?(output=`html) ~from brtx = (

        Pcre.substitute ~rex:prepro_regexp brtx ~subst:(fun s ->
            (* Shell.catch_break true; *)
            match s with
            | cite when Str.head cite 5 = "{cite" ->
                let cites =
                    Ls.map
                        (Str.nsplit (Str.sub s 6 (String.length s - 7)) ",")
                        ~f:Str.trim in
                if output = `html then (
                    "[" ^ (Str.concat ", "
                        (Ls.map cites ~f:(fun cite ->
                            sprintf p"{link %s#%s|%s}"
                                html_biblio_page cite cite))) ^ "]"
                ) else (
                    Opt.may todo_list ~f:(fun tl -> tl := `bibtex :: !tl);
                    sprintf p"{bypass}\\cite{%s}{end}"
                        (Str.concat "," cites)
                        (* (Str.sub s 6 (String.length s - 7)) *)
                )
            | s -> s;
        )
    )
end

(******************************************************************************)
module Bibliography = struct

    (* str_list is a list S-Expressions (already loaded in memory) *)
    let load str_list = (
        Sebib.Biblio.set_of_string (Str.concat " " str_list)
    )
    let to_brtx biblio = (
        let pattern = "
            {section 1 @{id}|{t|@{id}}}\
            {cite @{id}}{br}\
            {b|@{title}}{br} \
            @{if (has authors)}@{authors-and}\
                @{else}{i|-- no authors --}@{endif}{br}\
            @{year} - {i|@{how}}{br} \
            @{if (lo ((has url) (has pdfurl) (has doi)))} {b|Links:} \
            @{if (has url)}{t|{link @{url}|URL}}@{endif} \
            @{if (has pdfurl)}{t|{link @{pdfurl}|PDF}}@{endif} \
            @{if (has doi)}{t|{link @{doi}|doi}}@{endif}{br}@{endif} \
            {b|Tags:} {i|@{tags}} {br} \
            @{if (has keywords)}{b|Keywords:} {i|@{keywords}} {br}@{endif} \
            @{if (has abstract)}{b|Abstract:} {br} @{abstract} {br}@{endif} \
            @{if (has comments)}{b|Comments:} {br} @{comments}@{endif}" in
        "{header|{title|Bibliography}}" ^ (Sebib.Format.str ~pattern biblio)
    )
    let bibtex = Sebib.BibTeX.str
end


module Brtx_transform = struct

    (* TODO handle errors better *)
    let to_html ?todo_list ?class_hook ?filename ~from brtx = (
        let brtx_page =
            Preprocessor.brtx2brtx ?todo_list ~from brtx in
        let html_buffer = Buffer.create 1024 in
        let err_buffer = Buffer.create 512 in
        let writer, input_char =
            Bracetax.Transform.string_io brtx_page html_buffer err_buffer in
        let url_hook =
            Special_paths.rewrite_url ?todo_list ~from in
        Bracetax.Transform.brtx_to_html
            ~writer ?filename ?class_hook
            ~img_hook:url_hook ~url_hook ~input_char ();
        (html_buffer, err_buffer)
    )

    let html_toc ?filename brtx = (
        let brtx_buffer = Buffer.create 1024 in
        let err_buffer = Buffer.create 512 in
        let writer, input_char =
            Bracetax.Transform.string_io brtx brtx_buffer err_buffer in
        Bracetax.Transform.get_TOC ~writer ~input_char ?filename ();
        let h, e =
            to_html ~class_hook:"dbwtoc" ~from:[""]
                (Buffer.contents brtx_buffer) in
        (Buffer.contents h)
    )

    let to_latex ?todo_list ?filename ~from brtx = (
        let output = `pdf in
        let brtx_page =
            Preprocessor.brtx2brtx ~output ?todo_list ~from brtx in
        let latex_buffer = Buffer.create 1024 in
        let err_buffer = Buffer.create 512 in
        let writer, input_char =
            Bracetax.Transform.string_io brtx_page latex_buffer err_buffer in
        let url_hook = Special_paths.rewrite_url ~output ?todo_list ~from in
        let separate_header = ref ("", "", "") in
        Bracetax.Transform.brtx_to_latex
            ~separate_header
            ~writer ?filename ~img_hook:url_hook ~url_hook ~input_char ();
        (latex_buffer, err_buffer, !separate_header)
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
        let presort l =
            let dirs, files =
                Ls.partition (function Dir _ -> false | _ -> true) l in
            dirs @ files in
        let rec to_brtx  = function
            | Dir (path, name, l) ->
                (* eprintf p"path: %s, name: %s\n" path name; *)
                if not (pcre_matches excl name) then (
                    Buffer.add_string buf 
                        (sprintf p"{*} %s\n{begin list}\n" name);
                    Ls.iter ~f:to_brtx (presort l);
                    Buffer.add_string buf (sprintf p"{end} # %s\n" name);
                ) else (
                    Buffer.add_string buf
                        (sprintf p"# ignore: %s %s\n" path name);
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
        Ls.iter to_brtx (presort tree);
        Buffer.add_string buf (sprintf p"{end} # Root\n");
        Buffer.contents buf
    )

    let html_menu 
    ?(url_prefix="page:/")
    ?(filter="\\.brtx$") ?(exclude_dir=".*\\.svn.*")
    ?(chop_filter=true) ?(replace="") ~from tree = (
        let brtx =
            brtx_menu
                ~url_prefix ~filter ~exclude_dir ~replace ~chop_filter tree in
        let buf, err =
            Brtx_transform.to_html
                ~filename:"BRTX MENU" ~class_hook:"dibrawi_menu" ~from brtx in
        if Buffer.contents err <> "" then (
            eprintf p"Errors in the bracetax: \n%s\n------------%s\n"
                brtx (Buffer.contents err);
            failwith "brtx ended with errors";
        );
        (Buffer.contents buf)
    )
end

module Latex = struct

    let build path = (
        let pwd = Shell.getcwd () in
        let cd = Filename.dirname path in
        Shell.chdir cd;
        let pdflatex = 
            "pdflatex -interaction=nonstopmode" in
        let target = Filename.chop_extension (Filename.basename path) in
        let return =
            Unix.system (sprintf p"%s %s > /dev/null" pdflatex target) in
        if return = Unix.WEXITED 0 then (
            let _ =
                Unix.system (sprintf p"bibtex %s > /dev/null"  target) in
            let _ =
                Unix.system (sprintf p"%s %s > /dev/null" pdflatex target) in
            let _ =
                Unix.system (sprintf p"%s %s > /dev/null" pdflatex target) in
            Shell.chdir pwd;
            return
        ) else (
            Shell.chdir pwd;
            return
        )
    )


end


module Address_book = struct
    (* When grown up, this Adbose module is expected to become a 
     * standalone library 
     * *)

    module Adbose = struct
        type field = string list with sexp
        type kind = [ `person | `group | `organisation ] with sexp
        type entry = kind * string * (field list) with sexp

        type address_book = entry list with sexp

        let address_book_of_string str = 
            Sexplib.Sexp.of_string ("(" ^ str ^ ")") |> address_book_of_sexp

        let string_of_address_book ab  = (
            let s = sexp_of_address_book ab in 
            (* Sexplib.Sexp.to_string *)
            (SExpr.to_string_hum ~indent:4 s)
        )

        let get_one field_name (k, i, f) =
            Ls.find f ~f:(function
                | f :: _ when f = field_name -> true
                | _ -> false)
        let get_all field_name (k, i, f) =
            Ls.find_all f ~f:(function
                | f :: _ when f = field_name -> true
                | _ -> false)
        
        let sort_by_family ab = 
            Ls.sort ab ~cmp:(fun e1 e2 ->
                match (get_one "name" e1), (get_one "name" e2) with
                | Some [_; _; l1], Some [_; _; l2] -> Str.compare l1 l2
                | Some [_; l1], Some [_; l2] -> Str.compare l1 l2
                | Some [_; _; l1], Some [_; l2] -> Str.compare l1 l2
                | Some [_; l1], Some [_; _; l2] -> Str.compare l1 l2
                | _, Some [_; _; l2] -> -1
                | _, Some [_; l2] -> -1
                | Some [_; _; l1], _ -> 1
                | Some [_; l1], _ -> 1
                | _, _ -> 0)

    end
    let load str_list = (
        Adbose.address_book_of_string (Str.concat " " str_list)
    )
    let to_brtx abook = (
        let get_needed ((kind, id, fields) as entry) = 
            let kind_str = 
                match kind with
                | `person -> "Pers"
                | `group -> "Grp"
                | `organisation -> "Org" in
            let name_str =
                match Adbose.get_one "name" entry with
                | Some (_ :: f :: l :: _) -> sprintf p"%s, %s" l f
                | Some (_ :: f :: []) -> f
                | _ -> "___NO__VALID_NAME___" in
            (kind_str, id, name_str)
        in
        let header =
            "{header|{title|Address Book}}\n\n" in
        let sections =
            let convert_std = function
                | [_; n] -> (sprintf p"{i|%s}" n)
                | [_; t; n] -> (sprintf p"{b|[%s]} {i|%s}" t n)
                | _ -> "___NOT_A_VALID_STD_FIELD___" in
            let convert_link = function
                | [_; t] -> (sprintf p"{link %s}" t)
                | [_; t; n] -> (sprintf p"{link %s|%s}" t n)
                | [_; t; n; c] -> (sprintf p"{link %s|%s} ({i|%s})" t n c)
                | _ -> "___NOT_A_VALID_LINK_FIELD___" in
            let if_something m f = match m with [] -> "" | l -> f l in
            let make_list field_name convert_entry entry_name entry =
                if_something (Adbose.get_all field_name entry) (fun l -> 
                    let f = convert_entry in
                    (sprintf p"\n{b|%s:}{list|\n{*} %s\n}\n"
                        entry_name
                        (Str.concat "\n{*}" (Ls.map l ~f)))) in

            Ls.map (Adbose.sort_by_family abook) ~f:(fun entry ->
                let kstr, id, name = get_needed entry in
                let phones =
                    make_list "phone" convert_std "Phone numbers" entry in
                let addresses =
                    make_list "address" convert_std "Addresses" entry in
                let emails =
                    make_list "email" convert_std "E-Mails" entry in
                let links =
                    make_list "link" convert_link "Links" entry in
                let tags =
                    if_something (Adbose.get_all "tags" entry) (fun l ->
                        (sprintf p"{b|Tags}: %s{br}\n"
                            (Str.concat ", " (Ls.tl (Ls.hd l))))) in
                let comments =
                    if_something (Adbose.get_all "comments" entry) (fun l ->
                        (sprintf p"{b|Comments}:{br}\n%s{br}\n"
                            (Str.concat "{br}\n" (Ls.tl (Ls.hd l))))) in

                (sprintf p"{section 1 %s|%s (%s)}%s%s%s%s%s%s"
                    id name kstr phones addresses emails links
                    tags comments)) in
        (header ^ (Str.concat "\n\n" sections))
    )


end
