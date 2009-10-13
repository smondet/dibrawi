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
    (* When grown up, this module is expected to become a 
     * standalone library 
     * *)

    module Adbose = struct
        (*
        type tel =
            [ `mobile | `home | `work | `other of string ] * string
        type address =
            [ `home | `work | `other of string ] * string
        type field = [
            | `name of string * string
            | `tels of tel list
            | `addresses of address list
            | `emails of string list
            | `links of (string * string) list
        ]
        *)
        (*type phone_type = [
            `mobile | `work | `home | `other of string
        ] with sexp*)
        (* type phone_type = string with sexp *)
        (* type address_type = string with sexp *)
        (*
        type field = [
            | `names of (string * string * string) list
            | `phones of (string * string) list
            | `addresses of (string * string) list
            | `emails of (string * string) list
            | `links of (string * string) list
            | `dates of (string * string) list
            | `roles of (string * string) list
            (* | `roles of string  *)
            (* | `birthday of string *)
            | `affiliation of string
            | `comments of string
            | `tags of string list
        ] with sexp*)
        (* type field = field_type * ((string * string) list) *)
        type field = string * string * string with sexp
        type kind = [ `person | `group | `organisation ] with sexp
        type entry =
            kind * string * string * string * (field list)
        with sexp

        type address_book = entry list with sexp

        let address_book_of_string str = 
            Sexplib.Sexp.of_string ("(" ^ str ^ ")") |> address_book_of_sexp
        let string_of_address_book ab  =
            let s = sexp_of_address_book ab in 
            (* Sexplib.Sexp.to_string *)
            SExpr.to_string_hum ~indent:4 s
    end

    let test () = (
        printf p"Test Adbose\n";

        let smt = "
            (person seb Sebastien Mondet (
                ;(names (
                (std-name Sebastien  Mondet)
                (passport-name \"Sebastien Frédéric\" \"Mondet\")
                (titled-name \"Dr Sebastien\" Mondet)
                (spanish-name \"Sebastian\" \"Mondet Yañez\")
                (phone mobile +4790231969)
                (date birthday 1982-03-20)
                (date added \"Tue, 13 Oct 2009 17:36:21 +0200\")
                (email work smondet@ifi.uio.no)
                (email official seb@mondet.org)
            ))" in
        let uio = "
            (organisation uio university \"University of Oslo\" (
                (name complete \"University of Oslo, Norway\")
            ))" in
        let a4x = "
            (group a4x devteam \"Team of A4X potential developpers\" (
                (link head seb)
                (link member fred)
                (link member ion)
                (comment private \"This project is dead...\")
            ))" in
        let abs = (Str.concat "\n" [smt;uio;a4x]) in

        printf p"%s\n" abs;
        let ab = Adbose.address_book_of_string abs in
        printf p"\n\n%s\n" (Adbose.string_of_address_book ab);
    )
    (* let () = test () *)

end
