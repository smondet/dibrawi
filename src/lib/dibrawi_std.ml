
include Print

module Ls = List with Labels, LExceptionless
module En = Enum with Labels, LExceptionless
module Opt = Option with Labels

let pcre_matches rex str = (
    try ignore (Pcre.exec ~rex str); true with Not_found -> false
)

module Str = struct
    include String
    let rev_idx s c =
        try Some (rindex s c) with Not_found -> None


    let nsplit str sep =
        if str = "" then []
        else 
            (* str is non empty *)
            let seplen = length sep in
            let rec aux acc ofs =
                if ofs >= 0 then (
                    match
                    try Some (rfind_from str ofs sep)
                    with Invalid_string -> None
                    with
                    | Some idx -> (* sep found *)
                        let end_of_sep = idx + seplen - 1 in
                        if end_of_sep = ofs (* sep at end of str *)
                        then aux (""::acc) (idx - 1)
                        else
                            let token = sub str (end_of_sep + 1) (ofs - end_of_sep) in
                            aux (token::acc) (idx - 1)
                    | None     -> (* sep NOT found *)
                        (sub str 0 (ofs + 1))::acc
                )
                else
                    (* Negative ofs: the last sep started at the beginning of str *)
                    ""::acc
            in
            aux [] (length str - 1 )
        


end
