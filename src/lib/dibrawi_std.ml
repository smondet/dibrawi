
include Printf
include Dibrawi_Yaboon_PolyComp.CompAndNoPolyPhy
let (=$=) = eqs
let (<$>) = nes
let (=@=) = eqpoly
let (<@>) = nepoly


module Ls = struct
  include ExtList.List
  include ListLabels
  let find_opt ~f l =
    try Some (find ~f l) with Not_found -> None
end


module En = Enum
module Opt = struct
  include Option
  let bind f =
    function Some s -> f s | None -> None
  let may ~f o = may f o
  let may_apply ~f o a = 
    match o with
    | None -> a
    | Some p -> f p a
end

module Io = struct
  include IO
  let open_in f =
    let i = Pervasives.open_in f in
    IO.input_channel i
  let open_out f = 
    IO.output_channel (Pervasives.open_out f)
  let stdout = output_channel stdout

  let with_file_out filename f = 
    let o = open_out filename in
    try let r = f o in close_out o; r with e -> close_out o; raise e

  let with_file_in filename f = 
    let i = open_in filename in
    try let r = f i in close_in i; r with e -> close_in i; raise e

  let with_new_tmp ?(suffix=".tmp") ?(prefix="dibrawi_") f =
    let name, o = Filename.open_temp_file prefix suffix in
    let o = output_channel o in
    try let r = f o name in close_out o; r with e -> close_out o; raise e
end

module Ht = struct
  include ExtHashtbl.Hashtbl
  let find_opt ht key =
    try Some (find ht key) with Not_found -> None
end

let pcre_matches rex str = 
  try ignore (Pcre.exec ~rex str); true with Not_found -> false


let (|>) = fun a b -> b a

module Str = struct
  include ExtString.String
  let rev_idx s c =
    try Some (rindex s c) with Not_found -> None
  let head str len =
    sub str 0 len
  let tail str pos =
    sub str pos (length str - pos)

  let replace_all str ~sub ~by =
    let len = length str in
    let sublen = length sub in
    if sublen = 0 || len < sublen then str else
      let buf = Buffer.create len in
      (* let found = ref [] in *)
      let found = ref false in
      let i = ref 0 in
      while !i <= len - sublen do
        let j = ref 0 in
        found := false;
        while !j < sublen && get str (!i + !j) = get sub !j do
          incr j;
          if !j = sublen then (
            (* found := !i :: !found; *)
            found := true;
            j := sublen;
          );
        done;
        if !found then (
          Buffer.add_string buf by;
          i := !i + sublen;
        ) else (
          Buffer.add_char buf str.[!i];
          incr i;
        );
      done;
      Buffer.add_substring buf str !i (sublen - 1);
      (Buffer.contents buf)
          
end

module String_tree = struct
  type t = 
    | Str of string
    | Cat of t list
    | Empty        

  let str s = Str s
  let rec cat_sep sep =
    function
      | [] -> Empty
      | [one] -> one
      | one :: [Empty] -> one
      | Empty :: t ->
        cat_sep sep t
      | h :: Empty :: t ->
        Cat [h; sep; cat_sep sep t ]
      | h :: t ->
        Cat [h; sep; cat_sep sep t ]

  let cat ?sep l =
    match sep with
    | None -> Cat l
    | Some s -> cat_sep s l

  let new_line = Str "\n"
  let str_cat l = cat (Ls.map str l)
  let empty = Empty


  let rec print ?(out=Io.stdout) = function
    | Str s -> Io.nwrite out s
    | Cat l -> Ls.iter (print ~out) l
    | Empty -> ()

  let to_string st =
    let rec output out = function
      | Str s -> Io.nwrite out s
      | Cat l -> Ls.iter (output out) l
      | Empty -> ()
    in
    let o = Io.output_string () in
    output o st;
    Io.close_out o

end

module Substitute = struct

  type t = (string * string) list ref

  let create l = ref l
  let add t sa sb = t := (sa, sb) :: !t

  let copy t = ref !t

  let string t str =
    let escaped = Ls.map !t ~f:(fun (a,b) -> Pcre.quote a) in
    let rex = Pcre.regexp_or escaped in
    Pcre.substitute ~rex ~subst:(fun s ->
      match Ls.find_opt !t ~f:(fun (a, b) -> a = s) with
      | Some (a, b) -> b
      | None -> s) str

end


