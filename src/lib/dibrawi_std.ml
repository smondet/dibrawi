
include Core.Std
  
include Dibrawi_Yaboon_PolyComp.CompAndNoPolyPhy
let (=$=) = eqs
let (<$>) = nes
let (=@=) = eqpoly
let (<@>) = nepoly

let pcre_matches rex str = 
  try ignore (Pcre.exec ~rex str); true with Not_found -> false

(*
module Str = struct
  include ExtString.String
  let rev_idx s c =
    try Some (rindex s c) with Not_found -> None
  let head str len =
    sub str 0 len
  let tail str pos =
    sub str pos (length str - pos)

  let find_opt a b = try Some (find a b) with e -> None
  let split_opt s i = try Some (split s i) with e -> None

          
end
*)
module More_string = struct
  let replace_all str ~sub ~by =
    let len = String.length str in
    let sublen = String.length sub in
    if sublen = 0 || len < sublen then str else
      let buf = Buffer.create len in
    (* let found = ref [] in *)
      let found = ref false in
      let i = ref 0 in
      while !i <= len - sublen do
        let j = ref 0 in
        found := false;
        while !j < sublen &&  str.[(!i + !j)] =  sub.[!j] do
          incr j;
          if !j = sublen
          then (
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
        
  let compare_with_sub str ~buffer ~offset =
    let length = String.length str in
    let buffer_length = String.length buffer in
    let rec loop index =
      (* printf "index %d length %d, buffer_length %d  \n" index length buffer_length; *)
      if index = length
      then true
      else if offset + index >= buffer_length
      then false
      else if Char.equal str.[index] buffer.[offset + index]
      then loop (index + 1)
      else false
    in
    loop 0
      
  let find_index_from_left str ~token =
    let token_length = String.length token in
    let rec loop current_index =
      (* printf "current_index: %d token_length: %d\n" current_index token_length; *)
      if current_index >= String.length str - token_length + 1
      then None
      else if compare_with_sub token ~buffer:str ~offset:current_index
      then Some current_index
      else loop (current_index + 1)
    in
    loop 0
  
  let split_once_at_string str ~token =
    let token_length = String.length token in
    Option.map (find_index_from_left str ~token) (fun idx ->
      String.(slice str 0 idx,
              sub str (idx + token_length) (length str - token_length - idx)))
    
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
  let str_cat l = cat (List.map ~f:str l)
  let empty = Empty


  let rec print ?(out=stdout) = function
    | Str s -> Out_channel.output_string out s
    | Cat l -> List.iter l (print ~out)
    | Empty -> ()

  let to_string st =
    let buf = Buffer.create 42 in
    let rec output = function
      | Str s -> Buffer.add_string buf s
      | Cat l -> List.iter l output
      | Empty -> ()
    in
    output st;
    Buffer.contents buf

end

module Substitute = struct

  type t = (string * string) list ref

  let create l = ref l
  let add t sa sb = t := (sa, sb) :: !t

  let copy t = ref !t

  let string t str =
    let escaped = List.map !t ~f:(fun (a,b) -> Pcre.quote a) in
    let rex = Pcre.regexp_or escaped in
    Pcre.substitute ~rex ~subst:(fun s ->
      match List.find !t ~f:(fun (a, b) -> a = s) with
      | Some (a, b) -> b
      | None -> s) str

end

(** Simple paths which are lists of strings (c.f. Batteries). *)
module Path = struct

  type t = string list
  type path = t

  let str : path -> string = String.concat ~sep:"/"

end
