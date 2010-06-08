
include Printf
include Dibrawi_Yaboon_PolyComp.CompAndOveridePoly


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
end

module Io = struct
  include IO
  let open_in f =
    let i = Pervasives.open_in f in
    IO.input_channel i
  let open_out f = 
    IO.output_channel (Pervasives.open_out f)
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


end
