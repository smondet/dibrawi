
include Print

module Ls = List with Labels, LExceptionless
module En = Enum with Labels, LExceptionless
module Opt = Option with Labels

let pcre_matches rex str = (
    try ignore (Pcre.exec ~rex str); true with Not_found -> false
)
