val make_letter :
  src:string -> dest:string -> ?date:string ->
  ?sign:string -> ?opening:string -> ?closing:string -> 
  string -> string
val make_french_letter :
  src:string -> dest:string ->
  ?cut_rule:bool -> ?phone:string -> ?from_city:string ->
  ?subject:string -> ?date:string -> ?sign:string ->
  ?opening:string -> ?closing:string -> 
  string -> string

exception Build_error of string

type print_error_style = 
  [ `no | `simple of out_channel | `complex of out_channel ]

val build : 
  ?with_bibtex:bool -> ?raises:bool -> ?print_errors:print_error_style -> 
  string -> string

val do_clean : string -> unit
val build_string :
  ?with_bibtex:bool -> ?raises:bool -> ?print_errors:print_error_style -> 
  string -> string

val build_string_tree :
  ?with_bibtex:bool -> ?raises:bool -> ?print_errors:print_error_style -> 
  Dibrawi_std.String_tree.t -> string

val make_full_file :
  ?pdf_title:string -> ?pdf_authors:string -> ?pdf_subject:string ->
  latex_template:Dibrawi_std.String_tree.t ->
  ?add_document_env:bool ->
  ?bibtex_style:string -> ?bibtex_path:string -> 
  string -> Dibrawi_std.String_tree.t
  
module Template : sig
  type color_theme = [ `classy | `none ]
  type global_parameters
  type component = global_parameters -> Dibrawi_std.String_tree.t
      
  val compact_title_box : ?with_color:bool -> component
  val change_height : ?pt:int -> component
  val package_geometry :
    ?paper:[< `A0 | `A1 | `A2 | `A3 | `A4 | `A5 | `A6 > `A4 ] ->
    ?raw_options:string -> component
  val paragraphs :
    ?baselinestretch:float -> ?parskip:string -> ?parindent:string -> component
  val small_itemize : component
  val verbatim_style :
    ?font_size:[ `footnote | `small ] -> component
  val package_caption : component
  val compact_sections :
    ?how:[`with_titlesec | `homemade ] -> component
  val tabular_style :
    ?font_size:[ `footnote | `small ] ->
    ?vertical_cell_spacing:float -> component
  val listing_style :
    ?style:string -> component
  val package_fancyhdr : component
  val hyphenations : string list -> component
  val fontspec : component
  val section_numbers : ?section_numbers_depth:int -> ?toc_depth:int -> component
  val make :
    ?add:component list ->
    ?color:color_theme ->
    ?language:string ->
    ?document_class:[ `article of int
                    | `book of int
                    | `french_letter of int
                    | `letter of int
                    | `none ] ->
    ?columns:[ `one | `two ] ->
    ?geometry:component ->
    ?fontspec:component ->
    ?two_sides:bool ->
    unit -> Dibrawi_std.String_tree.t
end
