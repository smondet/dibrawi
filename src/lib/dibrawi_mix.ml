

module type CAMLMIX =
  sig
    val source : string ref
    val line : int ref
    val char : int ref  
    val printer : (string -> unit) ref 
    val print_with : (string -> unit) -> unit
    val print_if : bool -> unit
  end

module Make (Camlmix_input: CAMLMIX) = struct

  include Dibrawi_std

  let pr s = !Camlmix_input.printer s
  let print_if = Camlmix_input.print_if
  let end_if = ()

  module Params: sig
    type output
    (* val global_output : output ref *)
    val set_output :
      [ `LaTeX | `PDF | `html | `latex | `pdf ] -> unit
    val dibrawi_output : unit -> [ `html | `pdf ]
    val map_output : html:(unit -> 'a) -> latex:(unit -> 'a) -> 'a
    val citations : string list ref
    val args : string list
  end = struct
    type output =
      | Out_html
      | Out_latex

    let global_output = ref Out_html

    let set_output = function
      | `html -> global_output := Out_html
      | `latex | `LaTeX | `PDF | `pdf -> global_output := Out_latex
        
    let dibrawi_output () = 
      match !global_output with
      | Out_html -> `html
      | Out_latex -> `pdf

    let map_output ~html ~latex =
      match !global_output with
      | Out_html -> html ()
      | Out_latex -> latex ()

    let citations = ref []
    let args = 
      Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))

  end

  module Dbw: sig
    val prepro : ?mix_output:[ `camlmix | `wiki ] -> string -> string
    val brtx :
      ?do_prepro:bool ->
      ?separate_header:(string * string * string) ref ->
      ?table_caption_after:bool ->
      ?make_section_links:[ `never | `when_labeled | `always ] ->
      string -> string

  end = struct

    let prepro =
      let module DP = Dibrawi.Preprocessor in
      let output = Params.dibrawi_output () in
      let todo_list = None in
      DP.brtx2brtx
        ?todo_list
        ~html_cite:(DP.default_html_cite "")
        ~output ~from:["CamlMixedDocument"]

    let brtx ?(do_prepro=false) ?separate_header 
        ?table_caption_after 
        ?(make_section_links=`always)
        str =
      let doc = false in
      let url_hook =
        Dibrawi.Special_paths.rewrite_url ~from:[ "SomeCamlMix" ] in
      let brtx = if do_prepro then prepro str else str in
      let res, errors =
        Params.map_output
          ~html:(fun () ->
            Bracetax.Transform.str_to_html
              ~make_section_links ?separate_header ~doc ~url_hook brtx)
          ~latex:(fun () ->
            Bracetax.Transform.str_to_latex
              ?table_caption_after
              ?separate_header ~doc ~url_hook brtx)
      in
      match errors with
      | [] -> res
      | l ->
        let f = function 
          | `message m -> Bracetax.Error.to_string m | `undefined s -> s in
        failwith ("brtx compilation errors: " ^ (Str.concat "\n" (Ls.map ~f l)))

  end

  module System = Dibrawi_system

  module Alt = struct
    let start text =
      !Camlmix_input.printer text;
      Camlmix_input.print_with ignore
    let stop = ()
    let b = start
    let e = stop
  end



  module Recorder = struct

    open ExtHashtbl
    let records = Hashtbl.create 1
    let current_recorder = ref None

    let record_to name =
      begin match !current_recorder with
      | None ->
        let first_printer = !Camlmix_input.printer in
        let buf = Buffer.create 1000 in
        Camlmix_input.printer := Buffer.add_string buf; 
        current_recorder := Some (first_printer, name, buf);
      | Some (printer, previous_name, buf) ->
        Hashtbl.add records previous_name (Buffer.contents buf);
        Buffer.clear buf;
        current_recorder := Some (printer, name, buf);
      end

    let stop () =
      begin match !current_recorder with
      | None -> ()
      | Some (printer, previous_name, buf) ->
        Camlmix_input.printer := printer; 
        Hashtbl.add records previous_name (Buffer.contents buf);
        current_recorder := None;
      end

    let get ?(separator=" ") name =
      match Hashtbl.find_all records name with
      | [] -> failwith (sprintf "Recorder: Did not find record: %s" name)
      | l -> String.concat separator (List.rev l)

    let all_record_names () =
      Ls.of_enum (Hashtbl.keys records)
    
    let current () = !current_recorder
    let record_opt = function None -> () | Some (_, s, _) -> record_to s
  end 


  module Table_of_contents = struct
      
    let make document_for_toc =
      Params.map_output
        ~html:(fun ()->
          sprintf  "{bypass endbypass}\n\
                    <hr/>
                    <b>Table of Contents:</b>\n\
                    %s\n\
                    <hr/>\n\
                    {endbypass}"
            (Dibrawi.Brtx_transform.html_toc 
               (Dibrawi.Preprocessor.brtx2brtx document_for_toc)))
        ~latex:(fun () ->
          "{bypass}\n\\vspace{1em}\\hrule\n\
                \\tableofcontents\n\
                \\vspace{1em}\\hrule\n\n{end}")

  end
  module Biblio_report = struct

    let load_biblio_file path =
      Dibrawi.Bibliography.load [(Io.read_all (Io.open_in path))]

    let do_sections biblio pattern request_title_assoc =
      let buf = Buffer.create 42 in
      Ls.iter request_title_assoc ~f:(fun (req, open_sec, close_sec) ->
        Buffer.add_string buf open_sec;
        let subset = Sebib.Request.exec req biblio in
        let str = Sebib.Format.str ~pattern subset in
        Buffer.add_string buf str;
        Buffer.add_string buf close_sec);
      Buffer.contents buf

  end
  module Sebibrtx = struct
    let url_pdf_doi = 
     "@{if (or (has url) (has pdfurl) (has doi))} \
      [@{if (has url)}{t|{link @{url}|URL}}\
      @{if (or (has pdfurl) (has doi))}, @{endif}@{endif}\
      @{if (has pdfurl)}{t|{link @{pdfurl}|PDF}}\
      @{if (has doi)}, @{endif}@{endif}\
      @{if (has doi)}{t|{link @{doi}|DOI}}@{endif}]@{endif}"

    let label output =
      Params.map_output
        ~html:(fun () ->
          "{bypass endbypass}<a id=\"@{id}\"></a>{endbypass}")
        ~latex:(fun () ->
          "{bypass endbypass}\\phantomsection\\label{@{id}}{endbypass}")

  end

  module Local_bibliography = struct

    let make ?pattern biblio =
      let local_cites = (ref [] : (int * string) list ref) in
      let count = ref 0 in
      let meta_cite f ref =
        match Ls.find_opt !local_cites ~f:(fun (i, s) -> s = ref) with
        | Some (i, s) -> f i
        | None -> 
          incr count;
          local_cites := (!count, ref) :: !local_cites;
          f !count
      in
      let cite = fun r -> meta_cite (sprintf "[%d]") r |> pr in
      let cites refs =
        sprintf "[%s]" (Str.concat "," (Ls.map (meta_cite string_of_int) refs)) |> pr in
      let print () =
        let pattern_fun = 
          (match pattern with Some f -> f | None ->
            sprintf "[%d] @{authors}; {i|@{title-punct}} @{how}, @{year}. {br}") in
        Ls.iter (Ls.rev !local_cites) ~f:(fun (i, s) ->
          let subset = Sebib.Request.exec (`ids [s]) biblio in
          if Ls.length subset <> 1 then
            eprintf "Warning: subset has %d elements\n" (Ls.length subset);
          let str = Sebib.Format.str ~pattern:(pattern_fun i) subset in
          pr str) in
      (cite, cites, print)

  end

  module Math = struct

    type style = Latex | Text | MathML

    let style_pref = ref None
    let style () =
      match !style_pref with
      | None ->
        Params.map_output ~latex:(fun () -> Latex) ~html:(fun () -> MathML);
      | Some p -> p

    let set_style s =
      style_pref := Some s

    let either a b c =
      match style () with
      | Text -> a
      | Latex -> b
      | MathML -> c

    let either_fun a b c () =
      match style () with
      | Text -> a
      | Latex -> b
      | MathML -> c

  (* http://tlt.its.psu.edu/suggestions/international/bylanguage/mathchart.html *)
  (* http://xahlee.org/emacs/emacs_n_unicode.html
     -> Logic & set theory
  *)
  (* would be todo: 
     - more operators (Logic & set theory)
     - unary operators (minus, not, overbar, vector) 
     - fractions
     - common sets R, N, Q
     - set comprehensions
     - products i=0..n
     - some greek and hebrew letters (?)
     - layout management: 
     - matrix
     - optional \n's

  *)
    type binop =
      | BO_add
      | BO_sub
      | BO_mul
      | BO_div
      | BO_mod
      | BO_eq
      | BO_ne
      | BO_le
      | BO_ge
      | BO_lt
      | BO_gt
      | BO_imply
      | BO_and
      | BO_or
      | BO_equivalent
    (*   | BO_forall
         | BO_exists
         | BO_element
         | BO_not_element *)
      | BO_custom of string * string * string

    type formula =
      | Sum of formula * formula * formula * formula
      | Paren of formula
      | Binop of binop * formula * formula
      | Variable of string
      | Literal of string
      | Apply of formula * formula list
      | Custom of string * string * string
      | Sup of formula * formula
      | Sub of formula * formula
      | Nil

    module Constructors = struct
      let sum i f t s = Sum (i, f, t, s)
      let var s = Variable s
      let lit s = Literal s
      let int s = Literal (string_of_int s)
      let float s = Literal (string_of_float s)
      let (+) a b = Binop (BO_add, a, b)
      let ( * ) a b = Binop (BO_mul, a, b)
      let (-) a b = Binop (BO_sub, a, b)
      let (/) a b = Binop (BO_div, a, b)
      let (mod) a b = Binop (BO_mod, a, b)
      let bin t l m a b = Binop (BO_custom (t, l, m), a, b)
      let par f = Paren f
      let app f l = Apply (f, l)
      let (==) a b = Binop (BO_eq , a, b)
      let (!=) a b = Binop (BO_ne , a, b)
      let (<=) a b = Binop (BO_le , a, b)
      let (>=) a b = Binop (BO_ge , a, b)
      let (< ) a b = Binop (BO_lt , a, b)
      let (> ) a b = Binop (BO_gt , a, b)
      let (=>  ) a b = Binop (BO_imply     , a, b)
      let (&&  ) a b = Binop (BO_and       , a, b)
      let (||  ) a b = Binop (BO_or        , a, b)
      let (<=> ) a b = Binop (BO_equivalent, a, b)
      let custom t l m = Custom (t, l, m)
      let sup a b = Sup (a, b)
      let sub a b = Sub (a, b)

      let nilbin f = f Nil Nil
      let nil = Nil 

      let ( << ) x y = y x 
      and ( >> ) x y = x y 

    end
    let binop_str = function
      | BO_add -> "+"
      | BO_sub -> "-"
      | BO_mul -> either "⋅" "\\cdot" "⋅"
      | BO_div -> "/" (* "÷" "" "÷" *)
      | BO_mod -> "mod"
      | BO_eq  -> either "==" "=" "="
      | BO_ne  -> either "!=" "\\neq" "≠"
      | BO_le  -> either "<=" "\\le" "≤"
      | BO_ge  -> either ">=" "\\ge" "≥"
      | BO_lt  -> either "<" "<" "<"
      | BO_gt  -> either ">" "<" ">"
      | BO_imply       -> either "=>" "\\Rightarrow" "⇒"
      | BO_and         -> either "/\\" "\\wedge" "∧"
      | BO_or          -> either "\\/" "\\vee" "∨"
      | BO_equivalent  -> either "<=>" "\\Leftrightarrow" "⇔"
      | BO_custom (t, l, m) -> either t l m


    let rec to_string style f =
      match f with
      | Sum (i, f, t, s) ->
        either 
          (sprintf "Sum[%s=%s..%s] %s" (to_string style i) (to_string style f)
             (to_string style t) (to_string style s))
          (sprintf "\\sum_{%s=%s}^{%s}{%s}" (to_string style i) (to_string style f)
             (to_string style t) (to_string style s))
          (sprintf "\
            <munderover>\n\
              <mrow><mo>&#x2211;</mo></mrow>\n\
              <mrow>%s<mo>=</mo>%s</mrow>\n\
              <mrow>%s</mrow></munderover><mrow>%s</mrow>\n\
            "
             (to_string style i) (to_string style f) 
             (to_string style t) (to_string style s))
      | Paren f -> 
        either 
          (sprintf "(%s)" (to_string style f))
          (sprintf "\n(%s)" (to_string style f))
          (sprintf "\n<mo>(</mo>%s<mo>)</mo>\n" (to_string style f))
      | Binop (op, a, b) ->
        let sop = binop_str op and sa =  to_string style a and sb = to_string style b in
        either 
          (sprintf "%s %s %s" sa sop sb)
          (sprintf "%s %s %s" sa sop sb)
          (sprintf "%s<mo>%s</mo>%s" sa sop sb)
      | Variable s -> either s s (sprintf "<mi>%s</mi>" s)
      | Literal s -> either s s (sprintf "<mn>%s</mn>" s)
      | Apply (f, l) ->
        let sf = to_string style f and 
            sl = List.map (to_string style) l in
        either 
          (sprintf "%s(%s)" sf (String.concat ", " sl))
          (sprintf "\n%s(%s)" sf (String.concat ", " sl))
          (sprintf "\n%s<mo>(</mo>%s<mo>)</mo>\n" sf (String.concat "<mo>,</mo>" sl))
      | Custom (t,l,m) -> either t l (sprintf "<mi>%s</mi>" m)
      | Sup (a, b) ->
        let sa = to_string style a and sb = to_string style b in
        either 
          (sprintf "%s^{%s}" sa sb)
          (sprintf "%s^{%s}" sa sb)
          (sprintf "\n<msup>\n  <mrow>%s</mrow>\n  <mrow>%s</mrow>\n</msup>" sa sb)
      | Sub (a, b) ->
        let sa = to_string style a and sb = to_string style b in
        either 
          (sprintf "%s_{%s}" sa sb)
          (sprintf "%s_{%s}" sa sb)
          (sprintf "\n<msub>\n  <mrow>%s</mrow>\n  <mrow>%s</mrow>\n</msub>" sa sb)
      | Nil -> ""

    let inline f =
      let around =
        Params.map_output
          ~html:(either_fun 
                   (sprintf "{t|{text endformula}\n%s\n{endformula}}")
                   (sprintf "{t|{text endformula}%s{endformula}}")
                   (sprintf
                      "{bypass endbypass}\n\
                        <math display='inline'>%s</math>\n{endbypass}"))
          ~latex:(either_fun 
                    (sprintf "{t|{text endformula}\n%s\n{endformula}}")
                    (sprintf "{bypass endbypass}$%s${endbypass}")
                    (sprintf "{t|{text endformula}\n<math display='inline'>\
                              %s</math>\n{endformula}}"))      in
      !Camlmix_input.printer (around (to_string `inline f))

    let block f =
      let around =
        Params.map_output
          ~html:(either_fun 
                   (sprintf "{code endformula}\n%s\n{endformula}")
                   (sprintf "{code endformula}%s{endformula}")
                   (sprintf
                      "{bypass endbypass}\n\
                        <math display='block'>%s</math>\n{endbypass}"))
          ~latex:(either_fun 
                    (sprintf "{code endformula}\n%s\n{endformula}")
                    (sprintf "{bypass endbypass}$$%s$${endbypass}")
                    (sprintf "{code endformula}\n<math display='block'>\
                              %s</math>\n{endformula}}"))      in
      !Camlmix_input.printer (around (to_string `block f))

    let array_mathml transform a =
      let transformed =
        List.map (fun row -> 
          sprintf "<mtr>%s</mtr>\n"
            (String.concat ""
               (List.map (fun f -> 
                 let s = transform f in
                 sprintf "<mtd>%s</mtd>\n" s) row))) a in
      sprintf "<math display='block'><mtable>%s</mtable></math>"
        (String.concat "" transformed)

    let array_latex transform a =
      let transformed =
        List.map (fun row -> 
          (String.concat " & " (List.map transform row))) a in
      sprintf "\\begin{eqnarray*}%s\n\\end{eqnarray*}"
        (String.concat "\\\\ " transformed)

    let array_text transform a =
      let col_maxes = Array.create (Ls.length (Ls.hd a)) 0 in
      let transformed =
        Ls.map (fun row ->
          Ls.mapi (fun i cell ->
            let s = transform cell in 
            let l = String.length s in
            if l > col_maxes.(i) then col_maxes.(i) <- l;
            s
          ) row) a in
      let padded =
        Ls.map (fun row ->
          String.concat " " 
            (Ls.mapi (fun i cell ->
              cell ^ (String.make (col_maxes.(i) - (Str.length cell)) ' ')) row)
        ) transformed in
      sprintf "\n%s\n" (String.concat "\n" padded)


    let array a =
      let around =
        Params.map_output
          ~html:(either_fun 
                   (sprintf "{code endformula}\n%s\n{endformula}")
                   (sprintf "{code endformula}%s{endformula}")
                   (sprintf
                      "{bypass endbypass}%s{endbypass}"))
          ~latex:(either_fun 
                    (sprintf "{code endformula}\n%s\n{endformula}")
                    (sprintf "{bypass endbypass}%s{endbypass}")
                    (sprintf "{code endformula}%s{endformula}}"))      in
      !Camlmix_input.printer
        (either
           (around (array_text   (to_string `inline) a))
           (around (array_latex  (to_string `inline) a))
           (around (array_mathml (to_string `inline) a)))

    include Constructors

  end


  module Fig = struct

    let default_svg_inkscape_path = ref "/NOT_SET"

    let hash_fun = Hashtbl.hash

    let dot
        ?(label="") ?(size="100%") ?(caption="") 
        ?(basename="dbwgraphvizdot") ?(dest_dir="/tmp/") ?link_dir
        contents =
      let hash = hash_fun [basename; contents] in
      let out_format = 
        Params.map_output ~latex:(fun () -> "pdf") ~html:(fun () -> "png")
      in
      let tmpfile =
        sprintf "%s/%s%d.%s" dest_dir basename hash out_format in
      let cmd = sprintf "dot -Kdot -T%s -o%s" out_format tmpfile in
      let output = System.feed ~cmd ~input:contents in
      let path =
        match link_dir with
        | None -> tmpfile
        | Some d -> d ^ "/" ^ (Filename.basename tmpfile) in
      (sprintf "{ignore}Output of 'dot':\n%s\n{end}\n\
                          {image %s %s %s|%s}" output path size label caption)

    let start_ascii_art ?(label="") ~caption =
      let saved_printer = !Camlmix_input.printer in
      Camlmix_input.printer :=
        (fun contents -> 
          let start id =
            Params.map_output
              ~html:(fun () -> 
                sprintf "{bypass endbypass}<div class=\"figure\" id=\"%s\" >\
                        <div style=\"text-align: left;\">{endbypass}" id)
              ~latex:(fun () ->
                sprintf "{bypass endbypass}\\begin{figure}[htbp]\n\
                         \\begin{minipage}{\\columnwidth}{endbypass}\n")
          in
          let stop id caption =
            Params.map_output
              ~html:(fun () ->
                sprintf "{bypass endbypass}</div>%s</div>{endbypass}" 
                  (Dbw.brtx caption))
              ~latex:(fun () ->
                sprintf "{bypass endbypass}\\caption{%s\\label{%s}}\n\
                           \\end{minipage}\\end{figure}{endbypass}"
                  (Dbw.brtx caption)
                  (Bracetax.LatexPrinter.sanitize_nontext id))
          in
          saved_printer ((start label) ^ contents ^ (stop label caption));
          Camlmix_input.printer := saved_printer)
    let stop = ()
        

    let svg_layers srcpath dstpath file ?heightno layers = 
      
      let load filename = 
        let xml_channel = open_in filename in
        let xml = Xml.parse_in xml_channel in
        close_in xml_channel;
        xml
      in
      let save xml filename = 
        let _channel = open_out filename in
        Printf.fprintf _channel "%s" (Xml.to_string xml);
        close_out _channel;
      in
      let is_an_unwanted_layer layers atts =
        List.exists (fun (a, b) -> 
          if a =$= "id" then
            if (String.sub b 0 5 =$= "layer") then
              (List.for_all (fun l -> b <$> sprintf "layer%d" l) layers)
            else
              false
          else
            false
        ) atts in
      let contains_alt_height hno atts =
        match hno with None -> None | Some h ->
          Ls.find_opt
            (fun (a, b) -> a =$= (sprintf "inkscape:altheight%d" h)) atts
      in
      let change_height hs atts =
        Ls.map atts ~f:(function ("height", b) -> ("height", hs) | c -> c) in
      let rec filter layers xml = 
        match xml with
        | Xml.PCData s as x -> x
        | Xml.Element ("svg", attributes, children) ->
          begin match (contains_alt_height heightno attributes) with
          | Some (_, h) ->
            Xml.Element ("svg", change_height h attributes,
                         List.map (filter layers) children)
          | None ->
            Xml.Element ("svg", attributes, List.map (filter layers) children)
          end
        | Xml.Element (tagname, attributes, children) ->
          if tagname =$= "g" && (is_an_unwanted_layer layers attributes) then
            Xml.Element ("g", [],  []) 
          else
            Xml.Element (tagname, attributes, List.map (filter layers) children)
      in
      let infile = sprintf "%s/%s" srcpath file in
      let outfile =
        sprintf "%s/%s_%s" dstpath file
          (String.concat "-" (Ls.map string_of_int layers)) in
      save (filter layers (load (infile ^ ".svg"))) (outfile ^ ".svg");
      (Filename.basename outfile)

    type svg_transformation = [
    | `select_layers of int list
    | `select_layers_and_altheight_nb of int list * int
    ]

    let inkscape_svg 
        ?(label="") ?(size="100%") ?(caption="")
        ?(tmp_dir="/tmp/") ?(dest_dir="/tmp/") ?link_dir ?path
        ?(transform:svg_transformation option) filename =
      (* If SVGZ then decompress *)
      let the_path, the_file =
        let actual_path = Opt.default !default_svg_inkscape_path path in
        let chopped = Filename.chop_extension filename in
        if Filename.check_suffix filename ".svgz" then
          let cmd = 
            sprintf "gunzip -c %s/%s > %s/%s.svg" 
              actual_path filename tmp_dir chopped in
          System.run_command cmd;
          (tmp_dir, chopped)
        else
          (actual_path, chopped) in
      (* Do transformations *)
      let the_path, the_file = 
        match transform with
        | Some (`select_layers_and_altheight_nb (layers, heightno)) ->
          (tmp_dir, svg_layers the_path tmp_dir the_file ~heightno layers)
        | Some (`select_layers layers) ->
          (tmp_dir, svg_layers the_path tmp_dir the_file layers)
        | None ->
          (the_path, the_file) in
          (* Do the compilation *)
      let outcmd, outname, outfmt = 
        let dest = sprintf "%s/%s" dest_dir the_file in
        Params.map_output
          ~latex:(fun () -> "inkscape -z -A", dest, ".pdf")
          ~html:(fun () -> "inkscape -z -e",  dest, ".png") in
      let svg = sprintf "%s/%s.svg" the_path the_file in
      if not (Sys.file_exists outname) || System.is_newer svg outname then (
        System.run_command
          (sprintf "%s %s%s %s > /dev/null"  outcmd outname outfmt svg);
      ) else (
        eprintf "Alt_fig.Inkscape: The .svg is older than %s%s\n"
          outname outfmt;
      );
      let link = 
        sprintf "%s/%s%s" (Opt.default dest_dir link_dir) the_file outfmt in
      (sprintf "{image %s %s %s|%s}" link size label caption)
      

  end



  module Code = struct


    let get_code s =
      try
        let first = Str.find s "{code}" + 6 in
        let last = Str.find s "{end}" in
        Str.slice ~first ~last s
      with _ -> failwith "Mix.Code.get_code failed to get the code"


    let lstlisting_language = function
      | `OCaml | `ocaml | `caml -> "Caml" (* "[Objective]Caml" *)
      | `C | `c -> "C"

    let latex_figure ?id ?(caption="") ?latex_options language code =
    (*    let start id =
          sprintf "\\begin{figure}[htbp]\n\
          \\begin{minipage}{\\columnwidth}\n"
          in
          let stop caption id =
          sprintf "\\caption{%s}\\label{%s}\n\
          \\end{minipage}\\end{figure}"
          (Dbw.brtx caption) id
          in*)
      let content = get_code code in
      sprintf 
        "{bypass endbypass42}\
       \\begin{lstlisting}[%slanguage=%s%s]\
       %s\n\
       \\end{lstlisting}\n\
       {endbypass42}\n"
        (Opt.map_default (sprintf "%s,") "" latex_options)
        (lstlisting_language language)
        (Opt.map_default (sprintf ",caption={%s},label=%s" (Dbw.brtx caption))
           "" id)
        content
        

    let html_figure ?id ?(caption="") language code =
      let start id =
        sprintf "{bypass endbypass}<div class=\"figure\" id=\"%s\" >\
        <div style=\"text-align: left;\">{endbypass}" id
      in
      let stop caption id =
        sprintf "{bypass endbypass}</div>%s</div>{endbypass}" 
          (Dbw.brtx caption)
      in
    (*    let content = 
          (Pcre.substitute 
          ~pat:"(\\{code\\}|\\{ignore\\}|\\{text\\}|\\{end\\})\\s+"
          ~subst:(fun s -> "") code) in *)
      sprintf 
        "%s\n%s\n%s\n"
        (Opt.map_default start "" id)
        code
        (Opt.map_default (stop caption) "" id)


    let start ?id ?caption ?latex_options language =
      let saved_printer = !Camlmix_input.printer in
      Camlmix_input.printer :=
        (fun s ->
          let to_print =
            Params.map_output
            ~html:(fun () ->
              html_figure ?id ?caption language s)
            ~latex:(fun () -> 
              latex_figure ?id ?caption ?latex_options language s)
          in
          saved_printer to_print;
          Camlmix_input.printer := saved_printer)

    let stop = ()
  end

  module List_menu = struct

    let make entries 
        ?(current_entry= (fun n l -> sprintf "{b|%s}" n))
        ?(other_entries= (fun n l -> sprintf "{link %s|%s}" l n)) =
      let instance key =
        "{begin list}\n {*} " ^
        (Str.concat "\n {*} "
           (Ls.map entries ~f:(fun (k, n, l) -> 
            if k = key then current_entry n l else other_entries n l))) ^
          "\n{end}\n"
      in
      instance

  end


end


