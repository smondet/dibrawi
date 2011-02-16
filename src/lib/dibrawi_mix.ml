

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
    val lazy_map_output : html:(unit -> 'a) -> latex:(unit -> 'a) -> 'a
    val map_output : html:'a -> latex:'a -> 'a
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

    let lazy_map_output ~html ~latex =
      match !global_output with
      | Out_html -> html ()
      | Out_latex -> latex ()

    let map_output ~html ~latex =
      match !global_output with
      | Out_html -> html
      | Out_latex -> latex

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
        ?table_caption_after str =
      let doc = false in
      let url_hook =
        Dibrawi.Special_paths.rewrite_url ~from:[ "SomeCamlMix" ] in
      let brtx = if do_prepro then prepro str else str in
      let res, errors =
        Params.map_output
          ~html:(Bracetax.Transform.str_to_html ?separate_header ~doc ~url_hook brtx)
          ~latex:(Bracetax.Transform.str_to_latex
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
        ~html:("{bypass endbypass}<a id=\"@{id}\"></a>{endbypass}")
        ~latex:("{bypass endbypass}\\phantomsection\\label{@{id}}{endbypass}")

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
  end 


  module Table_of_contents = struct
      
    let make document_for_toc =
      Params.map_output
        ~html:(sprintf  "{bypass endbypass}\n\
                    <hr/>
                    <b>Table of Contents:</b>\n\
                    %s\n\
                    <hr/>\n\
                    {endbypass}"
            (Dibrawi.Brtx_transform.html_toc document_for_toc))
        ~latex:("{bypass}\n\\vspace{1em}\\hrule\n\
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


  module Math = struct

    type style = Latex | Text | MathML

    let style_pref = ref None
    let style () =
      match !style_pref with
      | None ->
        Params.map_output ~latex:Latex ~html:MathML;
      | Some p -> p

    let set_style s =
      style_pref := Some s

    let either a b c =
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
          ~html:(either 
                   (sprintf "{t|{text endformula}\n%s\n{endformula}}")
                   (sprintf "{t|{text endformula}%s{endformula}}")
                   (sprintf
                      "{bypass endbypass}\n\
                        <math display='inline'>%s</math>\n{endbypass}"))
          ~latex:(either 
                    (sprintf "{t|{text endformula}\n%s\n{endformula}}")
                    (sprintf "{bypass endbypass}$%s${endbypass}")
                    (sprintf "{t|{text endformula}\n<math display='inline'>\
                              %s</math>\n{endformula}}"))      in
      !Camlmix_input.printer (around (to_string `inline f))

    let block f =
      let around =
        Params.map_output
          ~html:(either 
                   (sprintf "{code endformula}\n%s\n{endformula}")
                   (sprintf "{code endformula}%s{endformula}")
                   (sprintf
                      "{bypass endbypass}\n\
                        <math display='block'>%s</math>\n{endbypass}"))
          ~latex:(either 
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
          ~html:(either 
                   (sprintf "{code endformula}\n%s\n{endformula}")
                   (sprintf "{code endformula}%s{endformula}")
                   (sprintf
                      "{bypass endbypass}%s{endbypass}"))
          ~latex:(either 
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


  module Alt_fig = struct

    let default_svg_inkscape_path = ref "/NOT_SET"

    let dot_fig ?(label="") ?(size="100%") ?(caption="") ?dest_dir contents =
      let out_format = Params.map_output ~latex:"pdf" ~html:"png" in
      let tmpfile =
        Filename.temp_file "notesgraphviz" (sprintf ".%s" out_format) in
      let cmd = sprintf "dot -Kdot -T%s -o%s" out_format tmpfile in
      let output = System.feed ~cmd ~input:contents in
    (* !Camlmix_input.printer  *)
      Opt.may (fun d ->
        System.run_command
          (sprintf "mkdir -p %s; cp %s %s/" d tmpfile d)) dest_dir;
      let path =
        match dest_dir with
        | None -> tmpfile
        | Some d -> d ^ "/" ^ (Filename.basename tmpfile) in
      Some (sprintf "{ignore}Output of 'dot':\n%s\n{end}\n\
                {image %s %s %s|%s}" output path size label caption)
  (* Camlmix_input.print_with ignore *)

    let inkscape_fig ?(label="") ?(size="100%") ?(caption="") ?dest_dir fullname =
      let svg = fullname ^ ".svg" in
      if Sys.file_exists svg then (
        let outcmd, outname = 
          Params.map_output
            ~latex:("inkscape -z -A", fullname ^ ".pdf")
            ~html:("inkscape -z -e", fullname ^ ".png") in
        if not (Sys.file_exists outname) || System.is_newer svg outname then (
          System.run_command 
            (sprintf "%s %s %s"  outcmd outname svg);
          Opt.may (fun d ->
            System.run_command
              (sprintf "mkdir -p %s; cp %s %s/" d outname d)) dest_dir;
        ) else (
          printf "The .svg is older than %s\n" outname;
        );
        let path =
          match dest_dir with
          | None -> outname
          | Some d -> d ^ "/" ^ (Filename.basename outname) in
        Some (sprintf "{image %s %s %s|%s}" path size label caption)
      ) else (
        printf "File %s does not exist.\n" svg;
        None
      )

    let svg_layers file ?heightno layers = 
      
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
      let outfile =
        sprintf "%s_%s" file (String.concat "-" (Ls.map string_of_int layers)) in
      save (filter layers (load (file ^ ".svg"))) (outfile ^ ".svg");
      outfile

    let ascii_art_fig ?(label="") ?(size="100%") ?(caption="") contents =
      let start id =
        Params.map_output
        ~html:(sprintf "{bypass endbypass}<div class=\"figure\" id=\"%s\" >\
                        <div style=\"text-align: left;\">{endbypass}" id)
        ~latex:(sprintf "{bypass endbypass}\\begin{figure}[htbp]\n\
                         \\begin{minipage}{\\columnwidth}{endbypass}\n")
      in
      let stop id caption =
        Params.map_output
          ~html:(sprintf "{bypass endbypass}</div>%s</div>{endbypass}" 
                   (Dbw.brtx caption))
          ~latex:(sprintf "{bypass endbypass}\\caption{%s}\\label{%s}\n\
                           \\end{minipage}\\end{figure}{endbypass}"
                    (Dbw.brtx caption) id)
      in
      Some ((start label) ^ contents ^ (stop label caption))

    type figure =
      | Dot of string
      | Inkscape of string
      | Layered_inkscape of string * (int option) * (int list)
      | Ascii_art 

    let ascii_art = Ascii_art

    let inkscape ?height ?layers ?path name =
      let full_name =
        match path with
        | None -> !default_svg_inkscape_path ^ name
        | Some p -> p ^ name in
      match layers with
      | Some l -> Layered_inkscape (full_name, height, l)
      | None -> Inkscape full_name

    let dot contents = Dot contents

    let start ?label ?size ?caption ?dest_dir what =
      let saved_printer = !Camlmix_input.printer in
      Camlmix_input.printer :=
        (fun s -> 
          let o = 
            match what with
            | Inkscape name -> inkscape_fig ?label ?size ?caption ?dest_dir name
            | Layered_inkscape (name, heightno, layers) ->
              if Sys.file_exists (name ^ ".svg") then
                inkscape_fig ?label ?size ?caption ?dest_dir
                  (svg_layers name ?heightno layers)
              else (
                printf "Layer source does not exist: %s\n" (name ^ ".svg");
                None
              )
            | Dot content ->
              dot_fig ?label ?size ?caption ?dest_dir content
            | Ascii_art ->
              ascii_art_fig  ?label ?size ?caption s
          in
          begin match o with
          | Some image -> saved_printer image;
          | None -> saved_printer s;
          end;
          Camlmix_input.printer := saved_printer)

    let stop = ()

  end



  module Code = struct

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
      let content = 
        (Pcre.substitute 
           ~pat:"(\\{code\\}|\\{ignore\\}|\\{text\\}|\\{end\\})\\s+"
           ~subst:(fun s -> "") code) in
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
            ~html:(html_figure ?id ?caption language s)
            ~latex:(latex_figure ?id ?caption ?latex_options language s)
          in
          saved_printer to_print;
          Camlmix_input.printer := saved_printer)

    let stop = ()
  end

  module LaTeX = struct

    let make_letter ~src ~dest ?date ?sign ?opening ?closing content =
      Str.concat "\n\n" [
        sprintf "\\address{%s}" src;
        Opt.map_default (fun s -> sprintf "\\signature{%s}" s) "% No sig" sign;
        Opt.map_default (fun s -> sprintf "\\date{%s}" s) "% No sig" date;
        sprintf "\\begin{letter}{%s}\n" dest;
        Opt.map_default (fun s -> sprintf "\\opening{%s}" s) "% No open" opening;
        content;
      (* sprintf "\\closing{%s}" (Opt.default "CLOSING" closing); *)
        "\\end{letter}\n";
      ]
    let make_french_letter
        ~src ~dest ?(cut_rule=false)
        ?phone ?from_city ?subject
        ?date ?sign ?opening ?closing content =
      Str.concat "\n\n" [
        sprintf "\\begin{letter}{%s}\n" dest;
        "\\nofax\n";
        Opt.map_default (fun s -> sprintf "\\telephone{%s}" s) "\\notelephone" phone;
      (* "\\name{Seb}\n"; *)
        Opt.map_default (fun s -> sprintf "\\lieu{%s}" s) "\\nolieu" from_city;
        sprintf "\\address{%s}" src;
        Opt.map_default (fun s -> sprintf "\\conc{%s}" s) "% No conc" subject;
        Opt.map_default (fun s -> sprintf "\\signature{%s}" s) "% No sig" sign;
        Opt.map_default (fun s -> sprintf "\\date{%s}" s) "% No sig" date;
        if cut_rule then "" else "\\NoRule\n";
        Opt.map_default (fun s -> sprintf "\\opening{%s}" s) "% No open" opening;
        content;
        sprintf "\\closing{%s}" (Opt.default "CLOSING" closing);
        "\\end{letter}\n";
      ]
        
    let build ?(with_bibtex=false) path = 
      let pwd = Sys.getcwd () in
      let cd = Filename.dirname path in
      Sys.chdir cd;
      let pdflatex = 
        "pdflatex -interaction=nonstopmode" in
      let target = Filename.chop_extension (Filename.basename path) in
      let run_command c =
        match Unix.system c with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED n ->
          printf "PDF: Compilation of %s failed with error code: %d\n\
                see %s/%s.log\n" target n cd target;
          failwith "pdflatex"
        | _ ->
          printf "PDF: Compilation of %s got killed (?)\n\
                see %s/%s.log\n" target cd target;
          failwith "pdflatex"
      in
      let commands =
        let latex = (sprintf "%s %s > /dev/null" pdflatex target) in
        if with_bibtex then
          [ latex; (sprintf "bibtex %s > /dev/null"  target); latex; latex ]
        else
          [ latex; latex ] in
      begin 
        try
          List.iter run_command commands
        with _ -> ()
      end;
      Sys.chdir pwd;
      (sprintf "%s/%s.pdf" cd target)

    let build_string ?(with_bibtex=false) str =
      let name = "/tmp/buildpdfstring.tex" in
      let o = open_out name in
      output_string o str;
      close_out o;
      build ~with_bibtex name

    let make_full_file 
        ?(pdf_title="") ?(pdf_authors="")  ?(pdf_subject="")
        ~latex_template
        ?(add_document_env=true)
        ?bibtex_style ?(bibtex_path="") content =
      let tmpl_regexp = Pcre.regexp "[A-Z]+_TEMPLATE_[A-Z]+" in
      let document = [
        if add_document_env then "\\begin{document}" else "";
        (if bibtex_style =@= None then
            "\\renewcommand\\cite[1]{[\\hyperref[#1]{#1}]}"
         else "");
        content;
        (match bibtex_style with None -> "" | Some s -> 
          sprintf 
            "\\bibliographystyle{%s}\n\\bibliography{%s}"
            s bibtex_path);
        if add_document_env then "\\end{document}" else "";
      ] in
      (Pcre.substitute ~rex:tmpl_regexp
         ~subst:(function
           | "PDF_TEMPLATE_TITLE" -> pdf_title
           | "PDF_TEMPLATE_AUTHORS" -> pdf_authors
           | "PDF_TEMPLATE_SUBJECT" -> pdf_subject
           | s -> s) 
         latex_template) ^ (Str.concat "\n" document)
        

  end



end


