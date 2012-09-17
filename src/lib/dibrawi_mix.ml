

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
  module System = Dibrawi_system
  module Xelatex = Dibrawi_xelatex

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
        failwith ("brtx compilation errors: " ^ (String.concat ~sep:"\n"
                                                   (List.map ~f l)))
          
  end

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
      | l -> String.concat ~sep:separator (List.rev l)

    let all_record_names () = Hashtbl.keys records
    
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
      In_channel.with_file path ~f:(fun i ->
        Dibrawi.Bibliography.load [In_channel.input_all i]
      )

    let do_sections biblio pattern request_title_assoc =
      let buf = Buffer.create 42 in
      List.iter request_title_assoc ~f:(fun (req, open_sec, close_sec) ->
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
      let local_cites = (ref [] : (int * string * string option) list ref) in
      let count = ref 0 in
      let meta_cite ?id f ref =
        match List.find !local_cites ~f:(fun (_, s, _) -> s = ref) with
        | Some (i, s, None) -> f (string_of_int i)
        | Some (i, s, Some id) -> f id
        | None -> 
          incr count;
          local_cites := (!count, ref, id) :: !local_cites;
          match id with None -> f (string_of_int !count) | Some s -> f s
      in
      let cite ?(silent=false) ?id r =
        meta_cite ?id (sprintf "[%s]") r |! (if not silent then pr else ignore)
      in
      let cites refs =
        sprintf "[%s]"
          (String.concat ~sep:", "
             (List.map ~f:(meta_cite (fun s -> s)) refs)) |! pr in
      let print () =
        let pattern_fun = 
          (match pattern with Some f -> f | None ->
            sprintf "[%s] @{authors}; {i|@{title-punct}} @{how}, @{year}. {br}") in
        List.iter (List.rev !local_cites) ~f:(fun (i, s, id) ->
          let subset = Sebib.Request.exec (`ids [s]) biblio in
          if List.length subset <> 1 then
            eprintf "Warning: subset has %d elements\n" (List.length subset);
          let str =
            let idstr = match id with None -> string_of_int i | Some s -> s in
            Sebib.Format.str ~pattern:(pattern_fun idstr) subset in
          pr str;
        ) in
      (cite, cites, print)

  end
  
  module Math = struct
    module Formula = Dibrawi_formula

    include Formula.Library
    include Formula.Constructors
    (* include Formula.Render *)
  
    let latex = Formula.Latex
    let text = Formula.Text
    let mathml = Formula.MathML
  
    let bypass = sprintf "{bypass endmathbypass}%s{endmathbypass}"
    let ttext = sprintf "{t|{text endmathtext}%s{endmathtext}}"
    let code = sprintf "{code endmathcode}%s{endmathcode}"
  
    let style_pref = ref None
    let style () =
      match !style_pref with
      | None ->
        Params.map_output ~latex:(fun () -> latex) ~html:(fun () -> mathml);
      | Some p -> p
        
    let set_style s =
      style_pref := Some s
  
    let either_fun a b c () =
      match style () with
      | Formula.Text -> a
      | Formula.Latex -> b
      | Formula.MathML -> c
  
    let inline f =
      Params.map_output (Formula.Render.inline (style ()) f)
        ~html:(either_fun ttext ttext bypass)
        ~latex:(either_fun ttext bypass ttext)
  
    let block f =
      Params.map_output (Formula.Render.block (style ()) f)
        ~html:(either_fun code code bypass)
        ~latex:(either_fun code bypass code)
  
    let array f =
      Params.map_output (Formula.Render.array (style ()) f)
        ~html:(either_fun code code bypass)
        ~latex:(either_fun code bypass code)
  
    let with_MathJax () =
      if Params.dibrawi_output () = `html then
        pr "{bypass}\
        <script type=\"text/javascript\" \
         src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?\
        config=TeX-AMS-MML_HTMLorMML\"></script>{end}"
      else ()
  
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
        List.exists ~f:(fun (a, b) ->
          if a =$= "id" then
            if (String.sub b 0 5 =$= "layer") then
              List.for_all ~f:(fun l -> b <$> sprintf "layer%d" l) layers
            else
              false
          else
            false
        ) atts in
      let contains_alt_height hno atts =
        Option.bind hno (fun h ->
          List.find
            ~f:(fun (a, b) -> a =$= (sprintf "inkscape:altheight%d" h)) atts)
      in
      let change_height hs atts =
        List.map atts ~f:(function ("height", b) -> ("height", hs) | c -> c) in
      let rec filter layers xml = 
        match xml with
        | Xml.PCData s as x -> x
        | Xml.Element (tagname, attributes, children)
            when tagname =$= "svg" || tagname =$= "svg:svg" ->
          begin match (contains_alt_height heightno attributes) with
          | Some (_, h) ->
            Xml.Element (tagname, change_height h attributes,
                         List.map ~f:(filter layers) children)
          | None ->
            Xml.Element (tagname, attributes, List.map ~f:(filter layers) children)
          end
        | Xml.Element (tagname, attributes, children) ->
          if tagname =$= "g" || tagname =$= "svg:g" then
            if (is_an_unwanted_layer layers attributes) then
              Xml.Element ("g", [],  [])
            else 
              Xml.Element (tagname, attributes, 
                           List.map ~f:(filter layers) children)
          else
            Xml.Element (tagname, attributes, 
                         List.map ~f:(filter layers) children)
      in
      let infile = sprintf "%s/%s" srcpath file in
      let outfile =
        sprintf "%s/%s_%s" dstpath file
          (String.concat ~sep:"-" (List.map ~f:string_of_int layers)) in
      save (filter layers (load (infile ^ ".svg"))) (outfile ^ ".svg");
      (Filename.basename outfile)

    type svg_transformation = [
    | `select_layers of int list
    | `select_layers_and_altheight_nb of int list * int
    ]

    let inkscape_svg_file
        ?(tmp_dir="/tmp/") ?(dest_dir="/tmp/") ?link_dir ?path
        ?(transform:svg_transformation option) filename =
      (* If SVGZ then decompress *)
      let the_path, the_file =
        let actual_path =
          Option.value ~default:!default_svg_inkscape_path path in
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
      if not (Sys.file_exists outname = `Yes) || System.is_newer svg outname
      then (
        System.run_command
          (sprintf "%s %s%s %s > /dev/null"  outcmd outname outfmt svg);
      ) else (
        eprintf "Alt_fig.Inkscape: The .svg is older than %s%s\n"
          outname outfmt;
      );
      let link = 
        sprintf "%s/%s%s" (Option.value ~default:dest_dir link_dir)
          the_file outfmt in
      link
      
    let inkscape_svg 
        ?(label="") ?(size="100%") ?(caption="")
        ?(tmp_dir="/tmp/") ?(dest_dir="/tmp/") ?link_dir ?path
        ?(transform:svg_transformation option) filename =
      let link = 
        inkscape_svg_file
          ~tmp_dir ~dest_dir ?link_dir ?path ?transform filename in
      (sprintf "{image %s %s %s|%s}" link size label caption)
        
  end



  module Code = struct


    let get_code s =
      let open Option in
      More_string.find_index_from_left s ~token:"{code}"
      >>= fun index ->
      let first = index + 6 in
      More_string.find_index_from_left s ~token:"{end}"
      >>= fun after_last ->
      return (String.slice s first after_last)


    let lstlisting_language = function
      | `OCaml | `ocaml | `caml -> "Caml" (* "[Objective]Caml" *)
      | `C | `c -> "C"
      | `none -> "NONE"
    let sourcehighlight_language = function
      | `OCaml | `ocaml | `caml -> "caml" (* "[Objective]Caml" *)
      | `C | `c -> "c"
      | `none -> "NONE"
       

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
        get_code code |! Option.value_exn_message "could not get the code" in
      sprintf 
        "{bypass endbypass42}\
       \\begin{lstlisting}[%slanguage=%s%s]\
       %s\n\
       \\end{lstlisting}\n\
       {endbypass42}\n"
        (Option.value_map ~f:(sprintf "%s,") ~default:"" latex_options)
        (lstlisting_language language)
        (Option.value_map ~default:"" id
           ~f:(sprintf ",caption={%s},label=%s" (Dbw.brtx caption)))
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
      let the_code =
        match language with
        | `none -> code
        | _ ->
          let input =
            get_code code |! Option.value_exn_message "cannot get {code}" in
          let cmd = 
            sprintf "source-highlight -s %s -f xhtml"
              (sourcehighlight_language language) in
          try
            "{bypass endofthecodehtml}" ^ (System.feed ~cmd ~input)
            ^ "{endofthecodehtml}"
          with
            e -> code
      in
      sprintf 
        "%s\n%s\n%s\n"
        (Option.value_map ~f:start ~default:"" id)
        the_code
        (Option.value_map ~f:(stop caption) ~default:"" id)
        
        
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
        (String.concat ~sep:"\n {*} "
           (List.map entries ~f:(fun (k, n, l) -> 
             if k = key then current_entry n l else other_entries n l))) ^
          "\n{end}\n"
      in
      instance

  end

  module Blog :
  sig
    type post = {
      title : string;
      date : string;
      tags : string list;
      key : string;
      description: string option;
    }
    class blog :
    object
      method all_tags : string list
      method end_post : unit
      method get_posts : [ `all | `tag of string ] -> post list
      method new_post :
        title:string ->
        tags:string list -> date:string -> ?description:string ->
        string -> unit
      method post_contents : string -> string
    end

    val rss :
      title:string ->
      description:string ->
      link:string ->
      last_build_date:string ->
      pub_date:string ->
      describe:(post -> string) ->
      make_link:(post -> string) -> post list -> string
      
    val disqus :
      short_name:string -> discus_id:string -> url:string -> string
  
  end = struct
      
    type post = {
      title : string;
      date : string;
      tags : string list;
      key : string;
      description: string option;
    }
    class blog = object (self)
      val mutable blog_posts_stack = []
      method new_post ~title ~tags ~date ?description key =
        blog_posts_stack <- 
          {title; date; tags; key; description} :: blog_posts_stack;
        Recorder.record_to key
      method end_post = Recorder.stop ()
      method get_posts (what:[`all | `tag of string]) =
        let rec f = function
          | `all -> List.rev blog_posts_stack
          | `tag s ->
            List.filter (f `all)
              ~f:(fun post -> List.exists ~f:((=) s) post.tags) in
        (f what)
      method all_tags =
        List.dedup (List.concat
                       (List.map ~f:(fun i -> i.tags) (self#get_posts `all)))
      method post_contents key = Recorder.get key
    end

    let rss 
        ~title ~description ~link ~last_build_date ~pub_date
        ~describe ~make_link pl =
      let head =
        sprintf 
          "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n\
          <rss version=\"2.0\">\n\
          <channel>\n\
          \    <title>%s</title>\n\
          \    <description>%s</description>\n\
          \    <link>%s</link>\n\
          \    <lastBuildDate>%s</lastBuildDate>\n\
          \    <pubDate>%s</pubDate>\n"
          title description link last_build_date pub_date in
      let foot = "</channel>\n</rss>\n" in
      let items =
        let f = 
          fun item ->
            let {title; date} = item in
            sprintf
              "    <item>\n\
             \         <title>%s</title>\n\
             \         <description>%s</description>\n\
             \         <link>%s</link>\n\
             \         <guid>%s</guid>\n\
             \         <pubDate>%s</pubDate>\n\
             \     </item>\n" title (describe item)
              (make_link item)  (make_link item) date 
        in 
        String.concat ~sep:"\n\n" (List.map ~f pl) in
      (head ^ items ^ foot)

    let disqus ~short_name ~discus_id ~url =
      sprintf
        "<div id=\"disqus_thread\"></div>\n\
        \  <script type=\"text/javascript\">\n\
        \      var disqus_shortname = '%s'; \n\
        \      // required: replace example with your forum shortname\n\
        \  \n\
        \      // The following are highly recommended additional\n\
        \      // parameters. Remove the slashes in front to use.\n\
        \      var disqus_identifier = '%s';\n\
        \      var disqus_url = '%s';\n\
        \  \n\
        \      /* * * DON'T EDIT BELOW THIS LINE * * */\n\
        \      (function() {\n\
        \          var dsq = document.createElement('script');\n\
        \          dsq.type = 'text/javascript'; dsq.async = true;\n\
        \          dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';\n\
        \          (document.getElementsByTagName('head')[0] ||\n\
        \             document.getElementsByTagName('body')[0]).appendChild(dsq);\n\
        \      })();\n\
        \  </script>\n\
        \  <noscript>Please enable JavaScript to view the\n\
        \  <a href=\"http://disqus.com/?ref_noscript\">comments powered\n\
        \  by Disqus.</a></noscript>\n\
        \  <a href=\"http://disqus.com\" class=\"dsq-brlink\">blog\n\
        \  comments powered by <span class=\"logo-disqus\">Disqus</span></a>"
        short_name discus_id url


  end

end

