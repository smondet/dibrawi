open Dibrawi_std
open String_tree
let ($) f x = f x
let opt_str_map f o = Opt.map_default (fun s -> str (f s)) empty o

let opt_fill_css param opt =
  match opt with
  | None -> empty
  | Some value ->
    str $ sprintf "   %s: %s;" param value

let css_block ?class_opt name l =
  cat [
    Opt.map_default (fun s -> str (s ^ " ")) empty class_opt;
    str name; str " {\n"; cat ~sep:new_line l; str "\n}"]



module Color = struct
  type color = string
  type text_color =
    | One of (string * color option * color option) 
    | List of (string list * color option * color option)
  type theme = {
    text: text_color list;
    borders: (string * color) list;
  }

  let color ?bg ?fg who = One (who, fg, bg)
  let color_list ?bg ?fg whol = List (whol, fg, bg)
  let fg who c = One (who, Some c, None)
  let bg who c = One (who, None, Some c)
  let theme ?(text=[]) ?(borders=[]) () =
    {text = text; borders = borders;}

  let empty_theme = theme ()
  let dummy_theme = 
    theme ()
      ~text:[
        color "body"     ~fg:"#113311" ~bg:"#ccccff";
        color "a:link"    ~fg:"#000044";
        color "a:visited" ~fg:"#440000";
        color "a:hover"  ~fg:"#00eeee" ~bg:"#0000ee"; 
        color_list ["tt"; "pre"; "code"]  ~fg:"#331133";
        fg ".dbwmixcode" "#dd0000";
        color_list ["div.figure:after"; "caption.tablefigure:after"] ~fg:"#00cc00";
      ]
      ~borders:[
        ("stdframe", "#0000bb");
        ("headerframe", "#900");
        ("blockquotebar", "#00c");
        ("h2border", "#0f0");
      ]

  let install_theme ?for_class theme =
    let color x = opt_fill_css "color" x in
    let background x = opt_fill_css "background-color" x in
    let block = css_block ?class_opt:for_class in
    cat ~sep:new_line 
      (Ls.map theme.text ~f:(function
        | One (p, f, b) -> block p [ color f; background b]
        | List (pl, f, b) ->
          cat (Ls.map pl ~f:(fun p -> block p [ color f; background b]))))

  let border theme name =
    Opt.map snd (Ls.find_opt theme.borders ~f:(fun (n, c) -> n = name))

  let greenish_main_theme, greenish_secondary_theme =
    let whitish = "#f8f8f8" in
    let blackish = "#070707" in
    let light_blue = "#66BBF4" in
    let faded_blue = "#6A96B3" in
    let light_green = "#308721" in
    let dark_green = "#07330A" in
    let brownish = "#6F3F27" in
    let brownish_light = "#EFDFC7" in
    let highlight_light = brownish_light in
    let highlight_dark = brownish in
    let highlight_slightly = "#ddd" in
    let main = 
      theme ()
        ~text:[
          color "body" ~fg:blackish ~bg:whitish;
          color "" ~fg:blackish ~bg:whitish;
          color "a:link"    ~fg:light_green;
          color "a:visited" ~fg:dark_green;
          color "a:hover"  ~bg:highlight_light; 
          color_list ["tt"; "pre"; "code"]   ~fg:brownish;
          bg ".dbwmixcode" highlight_slightly;
          color_list ["div.figure:after"; "caption.tablefigure:after"] 
            ~fg:light_green;

        ]
        ~borders:[
          ("stdframe",      faded_blue);
          ("headerframe",   dark_green);
          ("blockquotebar", dark_green);
          ("h2border",      dark_green);
        ]
    in
    let second =
      theme ()
        ~text:[
          color "body" ~fg:whitish ~bg:dark_green;
          color "" ~fg:whitish ~bg:dark_green;
          color "a:link"    ~fg:light_blue;
          color "a:visited" ~fg:faded_blue;
          color "a:hover"   ~bg:highlight_dark; 
          color_list ["tt"; "pre"; "code"]  ~fg:brownish_light;
          bg ".dbwmixcode" highlight_slightly;
          color_list ["div.figure:after"; "caption.tablefigure:after"] 
            ~fg:light_blue;

        ]
        ~borders:[
        ]
    in
    (main, second)

  let sober_redish_theme =
    let whitish = "#f8f8f8" in
    let blackish = "#070707" in
    let light_red = "#D62111" in
    let dark_red = "#8F1310" in
    let highlight_green = "#44E157" in
    let highlight_slightly = "#ddd" in
    let dark_green = "#135510" in
    let main = 
      theme ()
        ~text:[
          color "body" ~fg:blackish ~bg:whitish;
          color "" ~fg:blackish ~bg:whitish;
          color "a:link"    ~fg:light_red;
          color "a:visited" ~fg:dark_red;
          color "a:hover"  ~bg:highlight_green; 
          color_list ["tt"; "pre"; "code"] ~fg:dark_green;
          bg ".dbwmixcode" highlight_slightly;
          color_list ["div.figure:after"; "caption.tablefigure:after"] ~fg:light_red;

        ]
        ~borders:[
          ("stdframe",      blackish);
          ("headerframe",   dark_red);
          ("blockquotebar", dark_red);
          ("h2border",      dark_red);
        ]
    in
    main

end


module Font = struct

  type spec = {
    family: string option;
    align: string option;
    size: string option;
    variant: string option;
    style: string option;
    decoration: string option;
  }
  type theme = (string * spec) list

  let spec ?family ?align ?size ?variant ?style ?decoration () = {
    family = family; align = align; size = size; variant = variant;
    style = style; decoration = decoration;
  }

  let associate what spec = (what, spec)

  let specify  ?family ?align ?size ?variant ?style ?decoration what =
    associate what (spec  ?family ?align ?size ?variant ?style ?decoration ())

  let empty_theme = []
  let dummy_theme = [
    specify "h1" ~variant:"small-caps" ~size:"250%";
    specify "body" ~align:"justify" ~size:"80%";
    specify ".emph" ~style:"italic";
    specify "a" ~decoration:"underline thin";
  ]

  let install_theme ?for_class theme =
    let block = css_block ?class_opt:for_class in
    cat ~sep:new_line 
      (Ls.map theme ~f:(fun (w, s) -> block w [
        opt_fill_css "font-family" s.family;
        opt_fill_css "text-align" s.align;
        opt_fill_css "font-size" s.size;
        opt_fill_css "font-variant" s.variant;
        opt_fill_css "font-style" s.style;
        opt_fill_css "text-decoration" s.decoration;
      ]))

  let standardish_theme size family align = [
    specify "body" ~size ~family ~align;
    specify "" ~size ~family ~align;
    specify "div.header" ~align:"center";
    specify "h1" ~size:"200%" ~variant:"small-caps";
    specify "div.authors" ~size:"110%";
    specify "div.subtitle" ~size:"110%" ~style:"italic";
    specify "h2" ~size:"180%";
    specify "h3" ~size:"150%";
    specify "h4" ~size:"130%";
    specify "h5" ~size:"108%";
    specify "a" ~decoration:"underline thin";
    specify "tt,code,pre" ~family:"monospace";
    specify "div.figure,div.tablefigure" ~size:"90%";
  ]



end


    

type css_global_parameters = {
  color_theme: Color.theme;
}

let install_color_theme
    ?for_class ?(theme:Color.theme option) params =
  let theme = Opt.default params.color_theme theme in
  Color.install_theme ?for_class theme

let install_font_theme ?for_class theme params =
  Font.install_theme ?for_class theme

let paragraph_style ?(debug=false) ?(separate="0.5em") params =
  cat ~sep:new_line [
    str "div.p {";
    str "    padding-bottom: 0em;";
    if debug then 
      str "/* The debug border: */ border: thin #0e0 solid;"
    else 
      empty;
    str "}";
    str $ sprintf "div.p + div.p { padding-top: %s; }" separate;
  ]

let header_block ?(frame="5px") params =
  let frame_color = 
    Opt.default "black" (Color.border params.color_theme "headerframe") in
  str $ sprintf
    "div.header {\n\
    \    text-align: center;\n\
    \    border:  %s solid %s;\n\
    \    padding-top:    2.1em;\n\
    \    padding-bottom: 2.05em;\n\
    \    margin-bottom: 3em;\n\
    }" frame_color frame

let enable_scrolling params =
  str "body {overflow: auto;}\ncode,pre {overflow: auto}"

let blockquote ?(style=`left_bar) params =
  match style with
  | `left_bar ->
    str $ sprintf 
      "blockquote {\n\
      \    border-left: 2px solid %s;\n\
      \    padding-left: 1em;\n\
      }" (Opt.default "grey" (Color.border params.color_theme "blockquotebar"))

let list_geometry ?(style=`compact "2em") ?(debug=false) params =
  match style with
  | `compact indent ->
    str $ sprintf 
      "ul, ol {\n\
      \    padding-top: 0em;\n\
      \    padding-bottom: 0em;\n\
      \    padding-left:  %s;\n\
      \    margin-top: 0em;\n\
      \    margin-bottom: 0em;\n\
      %s\
      }" indent
      (if debug then "/* The debug border: */ border: thin #FF0000 solid;\n"
       else "")

let dibrawi_menu ?(style=`compact "1em") params =
  match style with
  | `compact indent ->
    str $ sprintf 
      ".dibrawimenudir { display: block; }
       ul  li.dibrawi_menuli {
       \  display: inline;
       } 
       ul.dibrawi_menuul {
       \  padding: 0em;
       \  margin: 0em;
       \  margin-left: %s;
       }" indent


let dibrawi_cmt params =
  str
    ".dibrawicomment:before { content:  \"[\"; }\n\
    .dibrawicomment:after { content: \"]\"; }\n\
    .dibrawicomment {\n\
    \    background-color: yellow;\n\
    \    color: red;\n\
    }"

let tables_and_figures params =
  str 
    "table.tablefigure {\n\
    \    margin-right:auto;\n\
    \    margin-left: auto;\n\
    \    border-collapse: collapse;\n\
    \    min-width: 80%;\n\
    \    margin-bottom: 1em;\n\
    }\n\
    caption.tablefigure {\n\
    \    caption-side:bottom;\n\
    }\n\
    div.figure:after, caption.tablefigure:after { content: \" [\" attr(id) \"]\"; }\n\
    td, th {\n\
    \    padding: 0.4em;\n\
    }\n\
    img {\n\
    \    border: none;\n\
    \    padding-bottom: 0.5em;\n\
    }div.figure, div.tablefigure {\n\
    \    text-align: center;\n\
    \    font-size:90%;\n\
    \    border: thin #959595 solid;\n\
    \    padding: 1ex;\n\
    \    margin: 1em;\n\
    \    width: 90%;\n\
    \    margin-left: 5%;\n\
    \    margin-right: 0em;\n\
    \    clear: both;\n\
    \n\
    }"

let footnotes params =
  str 
    "small.notebegin { font-size: 70%; vertical-align: super; \
                       counter-increment: footnote; }\n\
    small.noteend { font-size: 70%; vertical-align: super; }\n\
    small.noteend:before {  content: counter(footnote) }\n\
    small.note:before { content: counter(footnote) \": \" }\n\
    body { counter-reset: section subsection subsubsection footnote; }\n\
    \n\
    small.note {\n\
    \    font-size: 76%;\n\
    \    clear: right;\n\
    \    width: 40%;\n\
    \    border: thin #959595 solid;\n\
    \    float: right;\n\
    \    margin: 0.5em;\n\
    \    margin-right: 0em;\n\
    \    padding: 0.5em;\n\
    }"

let section_numbers params =
  str
    "h1 { counter-reset: section subsection subsubsection footnote; }\n\
    body { counter-reset: section subsection subsubsection footnote; }\n\
    h2:before { content: counter(section) \". \"; }\n\
    h2 {\n\
    \    counter-reset: subsection subsubsection;\n\
    \    counter-increment: section;\n\
    }\n\
    h3:before {\n\
        content: counter(section) \".\" counter(subsection) \". \" ;\n\
    }\n\
    h3 {\n\
    \    counter-increment: subsection;\n\
    \    counter-reset: subsubsection;\n\
    }\n\
    h4:before {\n\
    \    content: counter(section) \".\" counter(subsection) \".\" \
    counter(subsubsection) \". \";\n\
    }\n\
    h4 {\n\
    \    counter-increment: subsubsection;\n\
    }" 

let section_decoration params =
  str $ sprintf
    "h2 { border: %s solid 2px; padding: 0.2em; }\n\
    h3 { text-decoration: underline;}"
    (Opt.default "black" (Color.border params.color_theme "h2border"))

let code_blocks ?(with_border=`dashed) params =
  cat ~sep:new_line [
    str "pre {";
    begin match with_border with
    | `no -> empty
    | `dashed ->
      str "    border: dashed thin black; padding: 1ex;"
    end;
    str "    position: relative; left: 5%; width: 90%; clear: left;";
    str "}";
  ]

let layout kind params =
  match kind with
  | `simple (left, w) ->
    str $ sprintf "body {margin-left: %s; max-width: %s;}" left w
  | `three_columns width ->
    let on_the_left, on_the_right, in_the_middle =
      let pad = 0.9 in
      let margin = 0.4 in
      let l = 
        sprintf "  left:  %f%%; width: %f%%; padding: %f%%;" margin width pad in
      let r =
        sprintf "  right: %f%%; width: %f%%; padding: %f%%;" margin width pad in
      let m = 
        sprintf "  left: %f%%; right: %f%%; padding: %f%%;"
          (margin +. pad +. width +. pad +. margin +. margin)
          (margin +. pad +. width +. pad +. margin +. margin)
          pad in
      (str l, str r, str m) in
    let frame_color = 
      Opt.default "#999" (Color.border params.color_theme "stdframe") in
    let std_frame = str $ sprintf "  border: %s ridge 3px;" frame_color in
    cat ~sep:new_line [
      str "div.leftside {";
      on_the_left;
      std_frame;
      str "    top: 2px;";
      str "    position:fixed;";
      str "    overflow: auto;";
      str "    bottom: 2px;";
      str "}";
      str "div.rightside {";
      on_the_right;
      std_frame;
      str "    top: 2px;";
      str "    position:fixed;";
      str "    overflow: auto;";
      str "    bottom: 2px;";
      str "}";
      str "div.content {";
      in_the_middle;
      std_frame;
      str "    top: 2px;";
      str "    position: absolute;";
      str "    text-align: justify;";
      str "}";
    ]
  | `with_sidepane (which_side, width) ->
    let sidepane_part, content_part =
      let pad = 0.9 in
      let margin = 0.4 in
      match which_side with
      | `left ->
        let l = 
          sprintf "  left:  %f%%; width: %f%%; padding: %f%%;" margin width pad
        in
        let m = 
          sprintf "  left: %f%%; right: %f%%; padding: %f%%;"
            (margin +. pad +. width +. pad +. margin +. margin)
            (margin)
            pad
        in
        (str l, str m)
      | `right ->
        let r =
          sprintf "  right: %f%%; width: %f%%; padding: %f%%;" margin width pad
        in
        let m = 
          sprintf "  left: %f%%; right: %f%%; padding: %f%%;"
            (margin)
            (margin +. pad +. width +. pad +. margin +. margin)
            pad
        in
        (str r, str m)
    in
    let frame_color = 
      Opt.default "#999" (Color.border params.color_theme "stdframe") in
    let std_frame = 
      str $ sprintf "  border-%s: %s ridge 1px;" 
        (match which_side with `left -> "right" | `right -> "left")
        frame_color in
    cat ~sep:new_line [
      str "div.sidepane {";
      sidepane_part;
      std_frame;
      str "    top: 2px;";
      str "    position:fixed;";
      str "    overflow: auto;";
      str "    bottom: 2px;";
      str "}";
      str "div.content {";
      content_part;
      str "    top: 2px;";
      str "    position: absolute;";
      str "    text-align: justify;";
      str "}";
    ]





let css ?(color_theme=Color.empty_theme) ?raw l =
  let params = {
    color_theme = color_theme;
  } in
  let sep = new_line in
  let tree =
    cat ~sep [
      str "<style type=\"text/css\">";
      cat ~sep (Ls.map ~f:(fun f -> f params) l);
      Opt.map_default str empty raw;
      str "</style>";
    ] in
  `inline tree


type html_template =
    ?menu:string -> ?toc:string -> ?title:string -> ?footer:string ->
    ?from:(string list) ->
    string -> String_tree.t

type html_global_parameters = {
  path_to_root: string list;
}


type body_style = html_global_parameters -> html_template
    
let body style params =
  let sep = new_line in
  fun ?menu  ?toc ?title ?footer ?from content ->
    match style with
    | `raw  ->
      cat ~sep [
        str $ sprintf "<div class=\"dibrawicontent\">%s</div>" content;
        opt_str_map (sprintf "<div class=\"dibrawititle\">%s</div>") title;
        opt_str_map (sprintf "<div class=\"dibrawimenu\">%s</div>") menu;
        opt_str_map (sprintf "<div class=\"dibrawitoc\">%s</div>") toc;
        opt_str_map (sprintf "<div class=\"dibrawifooter\">%s</div>") footer;
      ]
    | `simple ->
      cat ~sep [
        str $ sprintf "<div class=\"dibrawicontent\">%s</div>" content;
        opt_str_map (sprintf "<div class=\"dibrawifooter\">%s</div>") footer;
      ]
    | `three_columns top_right ->
      cat ~sep [
        str "<div class=\"leftside\">";
        opt_str_map (sprintf "<b>Page:</b> %s <br/>") title;
        opt_str_map 
          (sprintf "<hr/><b>Menu: </b>%s")
          menu;
        str "</div>";
        str "<div class=\"rightside\">";
        str (top_right from);
        opt_str_map (sprintf "<hr/><b>Table of contents:</b><br/>%s<br/>") toc;
        opt_str_map (sprintf "<hr/>%s<br/>") footer;
        str "</div>";
        str "<div class=\"content\">";
        str content;
        str "<div id=\"pagefoot\" />";
        str "</div>";
      ]
    | `with_sidepane more_info ->
      cat ~sep [
        str "<div class=\"sidepane\">";
        cat ~sep:(str "<hr/>\n") [
          opt_str_map (sprintf "<b>Page:</b> %s <br/>") title;
          str (more_info from);
          opt_str_map (sprintf "<b>Menu: </b>%s") menu;
          opt_str_map (sprintf "<b>Table of contents:</b><br/>%s<br/>") toc;
          opt_str_map (sprintf "%s<br/>") footer;
        ];
        str "</div>";
        str "<div class=\"content\">";
        str content;
        str "<div id=\"pagefoot\" />";
        str "</div>";
      ]



let make
    ?(rss: string option) ?(atom: string option)
    ?(icon:string option) ?(css=`none) ?(body:body_style option) () =

  let insert_css css params =
    match css with
    | `none -> str ""
    | `link target ->
      str $ sprintf
        "<link rel=\"stylesheet\"  type=\"text/css\" href=\"%s\"/>"
        target
    | `link_from_root target ->
      let depth = Ls.length params.path_to_root - 1 in
      str $ sprintf
        "<link rel=\"stylesheet\"  type=\"text/css\" href=\"%s/%s\"/>"
        ("./" ^ (Str.concat "/" (Ls.init depth (fun _ -> ".."))))
        target
    | `inline string_tree -> string_tree
  in
  let template_function  ?menu ?toc ?title ?footer ?(from=[]) content =
    let params = { path_to_root = from } in
    cat ~sep:(str "\n") [
      str "<!DOCTYPE html\n\
        PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
        <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n\
        <!-- Generated with Dibrawi, BraceTax, Sebib, and some more hacks -->\n\
        <head>\n\
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>";
      insert_css css params;
      opt_str_map (sprintf "<link rel=\"shortcut icon\" href=\"%s\">") icon;
      opt_str_map 
        (sprintf "<link rel=\"alternate\" type=\"application/rss+xml\" \
                 title=\"RSS\" href=\"%s\">") rss;
      opt_str_map 
        (sprintf "<link rel=\"alternate\" type=\"application/atom+xml\" \
                 title=\"Atom\" href=\"%s\">") atom;
      opt_str_map (sprintf "<title>%s</title>")  title;
      str "</head>\n<body>";
      begin match body with
      | None -> str "<!-- No Body -->"
      | Some b ->
        (b params) ?menu  ?toc ?title ?footer ~from content
      end;
      str "</body></html>\n";
    ]
  in
  (template_function:html_template)

module Full = struct

  let three_columns_dummy
      ?(debug=false) ?(rss: string option) ?(atom: string option)
      ?(icon:string option)      
      ?(top_right=(fun _ -> "<!-- Right Side Insertion -->")) () =
    make ()
      ?rss ?atom ?icon
      ~css:(css ~color_theme:Color.dummy_theme [
        install_color_theme;
        install_font_theme Font.dummy_theme;
        header_block ~frame:"7px";
        paragraph_style ~separate:"1em" ~debug;
        enable_scrolling;
        blockquote ~style:(`left_bar);
        list_geometry ~style:(`compact "3em") ~debug;
        dibrawi_cmt;
        dibrawi_menu;
        tables_and_figures;
        footnotes;
        section_numbers;
        code_blocks;
        layout (`three_columns 20.);
      ])
      ~body:(body (`three_columns top_right))

  let three_columns_greenish
      ?(add_section_numbers=false)
      ?(debug=false)  ?(rss: string option) ?(atom: string option)
      ?(icon:string option)
      ?(top_right=(fun _ -> "<!-- Right Side Insertion -->")) () =
    make ()
      ?rss ?atom ?icon
      ~css:(css ~color_theme:Color.greenish_main_theme [
        install_color_theme ~for_class:".content" ~theme:Color.greenish_main_theme;
        install_color_theme ~theme:Color.greenish_secondary_theme;
        install_font_theme
          (Font.standardish_theme "70%" "sans-serif" "none");
        install_font_theme ~for_class:".content"
          (Font.standardish_theme "120%" "serif" "justify");
        header_block ~frame:"7px";
        paragraph_style ~separate:"0.5em" ~debug;
        enable_scrolling;
        blockquote ~style:(`left_bar);
        list_geometry ~style:(`compact "1.8em") ~debug;
        dibrawi_menu;
        dibrawi_cmt;
        tables_and_figures;
        footnotes;
        if add_section_numbers then section_numbers else (fun _ -> empty);
        section_decoration;
        code_blocks ~with_border:`no;
        layout (`three_columns 18.);
      ])
      ~body:(body (`three_columns top_right))

  let simple_page_greenish
      ?(add_section_numbers=true) ?(rss: string option) ?(atom: string option)
      ?(icon:string option)
      ?(base_font_size="90%") ?(text_width="40em") ?(left_margin="4em")
      ?(debug=false) () =
    make ()
      ?rss ?atom ?icon
      ~css:(css ~color_theme:Color.greenish_main_theme [
        install_color_theme ~theme:Color.greenish_main_theme;
        install_font_theme 
          (Font.standardish_theme base_font_size "serif" "justify");
        header_block ~frame:"3px";
        paragraph_style ~separate:"0.5em" ~debug;
        enable_scrolling;
        blockquote ~style:(`left_bar);
        list_geometry ~style:(`compact "1.8em") ~debug;
        dibrawi_menu;
        dibrawi_cmt;
        tables_and_figures;
        footnotes;
        if add_section_numbers then section_numbers else (fun _ -> empty);
        code_blocks ~with_border:`no;
        layout (`simple (left_margin, text_width));
      ])
      ~body:(body (`simple))

  let with_sidepane_greenish 
      ?(add_section_numbers=true) ?(side=`right)
      ?(debug=false)  ?(rss: string option) ?(atom: string option)
      ?(icon:string option)
      ?(insertion=(fun _ -> "<!-- Side Insertion -->")) () =
    make ()
      ?rss ?atom ?icon
      ~css:(css ~color_theme:Color.greenish_main_theme [
        install_color_theme ~for_class:".content" ~theme:Color.greenish_main_theme;
        install_color_theme ~theme:Color.greenish_secondary_theme;
        install_font_theme
          (Font.standardish_theme "70%" "sans-serif" "none");
        install_font_theme ~for_class:".content"
          (Font.standardish_theme "120%" "serif" "justify");
        header_block ~frame:"7px";
        paragraph_style ~separate:"0.5em" ~debug;
        enable_scrolling;
        blockquote ~style:(`left_bar);
        list_geometry ~style:(`compact "1.8em") ~debug;
        dibrawi_menu;
        dibrawi_cmt;
        tables_and_figures;
        footnotes;
        if add_section_numbers then section_numbers else (fun _ -> empty);
        section_decoration;
        code_blocks ~with_border:`no;
        layout (`with_sidepane (side, 30.));
      ])
      ~body:(body (`with_sidepane insertion))

  let with_sidepane_redish 
      ?(add_section_numbers=true) ?(side=`right)
      ?(debug=false) 
      ?(insertion=(fun _ -> "<!-- Side Insertion -->")) () =
    make ()
      ~css:(css ~color_theme:Color.sober_redish_theme [
        install_color_theme;
        install_font_theme
          (Font.standardish_theme "70%" "sans-serif" "none");
        install_font_theme ~for_class:".content"
          (Font.standardish_theme "120%" "serif" "justify");
        header_block ~frame:"7px";
        paragraph_style ~separate:"0.5em" ~debug;
        enable_scrolling;
        blockquote ~style:(`left_bar);
        list_geometry ~style:(`compact "1.8em") ~debug;
        dibrawi_cmt;
        dibrawi_menu;
        tables_and_figures;
        footnotes;
        if add_section_numbers then section_numbers else (fun _ -> empty);
        section_decoration;
        code_blocks ~with_border:`no;
        layout (`with_sidepane (side, 30.));
      ])
      ~body:(body (`with_sidepane insertion))

end

module File = struct


  (* Generates a BIG closure !! *)
  let tmpl_regexp =
    Pcre.regexp "DIBRAWI_TEMPLATE_[A-Z_]+"

  let load_html input_str : html_template = 
    fun ?(menu="") ?(toc="")  ?(title="") ?(footer="") ?(from=[""]) content ->
      str (Pcre.substitute ~rex:tmpl_regexp
             ~subst:(function
               | "DIBRAWI_TEMPLATE_TITLE" -> title
               | "DIBRAWI_TEMPLATE_MENU" -> menu
               | "DIBRAWI_TEMPLATE_TOC" -> toc
               | "DIBRAWI_TEMPLATE_CONTENT" -> content
               | "DIBRAWI_TEMPLATE_FOOTER" -> footer
               | "DIBRAWI_TEMPLATE_PATH_TO_ROOT" -> 
                 let depth = Ls.length from - 1 in
                 ("./" ^ (Str.concat "/" (Ls.init depth (fun _ -> ".."))))
               | s -> s) input_str)

end      

let tmpl_html_default =
    "<!DOCTYPE html\n\
    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
    <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n\
    <!-- Generated with BraceTax -->\n\
    <head>\n\
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n\
        <title>DIBRAWI_TEMPLATE_TITLE</title>\n\
    </head>\n\
    <body>\n\
    <h1>Page: DIBRAWI_TEMPLATE_TITLE</h1>\n\
    <h2>Menu:</h2>DIBRAWI_TEMPLATE_MENU \n\
    <h2>Table of Contents:</h2>DIBRAWI_TEMPLATE_TOC \n\
    <hr/>
    DIBRAWI_TEMPLATE_CONTENT \n\
    <hr/>
    DIBRAWI_TEMPLATE_FOOTER \n\
    </body></html>\n\
    "

let html_default = File.load_html tmpl_html_default


