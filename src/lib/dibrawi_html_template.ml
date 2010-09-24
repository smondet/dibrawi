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
  type theme = (string * color option * color option) list

  let color ?bg ?fg who = (who, fg, bg)
  let fg who c = (who, Some c, None)  

  let empty_theme:theme = []
  let dummy_theme:theme = [
    color "body"     ~fg:"#113311" ~bg:"#ccccff";
    color "a:link"    ~fg:"#000044";
    color "a:visited" ~fg:"#440000";
    color "a:hover"  ~fg:"#00eeee" ~bg:"#0000ee"; 
    color "tt,pre,code"  ~fg:"#331133";
    fg ".dbwmixcode" "#dd0000";
  ]

  let install_theme ?for_class theme =
    let color x = opt_fill_css "color" x in
    let background x = opt_fill_css "background-color" x in
    let block = css_block ?class_opt:for_class in
    cat ~sep:new_line 
      (Ls.map theme ~f:(fun (p, f, b) -> block p [ color f; background b]))


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


end


    

type css_global_parameters = {
  color_theme: Color.theme;
}

let install_color_theme
    ?for_class ?(overwrite_color_theme:Color.theme option) params =
  let theme = Opt.default params.color_theme overwrite_color_theme in
  Color.install_theme ?for_class theme

let install_font_theme ?for_class theme params =
  Font.install_theme ?for_class theme

let paragraph_style ?(debug=false) ?(separate="0.5em") params =
  cat ~sep:new_line [
    str "div.p {";
    str "    padding-bottom: 0em;";
    if debug then 
      str "/* The debug border: */ border: thin silver solid;"
    else 
      empty;
    str "}";
    str $ sprintf "div.p + div.p { padding-top: %s; }" separate;
  ]

let header_block ?(frame=("black", "5px")) params =
  let frame_color, frame_line_size = frame in
  str $ sprintf
    "div.header {\n\
    \    text-align: center;\n\
    \    border:  %s solid %s;\n\
    \    padding-top:    2.1em;\n\
    \    padding-bottom: 2.05em;\n\
    \    margin-bottom: 3em;\n\
    }" frame_color frame_line_size


let css ?(color_theme=Color.empty_theme) l =
  let params = {
    color_theme = color_theme;
  } in
  let sep = new_line in
  let tree =
    cat ~sep [
      str "<style type=\"text/css\">";
      cat ~sep (Ls.map ~f:(fun f -> f params) l);
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
    | `raw | `simple ->
      cat ~sep [
        opt_str_map (sprintf "<div class=\"dibrawititle\">%s</div>") title;
        opt_str_map (sprintf "<div class=\"dibrawimenu\">%s</div>") menu;
        opt_str_map (sprintf "<div class=\"dibrawitoc\">%s</div>") toc;
        str $ sprintf "<div class=\"dibrawicontent\">%s</div>" content;
        opt_str_map (sprintf "<div class=\"dibrawifooter\">%s</div>") footer;
      ]
    | `three_columns top_right ->
      cat ~sep [
        str "<div class=\"leftside\">";
        opt_str_map (sprintf "<b>Page:</b> %s <br/>") title;
        opt_str_map 
          (sprintf "<hr/><b>Menu: </b><ul class=\"beginmenu\">%s</ul>")
          menu;
        str "</div>";
        str "<div class=\"rightside\">";
        str (top_right from);
        opt_str_map (sprintf "<hr/><b>Table of contents:</b><br/>%s<br/>") toc;
        opt_str_map (sprintf "<hr/>%s<br/>") footer;
        str "</div>";
        str "<div class=\"content\">";
        str content;
        str "</div>";
      ]



let make ?(css=`none) ?(body:body_style option) () =

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

let _test () =
  let doc (tmpl:html_template) = 
    tmpl ~menu:"The Menu ..." ~from:[ "page.brtx"; "dir"; "dir"]
      "The content..." in
  let tmpla = make ~css:(`link_from_root "fromroot.css") () in
  let tmplb = 
    make ()
      ~css:(css [
         install_color_theme ~for_class:".the_class";
      ])
      ~body:(body `simple)
  in
  let tmplc = 
    make ()
      ~css:(css ~color_theme:Color.dummy_theme [
        install_color_theme;
        install_font_theme Font.dummy_theme;
        paragraph_style ~separate:"1em" ~debug:true;
      ])
      ~body:(body (`three_columns (fun _ -> "<!-- Right Side Insertion -->")))
  in
  let next () = printf "==================================================\n" in
  print $ doc tmpla; next ();
  print $ doc tmplb; next ();
  print $ doc tmplc; next ();
  ()
