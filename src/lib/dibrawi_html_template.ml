open Dibrawi_std
open String_tree
let ($) f x = f x
let opt_str_map f o = Opt.map_default (fun s -> str (f s)) empty o


type color = string
type color_theme = [ `background | `main_font ] -> color option

let no_theme = function
  | `background 
  | `main_font -> None

type css_global_parameters = {
  color_theme: color_theme;
}

let opt_fill_css param opt =
  match opt with
  | None -> empty
  | Some value ->
    str $ sprintf "   %s: %s;" param value

let body_colors_deprecated params =
  cat ~sep:(str "\n") [
    str "body {";
    opt_fill_css "background-color" (params.color_theme `background);
    opt_fill_css "color" (params.color_theme `main_font);
    str "}";
  ]

let css ?(color_theme=no_theme) l =
  let params = {
    color_theme = color_theme;
  } in
  let tree =
    cat ~sep:(str "\n") [
      str "<style type=\"text/css\">";
      cat (Ls.map ~f:(fun f -> f params) l);
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
  fun ?menu  ?toc ?title ?footer ?from content ->
    match style with
    | `raw | `simple ->
      cat ~sep:new_line [
        opt_str_map (sprintf "<div class=\"dibrawititle\">%s</div>") title;
        opt_str_map (sprintf "<div class=\"dibrawimenu\">%s</div>") menu;
        opt_str_map (sprintf "<div class=\"dibrawitoc\">%s</div>") toc;
        str $ sprintf "<div class=\"dibrawicontent\">%s</div>" content;
        opt_str_map (sprintf "<div class=\"dibrawifooter\">%s</div>") footer;
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
      ])
      ~body:(body `simple)
  in
  print $ doc tmpla;
  print $ doc tmplb;
  ()
