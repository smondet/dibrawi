open Dibrawi_std
open String_tree
let ($) f x = f x

type global_parameters = {
  path_to_root: string list;
}




let make ?(css=`none) () =
  let insert_css css params =
    match css with
    | `none -> str ""
    | `link target ->
      str $ sprintf
        "<link rel=\"stylesheet\"  type=\"text/css\" href=\"%s\"/>"
        target
    | `link_from_root target ->
      str $ sprintf
        "<link rel=\"stylesheet\"  type=\"text/css\" href=\"%s/%s\"/>"
        (Str.concat "/" params.path_to_root) target
  in
  let template_function  ?menu ?toc ?title ?footer ?(from=[]) content =
    let params = { path_to_root = from } in
    let opt_str_map f o = Opt.map_default (fun s -> str (f s)) empty o in
    cat [
      str "<!DOCTYPE html\n\
        PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
        <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n\
        <!-- Generated with Dibrawi, BraceTax, Sebib, and some more hacks -->\n\
        <head>\n\
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n";
      insert_css css params;
      opt_str_map (sprintf "<title>%s</title>\n")  title;
      str "</head>\n<body>\n";
      opt_str_map (sprintf "<div class=\"dibrawititle\">%s</div>\n") title;
      opt_str_map (sprintf "<div class=\"dibrawimenu\">%s</div>\n") menu;
      opt_str_map (sprintf "<div class=\"dibrawitoc\">%s</div>\n") toc;
      str $ sprintf "<div class=\"dibrawicontent\">%s</div>\n" content;
      opt_str_map (sprintf "<div class=\"dibrawifooter\">%s</div>\n") footer;
      str "</body></html>\n";
    ]
  in
  template_function

