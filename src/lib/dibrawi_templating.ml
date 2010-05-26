open Dibrawi_std

type html_template =
    ?menu:string -> ?toc:string -> ?title:string -> ?footer:string ->
    ?from:(string list) ->
    string -> string


(* Generates a BIG closure !! *)
let tmpl_regexp =
    Pcre.regexp "DIBRAWI_TEMPLATE_[A-Z_]+"

let load_html str : html_template = 
  fun ?(menu="") ?(toc="")  ?(title="") ?(footer="") ?(from=[""]) content ->
    Pcre.substitute ~rex:tmpl_regexp
      ~subst:(function
        | "DIBRAWI_TEMPLATE_TITLE" -> title
        | "DIBRAWI_TEMPLATE_MENU" -> menu
        | "DIBRAWI_TEMPLATE_TOC" -> toc
        | "DIBRAWI_TEMPLATE_CONTENT" -> content
        | "DIBRAWI_TEMPLATE_FOOTER" -> footer
        | "DIBRAWI_TEMPLATE_PATH_TO_ROOT" -> 
          let depth = Ls.length from - 1 in
          ("./" ^ (Str.concat "/" (Ls.init depth (fun _ -> ".."))))
        | s -> s) str
      

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

let html_default = load_html tmpl_html_default
