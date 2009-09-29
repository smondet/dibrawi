open Dibrawi_std

type template =
    ?menu:string -> ?toc:string -> ?title:string -> ?footer:string -> string -> string


(* Generates a BIG closure !! *)
let tmpl_regexp =
    Pcre.regexp "DIBRAWI_TEMPLATE_[A-Z]+"

let load str = (
    fun ?(menu="") ?(toc="")  ?(title="") ?(footer="") content ->
        Pcre.substitute ~rex:tmpl_regexp ~subst:(function
            | "DIBRAWI_TEMPLATE_TITLE" -> title
            | "DIBRAWI_TEMPLATE_MENU" -> menu
            | "DIBRAWI_TEMPLATE_TOC" -> toc
            | "DIBRAWI_TEMPLATE_CONTENT" -> content
            | "DIBRAWI_TEMPLATE_FOOTER" -> footer
            | s -> s) str
)


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

let html_default = load tmpl_html_default

let tmpl_latex_default =
"\
    \\documentclass[a4paper,10pt]{article}\n\
    \n\
    \\clubpenalty=10000\n\
    \\widowpenalty=10000\n\
    \n\
    \\usepackage[T1]{fontenc}\n\
    \\usepackage[english]{babel}\n\
    \\usepackage{multirow}\n\
    \\usepackage{ucs}\n\
    \\usepackage[utf8x,utf8]{inputenc}\n\
    \\usepackage[                         \n\
        bookmarks         = true,         \n\
        bookmarksnumbered = true,         \n\
        colorlinks        = true,         \n\
    ]{hyperref}                           \n\
    \\usepackage{color}\n\
    \\definecolor{webred}{rgb}{0.3,0,0}\n\
    \\definecolor{blurl}{rgb}{0,0,0.3}\n\
    \\hypersetup{\n\
    linkcolor         = webred, %%black\n\
    citecolor         = webred, %%black\n\
    urlcolor          = blurl , %%black\n\
    linkbordercolor   = {1 1 1},\n\
    citebordercolor   = {1 1 1},\n\
    urlbordercolor    = {1 1 1},\n\
    pdfauthor   = {},\n\
    pdftitle    = {DIBRAWI_TEMPLATE_TITLE},\n\
    pdfsubject  = {},\n\
    pdfkeywords = {},\n\
    pdfcreator  = {Dibrawi, Bracetax and PDFLaTeX},\n\
    pdfproducer = {Dibrawi, Bracetax and PDFLaTeX}}\n\
    \n\
    \\usepackage[pdftex]{graphicx}\n\
    \\frenchspacing\n\
    \\DeclareGraphicsExtensions{.jpg,.mps,.pdf,.png}\n\
    \n\
    \\begin{document}\n\
    \n\
    DIBRAWI_TEMPLATE_CONTENT
    \\end{document}
    "

let latex_default = load tmpl_latex_default
