open Dibrawi_std

type html_template =
    ?menu:string -> ?toc:string -> ?title:string -> ?footer:string -> string ->
    string
type latex_template =
    ?title:string -> ?authors:string -> ?subtitle:string ->
    ?bibtex_file:string -> string -> string


(* Generates a BIG closure !! *)
let tmpl_regexp =
    Pcre.regexp "DIBRAWI_TEMPLATE_[A-Z]+"

let load_html str = (
    fun ?(menu="") ?(toc="")  ?(title="") ?(footer="") content ->
        Pcre.substitute ~rex:tmpl_regexp ~subst:(function
            | "DIBRAWI_TEMPLATE_TITLE" -> title
            | "DIBRAWI_TEMPLATE_MENU" -> menu
            | "DIBRAWI_TEMPLATE_TOC" -> toc
            | "DIBRAWI_TEMPLATE_CONTENT" -> content
            | "DIBRAWI_TEMPLATE_FOOTER" -> footer
            | s -> s) str
)

let load_latex str = (
    fun ?(title="") ?(authors="")  ?(subtitle="") ?(bibtex_path="") filename ->
        Pcre.substitute ~rex:tmpl_regexp ~subst:(function
            | "DIBRAWI_TEMPLATE_TITLE" -> title
            | "DIBRAWI_TEMPLATE_AUTHORS" -> authors
            | "DIBRAWI_TEMPLATE_SUBTITLE" -> subtitle
            | "DIBRAWI_TEMPLATE_FILENAME" -> filename
            | "DIBRAWI_TEMPLATE_BIBTEXFILE" -> bibtex_path
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

let html_default = load_html tmpl_html_default

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
    pdfauthor   = {DIBRAWI_TEMPLATE_AUTHORS},\n\
    pdftitle    = {DIBRAWI_TEMPLATE_TITLE},\n\
    pdfsubject  = {DIBRAWI_TEMPLATE_SUBTITLE},\n\
    pdfkeywords = {},\n\
    pdfcreator  = {Dibrawi, Bracetax and PDFLaTeX},\n\
    pdfproducer = {Dibrawi, Bracetax and PDFLaTeX}}\n\
    \n\
    \\usepackage[pdftex]{graphicx}\n\
    \\frenchspacing\n\
    \\DeclareGraphicsExtensions{.jpg,.mps,.pdf,.png}\n\
    \n\
    \\title{DIBRAWI_TEMPLATE_TITLE}\n\
    \\author{DIBRAWI_TEMPLATE_AUTHORS}\n\
    \\date{DIBRAWI_TEMPLATE_SUBTITLE}\n\
    \\begin{document}\n\
    \\maketitle\n\
    \n\
    \\input{DIBRAWI_TEMPLATE_FILENAME}\n\
    \\bibliographystyle{alpha}\n\
    \\bibliography{DIBRAWI_TEMPLATE_BIBTEXFILE}\n\
    \\end{document}\n\
    "

let latex_default = load_latex tmpl_latex_default
