open Dibrawi_std


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
    "xelatex -interaction=nonstopmode" in
  let target = Filename.chop_extension (Filename.basename path) in
  let run_command c =
    match Unix.system c with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
      printf "XeLaTeX: Compilation of %s failed with error code: %d\n\
                see %s/%s.log\n" target n cd target;
      failwith "xelatex"
    | _ ->
      printf "XeLaTeX: Compilation of %s got killed (?)\n\
                see %s/%s.log\n" target cd target;
      failwith "xelatex"
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

let build_string_tree ?(with_bibtex=false) stree =
  let name = "/tmp/xelatexbuildstringtree.tex" in
  Io.with_file_out name (fun out ->
    String_tree.print ~out stree;
  );
  build ~with_bibtex name

let make_full_file 
    ?(pdf_title="") ?(pdf_authors="")  ?(pdf_subject="")
    ~latex_template
    ?(add_document_env=true)
    ?bibtex_style ?(bibtex_path="") content =
  let document =
    String_tree.str_cat [
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
  let hypersetup =
    sprintf
      "\\hypersetup{
          pdfauthor   = {%s},
          pdftitle    = {%s},
          pdfsubject  = {%s},
        }"
      pdf_authors pdf_title pdf_subject in
  String_tree.cat [
    latex_template;
    (String_tree.str hypersetup);
    document]
        
module Template = struct
  open String_tree

  type color_theme = [`none | `classy ]

  type global_parameters = {
    color_theme: color_theme;
  }

  let compact_title_box ?with_color params =
    let really_with_color =
      match with_color, params.color_theme with
      | Some cond, _ -> cond
      | None, `none -> false
      | None, `classy -> true in
    sprintf
"
%s
\\makeatletter
\\def\\MyBox#1{\\framebox[\\textwidth][c]{#1}}
\\def\\maketitle{
    \\twocolumn[%%
\\MyBox{\\begin{minipage}{0.7\\textwidth}
    \\begin{center}
    %s{\\bf\\huge \\@title}
    \\vskip 0.3cm
    {\\large \\@author}
    \\vskip 0.3cm
    {\\it \\@date}
    \\vskip 0.3cm
    \\end{center}
    \\end{minipage} }
    \\vskip 0.3cm
    ]
}
\\makeatother
" 
(if really_with_color then "\\definecolor{webred}{rgb}{0.3,0,0}" else "")
(if really_with_color then "\\textcolor{webred}" else "")

  let change_height ?(pt=700) params =
    sprintf 
"
\\usepackage{layout}
\\setlength{\\textheight}{%dpt}
" pt

  let make_things_smaller ?(baselinestretch=0.9) ?(parskip="1ex") params =
    sprintf
"
\\renewcommand{\\baselinestretch}{%.2f}
\\setlength{\\parindent}{0em}
\\usepackage[small,compact]{titlesec}
\\setlength{\\topsep}{0pt}
\\setlength{\\itemsep}{0pt}
\\setlength{\\parskip}{%s}
\\setlength{\\parsep}{0pt}
\\setlength{\\headsep}{0pt}
\\setlength{\\topskip}{0pt}
\\setlength{\\topmargin}{0pt}
\\setlength{\\topsep}{0pt}
\\setlength{\\partopsep}{0pt}
"
  baselinestretch parskip

  let small_itemize params =
"
\\makeatletter  % makes '@' an ordinary character
\\def\\itemhook{}
\\def\\itemhooki{}
\\def\\itemhookii{}
\\def\\itemhookiii{}
\\def\\itemhookiv{}
\\def\\itemize{%
    \\ifnum \\@itemdepth >\\thr@@\\@toodeep\\else
        \\advance\\@itemdepth\\@ne
        \\edef\\@itemitem{labelitem\\romannumeral\\the\\@itemdepth}%
        \\expandafter
        \\list
        \\csname\\@itemitem\\endcsname
        %{$\\triangleright$}
        {%
        \\setlength{\\topsep}{0pt}%
        \\setlength{\\parsep}{0pt}%
        \\setlength{\\parskip}{0pt}%
        \\setlength{\\partopsep}{0pt}%
        \\setlength{\\itemsep}{1pt}%
        \\setlength{\\leftmargin}{1.3em}%
        \\setlength{\\labelwidth}{0.7em}%
        \\setlength{\\labelsep}{0.5em}%
        \\csname \\the\\@itemdepth\\endcsname}%
    \\fi}
\\makeatother
"

  (*  Caption Package: http://www.dd.chalmers.se/latex/Docs/PDF/caption.pdf *)
  let package_caption params =
"
\\usepackage[margin=10pt,font=small,labelfont=bf,labelsep=endash,nooneline]{caption}
"

  let compact_sections params =
"
\\makeatletter  % makes '@' an ordinary character
\\renewcommand{\\paragraph}{\\@startsection{paragraph}{4}{\\z@}%
             {2.25ex \\@plus 1ex \\@minus .2ex}%
             {0.5em}%
             {\\normalfont\\normalsize\\bfseries}}

\\renewcommand{\\section}{\\@startsection{section}{1}{\\z@}%
             {0.5em}%{2.25ex \\@plus 1ex \\@minus .2ex}%
             {0.1em}%{0.5em}%
             {\\normalfont\\Large\\bfseries}}
\\renewcommand{\\subsection}{\\@startsection{subsection}{2}{\\z@}%
             {0.2em \\@plus 1ex \\@minus .2ex}%
             {0.1em \\@plus 1ex \\@minus .2ex}%{0.5em}%
             {\\normalfont\\large\\itshape\\bfseries}}
\\renewcommand{\\subsubsection}{\\@startsection{subsubsection}{3}%
             {\\z@}%
             {0.1em}%{2.25ex \\@plus 1ex \\@minus .2ex}%
             {0.1em}%{0.5em}%
             {\\normalfont\\normalsize\\bfseries}}
\\makeatother   % makes '@' a special symbol again
"
  let tabular_style ?(font_size=`footnote) ?(vertical_cell_spacing=1.5) params =
    sprintf "
\\makeatletter
\\let\\orig@tabular\\tabular
\\let\\endorig@tabular\\endtabular
\\renewenvironment*{tabular}[1]
	{\\%s\\begin{orig@tabular}{#1}}
	{\\end{orig@tabular}}
\\makeatother
\\renewcommand{\\arraystretch}{%.3f}
"
(match font_size with `footnote -> "footnotesize" | `small -> "small")
vertical_cell_spacing

  let listing_style params =
    sprintf
"
\\lstset{ %%
language=[Objective]Caml,                %% choose the language of the code
basicstyle=\\ttfamily\\small, %%\\tiny\\bfseries, 
%%numbers=left,  
numberstyle=\\tiny,
stepnumber=2, %% the step between two line-numbers. If it's 1 each line 
%% will be numbered
numbersep=5pt, %% how far the line-numbers are from the code
backgroundcolor=\\color{white},  %% choose the background color. You must add \\usepackage{color}
showspaces=false, %% show spaces adding particular underscores
showstringspaces=false, %% underline spaces within strings
showtabs=false, %% show tabs within strings adding particular underscores
frame=single, %% adds a frame around the code
tabsize=2, %% sets default tabsize to 2 spaces
captionpos=b, %% sets the caption-position to bottom
breaklines=true, %% sets automatic line breaking
breakatwhitespace=false, %% sets if automatic breaks should only happen at whitespace
%%title=\\lstname, %% show the filename of files included with \\lstinputlisting;
 %% also try caption instead of title
escapeinside={\\%%*}{*)}, %% if you want to add a comment within your code
morekeywords={*,...}, %% if you want to add more keywords to the set
keywordstyle=\\color{red}\\bfseries,
commentstyle=\\color[named]{RawSienna},
stringstyle=\\color[named]{NavyBlue},
}
"

let make ?(add=[]) ?(color=`none)
    ?(language="english") ?(section_numbers_depth=3) () =
  let params = {
    color_theme = color;
  } in
  let ($) f x = f x in 
  cat [
    str "
\\documentclass[a4paper,8pt,twocolumn]{extarticle}
\\clubpenalty=10000
\\widowpenalty=10000
\\sloppy
\\usepackage[dvipsnames,usenames]{color}
\\usepackage{ucs}
\\usepackage{xunicode}
\\usepackage{xltxtra}\n";
    str $ sprintf "\\usepackage[%s]{polyglossia}\n" language;
    str "
\\usepackage{fontspec}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont[]{FreeSerif}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\setsansfont[Scale=MatchLowercase]{DejaVu Sans}
\\usepackage[                         
bookmarks         = true,         
bookmarksnumbered = true,         
colorlinks        = true,         
]{hyperref}";
    (match color with
      `none -> str ""
    | `classy -> str
      "\\definecolor{webred}{rgb}{0.3,0,0}\n\
       \\definecolor{blurl}{rgb}{0,0,0.3}\n\
       \\definecolor{darkgreen}{rgb}{0,0.3,0}");
    str $ sprintf "                           
\\hypersetup{
breaklinks = true,
%s
linkbordercolor   = {1 1 1},
citebordercolor   = {1 1 1},
urlbordercolor    = {1 1 1},
pdfkeywords = {},
pdfcreator  = {},
pdfproducer = {}}
"
      (match color with
        `none -> 
          "linkcolor         = black,\n\
           citecolor         = black,\n\
           urlcolor          = black,"
      | `classy ->
        "linkcolor         = webred, \n\
         citecolor         = webred, \n\
         urlcolor          = blurl,");

    str "
\\usepackage{multirow}
\\usepackage{listings}
\\usepackage{graphicx}
\\DeclareGraphicsExtensions{.jpg,.mps,.pdf,.png}
\\frenchspacing
\\newcommand\\dbwcmt[1]{\\textbf{\\textcolor{red}{[#1]}}}
\\newcommand\\chapter[1]{{\\LARGE{\\textbf{Chapter: #1}}}\\setcounter{section}{0} \\par}
";
    str $ sprintf "\\setcounter{secnumdepth}{%d}" section_numbers_depth;
    (str_cat (Ls.map (fun x -> x params) add))
  ]
end
