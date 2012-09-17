open Dibrawi_std

let opt_map_default f default o = Option.value_map ~f ~default o
  
let make_letter ~src ~dest ?date ?sign ?opening ?closing content =
  String.concat ~sep:"\n\n" [
    sprintf "\\address{%s}" src;
    opt_map_default (fun s -> sprintf "\\signature{%s}" s) "% No sig" sign;
    opt_map_default (fun s -> sprintf "\\date{%s}" s) "% No sig" date;
    sprintf "\\begin{letter}{%s}\n" dest;
    opt_map_default (fun s -> sprintf "\\opening{%s}" s) "% No open" opening;
    content;
    opt_map_default (sprintf "\\closing{%s}") "% No closing" closing;
    "\\end{letter}\n";
  ]
let make_french_letter
    ~src ~dest ?(cut_rule=false)
    ?phone ?from_city ?subject
    ?date ?sign ?opening ?closing content =
  String.concat ~sep:"\n\n" [
    sprintf "\\begin{letter}{%s}\n" dest;
    "\\nofax\n";
    opt_map_default (fun s -> sprintf "\\telephone{%s}" s) "\\notelephone" phone;
        (* "\\name{Seb}\n"; *)
    opt_map_default (fun s -> sprintf "\\lieu{%s}" s) "\\nolieu" from_city;
    sprintf "\\address{%s}" src;
    opt_map_default (fun s -> sprintf "\\conc{%s}" s) "% No conc" subject;
    opt_map_default (fun s -> sprintf "\\signature{%s}" s) "% No sig" sign;
    opt_map_default (fun s -> sprintf "\\date{%s}" s) "% No sig" date;
    if cut_rule then "" else "\\NoRule\n";
    opt_map_default (fun s -> sprintf "\\opening{%s}" s) "% No open" opening;
    content;
    sprintf "\\closing{%s}" (Option.value ~default:"CLOSING" closing);
    "\\end{letter}\n";
  ]
        
exception Build_error of string
    
let show_errors ?(out=stderr) log =
  let sep = "===================================================" in
  let log_extract =
    Dibrawi_system.slurp_command
      (sprintf
         "egrep -A 10 ^! %s | sed 's/^--$/%s/'" log sep) in
  fprintf out "\nERROR:\n  => XeLaTeX error\n  log: %s\n%s\n%s"
    log sep log_extract
    
type print_error_style = 
  [ `no | `simple of out_channel | `complex of out_channel ]

let build ?(with_bibtex=false) ?(raises=true)
    ?(print_errors:print_error_style=`complex stderr) path = 
  let pwd = Sys.getcwd () in
  let cd = Filename.dirname path in
  Sys.chdir cd;
  let pdflatex = 
    "xelatex -interaction=nonstopmode" in
  let target = Filename.chop_extension (Filename.basename path) in
  let run_command c =
    match Unix.system c with
    | Ok () -> ()
    | err ->
      let log = sprintf "%s/%s.log" cd target in
      raise (Build_error log) in
  let commands =
    let latex = (sprintf "%s %s > /dev/null 2>&1" pdflatex target) in
    if with_bibtex then
      [ latex; (sprintf "bibtex %s > /dev/null"  target); latex; latex; latex ]
    else
      [ latex; latex; latex ] in
  begin try List.iter ~f:run_command commands with
  | Build_error log as e ->
    begin match print_errors with
    | `no -> ()
    | `simple out ->
      (fprintf out "XeLaTeX: Compilation of %s failed; see %s\n" target log)
    | `complex out ->
      show_errors ~out log
    end;
    if raises then raise e;
  end;
  Sys.chdir pwd;
  (sprintf "%s/%s.pdf" cd target)

let do_clean basename =
  let exts = [ ".aux"; ".bbl"; ".blg"; ".out"; ".toc"; ".tns" ] in
  let cmd =
    "rm -f " ^ (String.concat ~sep:" " (List.map exts ~f:((^) basename))) in
  ignore (Unix.system cmd)

let build_string ?with_bibtex ?raises ?print_errors str =
  let name = "/tmp/buildpdfstring.tex" in
  let o = open_out name in
  output_string o str;
  close_out o;
  let result = build ?with_bibtex ?raises ?print_errors name in
  do_clean "/tmp/buildpdfstring";
  result

let build_string_tree ?with_bibtex ?raises ?print_errors stree =
  let name = "/tmp/xelatexbuildstringtree.tex" in
  Out_channel.with_file name ~f:(fun out ->
    String_tree.print ~out stree;
  );
  let result = build ?with_bibtex ?raises ?print_errors name in
  do_clean "/tmp/xelatexbuildstringtree";
  result

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
  let ($) f x = f x

  type color_theme = [`none | `classy ]

  type global_parameters = {
    color_theme: color_theme;
    columns: [ `one | `two ];
  }
  
  type component = global_parameters -> Dibrawi_std.String_tree.t

  let compact_title_box ?with_color params =
    let really_with_color =
      match with_color, params.color_theme with
      | Some cond, _ -> cond
      | None, `none -> false
      | None, `classy -> true in
    cat [
      str (if really_with_color then
          "\\definecolor{webred}{rgb}{0.3,0,0}" else "");
      str  $ sprintf
"
\\makeatletter
\\def\\MyBox#1{\\framebox[\\textwidth][c]{#1}}
\\def\\maketitle{
%s
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
    %s
}
\\makeatother
" 
(if params.columns = `two then "\\twocolumn[%%" else "")
(if really_with_color then "\\textcolor{webred}" else "")
(if params.columns = `two then "]" else "")]

  let change_height ?(pt=700) params =
    str $ sprintf "\n\\usepackage{layout}\n\\setlength{\\textheight}{%dpt}\n" pt


  let package_geometry ?(paper=`A4) ?(raw_options="") params =
    let paper_str =
      match paper with
      | `A0 -> "a0paper,"
      | `A1 -> "a1paper,"
      | `A2 -> "a2paper,"
      | `A3 -> "a3paper,"
      | `A4 -> "a4paper,"
      | `A5 -> "a5paper,"
      | `A6 -> "a6paper,"
    in
    let usepackage =
      str_cat ["\n\\usepackage["; paper_str; raw_options; "]{geometry}\n" ] in
    usepackage

  let paragraphs ?baselinestretch ?parskip ?parindent params =
    let omd f o = opt_map_default f "" o in
    str_cat [
      omd (sprintf "\\renewcommand{\\baselinestretch}{%.2f}\n") baselinestretch;
      omd (sprintf "\\setlength{\\parindent}{%s}\n") parindent;
      omd (sprintf "\\setlength{\\parskip}{%s}\n") parskip
    ]

  let small_itemize params = str
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
        \\setlength{\\leftmargin}{2em}%
        \\setlength{\\labelwidth}{0.5em}%
        \\setlength{\\labelsep}{0.5em}%
        \\csname \\the\\@itemdepth\\endcsname}%
    \\fi}
\\makeatother
"

  let verbatim_style ?(font_size=`footnote) params =
    str $ sprintf
"
\\makeatletter
\\renewcommand{\\verbatim@font}{%%
  \\ttfamily\\%s\\catcode`\\<=\\active\\catcode`\\>=\\active
}  
\\makeatother
"
(match font_size with `footnote -> "footnotesize" | `small -> "small")


  (*  Caption Package: http://www.dd.chalmers.se/latex/Docs/PDF/caption.pdf *)
  let package_caption params = str
"
\\usepackage[margin=10pt,font=small,labelfont=bf,labelsep=endash,nooneline]{caption}
"

  let compact_sections
      ?(how:[`with_titlesec | `homemade ]= `with_titlesec) params = 
    match how with
    | `with_titlesec ->
      str "\\usepackage[small,compact]{titlesec}\n"
    | `homemade ->
      str
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
    str $ sprintf "
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

  let listing_style ?(style="\\ttfamily\\footnotesize") params =
    str $ sprintf
"
\\lstset{ %%
language=[Objective]Caml,
basicstyle=%s,
%%numbers=left,  
numberstyle=\\tiny,
stepnumber=2,
%% will be numbered
numbersep=5pt,
backgroundcolor=\\color{white},
showspaces=false,
showstringspaces=false,
showtabs=false,
frame=single,
tabsize=2,
captionpos=b, %% sets the caption-position to bottom
breaklines=true, %% sets automatic line breaking
breakatwhitespace=true, %% sets if automatic breaks should only happen at whitespace
escapeinside={\\%%*}{*)}, %% if you want to add a comment within your code
morekeywords={*,...}, %% if you want to add more keywords to the set
keywordstyle=%s\\bfseries,
commentstyle=%s\\it,
stringstyle=%s\\bfseries,
}
"
      style
      (if params.color_theme = `none then "" else "\\color{red}")
      (if params.color_theme = `none then "" else "\\color[named]{RawSienna}")
      (if params.color_theme = `none then "" else "\\color[named]{NavyBlue}")

  (* http://texblog.wordpress.com/2007/11/07/headerfooter-in-latex-with-fancyhdr/ *)
  let package_fancyhdr param = 
    str "
\\usepackage{fancyhdr}
\\pagestyle{fancy}
% with this we ensure that the chapter and section
% headings are in lowercase
\\renewcommand{\\chaptermark}[1]{\\markboth{Chapter \\thechapter:\\ #1}{}}
\\renewcommand{\\sectionmark}[1]{\\markright{\\thesection\\ #1}}
%\\fancyhf{} %delete the current section for header and footer
\\fancyhead[LE,RO]{\\thepage}
\\fancyhead[LO]{\\rightmark}
\\fancyhead[RE]{\\leftmark}
\\fancyfoot[C]{}
"
  let hyphenations l params =
    str_cat [
      "\\hyphenation{";
      String.concat ~sep:" " l;
      "}\n";
    ]

  let fontspec params =
    str "
\\usepackage{fontspec}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont[]{FreeSerif}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\setsansfont[Scale=MatchLowercase]{DejaVu Sans}
"
  let section_numbers ?section_numbers_depth ?toc_depth params =
    let omd f o = opt_map_default f "" o in
    str_cat [
      omd (sprintf "\\setcounter{secnumdepth}{%d}") section_numbers_depth;
      omd (sprintf "\\setcounter{tocdepth}{%d}") toc_depth;
    ]

let make ?(add=[]) ?(color=`none)
    ?(language="english") 
    ?(document_class=`article 8)
    ?(columns=`two)
    ?(geometry:component option)
    ?(fontspec:(global_parameters -> String_tree.t) option)
    ?(two_sides=true)
    () =
  let params = {
    color_theme = color;
    columns = columns;
  } in
  let docclass =
    let col = match columns with `one -> "" | `two -> ",twocolumn" in
    str 
      (match document_class with
      | `article pt ->
        sprintf 
          "\\documentclass[%dpt%s%s]{extarticle}\n\
           \\newcommand\\chapter[1]{{\\LARGE{\\textbf{Chapter: #1}}}\
              \\setcounter{section}{0} \\par}"
          pt col (if two_sides then ",twoside" else "")
      | `book pt ->
        sprintf 
          "\\documentclass[%dpt%s%s]{extbook}\n"
          pt col (if two_sides then ",twoside" else "")
      | `french_letter pt ->
        sprintf "
\\documentclass[%dpt]{lettre}
\\makeatletter
\\newcommand*{\\NoRule}{\\renewcommand*{\\rule@length}{0}}
\\makeatother
" pt
      | `letter pt ->
        sprintf "\\documentclass[%dpt]{letter}\n" pt
      | `none -> ""
      ) in
  cat [
    docclass;
    (match geometry with
    | None -> str "" (* package_geometry ~paper:`A4 () *)
    | Some f -> f params);
    str "
\\clubpenalty=10000
\\widowpenalty=10000
\\sloppy
\\usepackage[dvipsnames,usenames]{color}
\\usepackage{ucs}
\\usepackage{xunicode}
\\usepackage{xltxtra}\n";
    str $ sprintf "\\usepackage[%s]{polyglossia}\n" language;
    (match fontspec with
    | None -> str ""
    | Some f -> f params);
    str "
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
";
    (cat (List.map ~f:(fun x -> x params) add))
  ]
end
