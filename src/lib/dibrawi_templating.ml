open Dibrawi_std

type html_template =
    ?menu:string -> ?toc:string -> ?title:string -> ?footer:string -> string -> string

let html_default
?(menu="") ?(toc="")  ?(title="") ?(footer="") content = (
    let css =
        "

div.sidepane {
    position:fixed;
    font-size: 80%;
    /* float_on_right */
    float: right;
    width: 30%;
    margin-left: 65%;
    padding: 1em;
}
div.content {
    margin-left: 5%;
    border-right: 1px solid gray;
    padding: 1em;
    width: 55%;
}


        body {
            background-color:#ffffff;
            color: black;
            font-family: sans-serif;
            min-width: 80em;
            text-align: justify;
            font-size: 80%;
        }

        div.p {
            padding-bottom: 0em;
            /* The debug border: */
            /* border: thin silver solid; */
        }
        div.p + div.p { padding-top: 0.5em; }

        div.header {
            text-align: center;
            /* border: #3E0C0C solid; */
            padding-top:    2.1em;
            padding-bottom: 2.05em;
            margin-bottom: 3em;
        }
        h1 {           font-size: 300%; font-variant: small-caps; }
        div.authors {  font-size: 150%; line-height: 200%; }
        div.subtitle { font-size: 140%; font-style: italic; }
        h2 {           font-size: 180%; }
        h3 {           font-size: 150%; padding-top: 0.1em;padding-bottom: 0.05em;}
        h4 {           font-size: 130%; padding-top: 0.1em;padding-bottom: 0.05em;}
        h5 {           font-size: 105%; padding-top: 0.1em;padding-bottom: 0.05em;}

        a {
            text-decoration:none;
        }
        /* http://www.vision.to/add-a-small-icon-to-your-links-css-only.php */
        a[href$='']:hover {text-decoration: underline;}
        a[href$='.pdf']:hover {text-decoration: underline;}
        a[href$=''] { color: #831825;}
        a[href$='.pdf'] { color: #831825;}

        tt {
            font-family: monospace;
            font-size: 120%;
            color: #003300;
        }

        code, pre {
            font-family: monospace;
            /* letter-spacing: 1px; */
            color: #003300;
        }
        pre {
            /* border: dashed;
            border-width: thin;
            border-color: #dddddd;*/
            position: relative;
            /*background-color: #eeeeee;*/
            left: 5%;
            width: 70%;
            clear: left;
            /*padding-bottom: 1em;*/
        }

        ul, ol {
            padding-top: 0em;
            padding-bottom: 0em;
            margin-top: 0em;
            margin-bottom: 0em;
            /* The debug border: */
            /* border: thin silver solid; */
        }

/*********** Tables  ***********/
table.tablefigure {
    margin-right:auto;
    margin-left: auto;
    border-collapse: collapse;
    min-width: 80%;
    margin-bottom: 1em;
}
caption.tablefigure {
    /* margin-left:1em; */
    /* min-width: 30%; */
    caption-side:bottom;
}
/*caption.tablefigure:after { content: \" [\" attr(id) \"]\"; color: figid_color; }*/
div.tablefigure {
    text-align: center;
    font-size:90%;
    border: thin #959595 solid;
    margin: 0.5em;
    padding: 0.5em;
    /*float: right;
    width: 50%;
    margin-right: 0em;
    padding: 0.5em;*/
}
        "
    in

    sprintf p"\n\
<!DOCTYPE html\n\
    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
    <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n\
    <!-- Generated with BraceTax -->\n\
    <head>\n\
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n\
    <title>%s</title>\n\
    <style>\n\
%s\n\
    </style>\n\
    </head>\n\
    <body>\n\
    <div class=\"sidepane\">\n\
        <b>%s</b><br/>\n\
        %s\n\
        <hr/>\n\
        %s\n\
        <hr/>\n\
        %s\n\
    </div>\n\
    <div class=\"content\">\n\
        %s\n\
    </div>\n\
</body></html>\n\
"
title css title menu toc footer content
)

(* Generates a BIG closure !! *)
let load str = (
    failwith "Templating.load not implemented"
)


