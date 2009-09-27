

open Dibrawi_std


let transform data build = (
)

let () = (
    let usage = "rtfm: dbw -help" in

    if Array.length Sys.argv = 1 then (
        printf p"%s\n" usage;
    ) else (
        match Sys.argv.(1) with
        | "-version" ->
            printf p"dbw v. 0 (%s)\n" Dibrawi.Info.version_string;
            printf p"OCaml: %s, Batteries: %s, PCRE: %s, Bracetax: %s\n"
                Shell.ocaml_version Batteries_config.version Pcre.version
                Bracetax.Info.version;
        | "-help" ->
            printf p"dwb -help, or dwb -version, or dwb <datadir> <targetdir>\n";

        | s ->
            if Array.length Sys.argv = 3 then (
                transform Sys.argv.(1) Sys.argv.(2)
            ) else (
                printf p"%s\n" usage;
            )
    )
)


