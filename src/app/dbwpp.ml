
open Dibrawi_std

open Printf

let () = (
    let citations_file = ref "" in
    let biblio_html_prefix = ref None in
    let to_preprocess = ref [] in
    let output_file = ref "" in
    let out_format = ref `html in
    let mix_output = ref `wiki in
    let run_mix = ref false in
    let mix_args = ref [] in
    let usage = "dbwpp [OPTIONS] file1 file2 ..." in

    Arg.parse [
        ("-version",
            Arg.Unit (fun () ->
                printf "Dibrawi Preprocessor, for %s\n%!"
                    Dibrawi.Info.version_string),
            "\n\tprint version");
        ("-citations",
            Arg.Set_string citations_file,
            "<file> \n\tOutput citations to <file>");
        ("-output",
            Arg.Set_string output_file,
            "<file>\n\tOutput to file <file>");
        ("-biblio-html-prefix",
            Arg.String (fun s -> biblio_html_prefix := Some s),
            "<str>\n\tSet the prefix for HTML biblio-links to <str>");
        ("-html",
            Arg.Unit (fun () -> out_format := `html),
            "\n\tOutput for HTML format");
        ("-camlmix",
            Arg.Unit (fun () -> mix_output := `camlmix),
            "\n\tOutput mix:*'s for Camlmix");
        ("-run", 
         Arg.Unit (fun () ->
           run_mix := true;
           mix_output := `camlmix;
           if !output_file = "" then (
             output_file := "/tmp/dbwpp_camlmix.mlx";
           );
           if !biblio_html_prefix = None then (
             biblio_html_prefix := Some "";
           );
         ),
         "\n\tRun camlmix (implies -camlmix, and sets default values, for \
              \n\t-output, and -biblio-html-prefix which can be \
              overwritten by setting\n\tthem “after” -run)");
        ("-arg", Arg.String (fun s -> mix_args := s :: !mix_args),
         "\n\tAdd argument for the mix-ed executable");
        ("-args", Arg.Rest (fun s ->  mix_args := s :: !mix_args),
         "\n\tAdd all following arguments for the mix-ed executable");
        ("-latex",
            Arg.Unit (fun () -> out_format := `pdf),
            "\n\tOutput for LaTeX format");

    ] (fun s -> to_preprocess := s :: !to_preprocess) usage;
    let output_citations_chan = 
        if !citations_file =$= "" then None else Some (open_out !citations_file)
    in
    let output_chan =
        if !output_file =$= "" then stdout else (open_out !output_file) in
    let citations = ref [] in
    let html_cite =
        let prefix = 
            Opt.default 
                Dibrawi.Preprocessor.default_html_biblio_page
                !biblio_html_prefix in
        fun cites ->
            citations := cites :: !citations;
            Dibrawi.Preprocessor.default_html_cite prefix cites
    in
    Ls.iter (Ls.rev !to_preprocess) ~f:(fun filename ->
        let page = Dibrawi.Data_source.get_file filename in
        let preprocessed =
            Dibrawi.Preprocessor.brtx2brtx ~mix_output:!mix_output
                ~html_cite ~output:!out_format ~from:["cmdline"] page in
        fprintf output_chan "%s\n" preprocessed;
    );
    let cites = Ls.flatten !citations in
    Opt.may output_citations_chan ~f:(fun o ->
      fprintf o "%s\n" (Str.concat " " (Ls.rev cites));
    );
    close_out output_chan;
        
    if !run_mix then (
      eprintf "Camlmixing ...\n";
      Dibrawi.System.run_command 
        (sprintf "camlmix \
                  -insert 'module Mix = Dibrawi_mix.Make(Camlmix) \
                    let () = (\
                    Mix.Params.set_output `%s; \
                    Mix.Params.citations := [%s])\
                  ' \
                  -c -co /tmp/dbwpp_camlmix_out.ml %s"
           (match !out_format with `html -> "html" | `pdf -> "pdf")
           (Str.concat ";" (Ls.map (sprintf "%S") (Ls.rev cites)))
           !output_file);
      Dibrawi.System.run_command 
        (sprintf "ocamlfind ocamlopt -package dibrawi -linkpkg \
                  /tmp/dbwpp_camlmix_out.ml -o /tmp/dbwpp_camlmix_out ");
      Dibrawi.System.run_command 
        (sprintf "/tmp/dbwpp_camlmix_out %s"
           (Str.concat " " (Ls.map (sprintf "'%s'") (Ls.rev !mix_args))));
    );

)
