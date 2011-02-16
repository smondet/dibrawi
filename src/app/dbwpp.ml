
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
    let magic_args = ref [] in
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
            "\n\tOutput for HTML format (default)");
        ("-latex",
            Arg.Unit (fun () -> out_format := `pdf),
            "\n\tOutput for LaTeX format");
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
        ("-magic", 
         Arg.Rest (fun s ->
           magic_args := s :: !magic_args),
         "\n\tGuess what to do from the remaining arguments …\
          \n\t* Arguments ending with .brtx will be treated as files \
              to preprocess\n\t* All the other arguments will be passed to \
              the executable\n\t  (like with -arg)\n\t* If at least one \
              argument starts with “pdf”, “latex”, or “tex” \
              \n\t  or ends with “.tex”, “.pdf”, or “.ltx”, \
              this will imply -latex ");

    ] (fun s -> to_preprocess := s :: !to_preprocess) usage;
    let () =
      if !magic_args <> [] then (
        run_mix := true;
        mix_output := `camlmix;
        if !output_file = "" then (
          output_file := "/tmp/dbwpp_camlmix.mlx";
        );
        if !biblio_html_prefix = None then (
          biblio_html_prefix := Some "";
        );
        let is_to_preprocess s =
          Str.ends_with s ".brtx" in
        let implies_latex s =
          Str.ends_with s ".pdf" || Str.starts_with s "pdf"
          || Str.ends_with s ".tex" || Str.starts_with s "tex"
          || Str.ends_with s ".ltx" || Str.starts_with s "latex" in 
        if Ls.exists !magic_args ~f:implies_latex then (
          out_format := `pdf;
        );
        Ls.iter (Ls.rev !magic_args) ~f:(fun s ->
          if is_to_preprocess s then (
            to_preprocess := s :: !to_preprocess;
          ) else (
            mix_args := s :: !mix_args;
          )
        );
      );
    in
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
