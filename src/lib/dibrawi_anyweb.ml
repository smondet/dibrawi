
open Dibrawi_std
module Dbw_sys = Dibrawi_system

type environment = {
  start_token : string;
  on_begin : unit -> string;
  on_text: string -> string;
  on_change : unit -> string;
  end_token : string;
  on_end : unit -> string;
  contains : string list;
}

let environment
    ?(on_begin = fun () -> "")
    ?(on_text = fun s -> s)
    ?(on_change = fun () -> "")
    ?(on_end = fun () -> "")
    start_token end_token contains =
  { on_begin; on_text; on_change; on_end;
    start_token; end_token; contains }    

let split_first s l current =
  List.fold_left ~f:(fun m k -> 
    match m, k with
    | None, x -> x
    | x, None -> x
    | Some (_, _, mb, _), Some (_, _, kb, _) ->
      if String.length mb <= String.length kb then m else k)
    ~init:None
    ((match More_string.split_once_at_string s current.end_token with 
      None -> None | Some (b, a) -> Some (true, current, b, a)) ::
        (List.map ~f:(fun env ->
          match More_string.split_once_at_string s env.start_token with
          | None -> None
          | Some (b, a) -> Some (false, env, b, a)) l))


let transform environments str =
  let buffer = Buffer.create 42 in
  let write = Buffer.add_string buffer in
  let next_line = 
    let l = ref (String.split str ~on:'\n') in
    (fun () -> match !l with [] -> None | h :: t -> l := t; Some (h ^ "\n")) in
  let rec loop stack current_text =
    match stack with
    | env :: l ->
      let inside = List.map ~f:(fun x -> List.Assoc.find_exn environments x) env.contains in
      begin match split_first current_text inside env with
      | Some (true, s, before, after) -> (* unstack *)
        env.on_text before |! write;
        env.on_end () |! write;
        loop l after
      | Some (false, s, before, after) -> (* stack *)
        env.on_text before |! write;
        env.on_change () |! write;
        s.on_begin () |! write;
        loop (s :: stack) after
      | None ->
        env.on_text current_text |! write;
        begin match next_line () with
        | Some line ->
          loop stack line
        | None -> env.on_end () |! write; ()
        end
      end
    | [] ->
      failwith 
        (sprintf "Unstacked too much, do not know what to do now: %S" 
           current_text)
  in
  let toplevel = (snd (List.hd_exn environments)) in
  toplevel.on_begin () |! write;
  loop [ toplevel ] "";
  (Buffer.contents buffer)
    
let bufferise_and_do f =
  let buffer = Buffer.create 42 in
  ((fun s -> Buffer.add_string buffer s; ""),
   (fun () ->
     let stuff_done = f (Buffer.contents buffer) in
     Buffer.clear buffer; stuff_done))

let is_whitespace s =
  try
    String.iter ~f:(function
      | ' ' | '\n' | '\r' | '\t' -> ()
      | c -> raise Exit) s;
    true
  with Exit -> false

let strip_lines s =
  let lines = 
    List.map ~f:(fun s -> s ^ "\n") (String.split s ~on:'\n') in
  List.rev (List.drop_while ~f:is_whitespace 
              (List.rev (List.drop_while ~f:is_whitespace lines)))
  |! String.concat ~sep:""

let caml fmt =
  ("caml",  
   (let on_text, on_end =
      let cmd = 
        sprintf "source-highlight -s caml -f %s" 
          (match fmt with `html -> "xhtml" | `latex -> "latex") in
      bufferise_and_do (fun input ->
        if is_whitespace input then "# Removed whitespace\n"
        else
          let input = strip_lines input in
          "{bypass endanywebcode}" ^ 
            (Dbw_sys.feed ~cmd ~input) ^ "{endany" ^ "webcode}") in
    environment  ~on_text ~on_end ~on_change:on_end
      ("[ca" ^ "ml[") ("]ca" ^ "ml]") [ "bracetax" ]))
let camlbrtx fmt = [
  caml fmt;
  "bracetax", environment ("(*" ^ "B") ("B" ^ "*)") [ "caml" ];
]

let coqbrtx fmt = 
  let coqdoc =
    let file_name = "/tmp/coqbrtx" in
    sprintf
      "cat > %s.v ; coqdoc -s -utf8 --parse-comments --stdout \
        --body-only --no-externals --no-index %s %s.v \
        2> %s.log" file_name
      (match fmt with `html -> "--html" | `latex -> "--latex")
      file_name file_name
  in 
  let coq name start stop =
    (name,  
     (let on_text, on_end =
        bufferise_and_do (fun input ->
          if is_whitespace input then "# Removed whitespace\n"
          else
            let input = strip_lines input in
            let hash = Digest.string input |! Digest.to_hex in
            (sprintf "{bypass endanywebbypass%s}<div class=\"%s\">%s\
                      </div>{endanywebbypass%s}"
              hash name (Dbw_sys.feed ~cmd:coqdoc ~input) hash)) in
      environment ~on_text ~on_end ~on_change:on_end
        start stop [ "bracetax" ])) in
  
  [ coq "maincoq" "[maincoq[" "]maincoq]"; 
    coq "coqexample" "[coq[" "]coq]"; 
    caml fmt; 
    "bracetax", 
      environment ("(*" ^ "B") ("B" ^ "*)") [ "maincoq"; "coqexample"; "caml" ];
  ]

let coq_brtx ?(fmt:[`html | `latex]=`html) str =
  transform (coqbrtx fmt) str
let ocaml_brtx ?(fmt:[`html | `latex]=`html) str =
  transform (camlbrtx fmt) str
