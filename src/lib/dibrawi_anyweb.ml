
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
  List.fold_left
    (fun m k -> 
      match m, k with
      | None, x -> x
      | x, None -> x
      | Some (_, _, mb, _), Some (_, _, kb, _) ->
        if String.length mb <= String.length kb then m else k)
    None
    ((match Str.split_opt s current.end_token with 
      None -> None | Some (b, a) -> Some (true, current, b, a)) ::
        (List.map (fun env ->
          match Str.split_opt s env.start_token with
          | None -> None
          | Some (b, a) -> Some (false, env, b, a)) l))


let transform environments str =
  let buffer = Buffer.create 42 in
  let write = Buffer.add_string buffer in
  let next_line = 
    let l = ref (Str.nsplit str "\n") in
    (fun () -> match !l with [] -> None | h :: t -> l := t; Some (h ^ "\n")) in
  let rec loop stack current_text =
    match stack with
    | env :: l ->
      let inside = List.map (fun x -> List.assoc x environments) env.contains in
      begin match split_first current_text inside env with
      | Some (true, s, before, after) -> (* unstack *)
        env.on_text before |> write;
        env.on_end () |> write;
        loop l after
      | Some (false, s, before, after) -> (* stack *)
        env.on_text before |> write;
        env.on_change () |> write;
        s.on_begin () |> write;
        loop (s :: stack) after
      | None ->
        env.on_text current_text |> write;
        begin match next_line () with
        | Some line ->
          loop stack line
        | None -> env.on_end () |> write; ()
        end
      end
    | [] ->
      failwith 
        (sprintf "Unstacked too much, do not know what to do now: %S" 
           current_text)
  in
  let toplevel = (snd (List.hd environments)) in
  toplevel.on_begin () |> write;
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
    String.iter (function
      | ' ' | '\n' | '\r' | '\t' -> ()
      | c -> raise Exit) s;
    true
  with Exit -> false

let strip_lines s =
  let lines = 
    Ls.map (fun s -> s ^ "\n") (Str.nsplit s "\n") in
  List.rev (Ls.dropwhile is_whitespace 
              (List.rev (Ls.dropwhile is_whitespace lines)))
  |> Str.concat ""

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
    sprintf
      "cat > /tmp/ttt.v ; coqdoc -s --parse-comments --stdout \
        --body-only --no-externals --no-index %s /tmp/ttt.v"
      (match fmt with `html -> "--html" | `latex -> "--latex") in 
  let coq =
    ("coq",  
     (let on_text, on_end =
        bufferise_and_do (fun input ->
          if is_whitespace input then "# Removed whitespace\n"
          else
            let input = strip_lines input in
            "{bypass endanywebbypass}" ^ (Dbw_sys.feed ~cmd:coqdoc ~input)
            ^ "{endanywebbypass}") in
      environment ~on_text ~on_end ~on_change:on_end
        "[coq[" "]coq]" [ "bracetax" ])) in
  
  [ coq; caml fmt; 
    "bracetax", environment ("(*" ^ "B") ("B" ^ "*)") [ "coq"; "caml" ];
  ]

let coq_brtx ?(fmt:[`html | `latex]=`html) str =
  transform (coqbrtx fmt) str
let ocaml_brtx ?(fmt:[`html | `latex]=`html) str =
  transform (camlbrtx fmt) str
