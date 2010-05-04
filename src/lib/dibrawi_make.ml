

(*
TODO:
- add checking for no cyclic dependencies
  (how? is it possible to create?)

  let rec a = target ~build ~deps:[b] and b  = target ~build ~deps:[a] ;;
              ^^^^^^^^^^^^^^^^^^^^^^^
  Error: This kind of expression is not allowed as right-hand side of `let rec'



- do much more testing

- convert to purely functional

- translate to Coq

*)


type target = { 
  mutable done_once: bool;
  build: bool -> bool;
  deps: target list; }
    
let target ~build ~deps =
  { done_once = false; build = build; deps = deps }
    

let rec make_simple target =
  let there_is_one_true = List.fold_left (||) false in
  let results = List.map make_simple target.deps in
  target.build (there_is_one_true results)

let rec prepare_complex target = 
  target.done_once <- false;
  List.iter prepare_complex target.deps

let rec make_complex target =
  let there_is_one_true = List.fold_left (||) false in
  let results = List.map make_complex target.deps in
  if target.done_once then
    true (* even if already done, dependent targets must be rebuilt *)
  else
    let this_result = target.build (there_is_one_true results) in
    (target.done_once <- this_result; this_result)
      
let make ?(complex=true) target =
  if complex then
    (prepare_complex target; make_complex target)
  else
    make_simple target

type file_and_md5 = {
  mutable previous_md5: Digest.t option;
  filename: string;
}


type ('digest, 'content) digestible_content = {
  mutable current_digest: 'digest option;
  mutable current_content: 'content option;
}
let get_content dc = dc.current_content


let make_digestible_target
    ~(build_command: 'content option -> 'content)
    ~(digest: 'content option -> 'digest option)
    ?initial_content
    dependencies =
  let record_for_closure =
    match initial_content with
    | None -> { current_digest = None; current_content = None; }
    | Some c -> c in
  let build_and_propagate () =
    let new_content = Some (build_command record_for_closure.current_content) in
    let new_digest = digest new_content in
    (* If after rebuild, 'it' is still different from the previous,
       The parent need to force rebuild, and hence return true. *)
    if new_digest <> None &&
      record_for_closure.current_digest = new_digest then
        false
    else
      (record_for_closure.current_digest <- new_digest;
       record_for_closure.current_content <- new_content;
       true)
  in
  let build = function
    | true ->
        (* One dependency has changed, hence we force the rebuild. *)
        (build_and_propagate ())
    | false -> 
        (* Check if 'it' has changed. *)
        let current_digest = digest record_for_closure.current_content in
        if current_digest = None
          || record_for_closure.current_digest <> current_digest then
            (* 'It' has changed, so we have to rebuild. *)
            (build_and_propagate ())
        else 
          false
  in
  (target ~build ~deps:dependencies, record_for_closure)

module MD5 = struct  
  let make_string_target
      ?initial_content  ~build_cmd dependencies =
    let build_command = function
      | None -> build_cmd ()
      | Some s -> build_cmd () in
    let digest = function
      | None -> None
      | Some rs -> Some (Digest.string rs) in
    make_digestible_target ?initial_content ~build_command ~digest dependencies

  type file_content = (Digest.t, string) digestible_content

  let make_file_target ?initial_content ~filename  ~build_cmd dependencies =
    let md5_file file = try Some (Digest.file file) with _ -> None in
    let build_command = function
      | None -> (build_cmd ():unit); filename
      | Some s -> build_cmd (); filename in
    let digest = function
      | None -> None
      | Some rs -> md5_file rs in
    make_digestible_target ?initial_content ~build_command ~digest dependencies

  let make_phony_target ?initial_content ~name ~build_cmd dependencies =
    let build_command = function
      | None -> (build_cmd ():unit); name
      | Some s -> build_cmd (); name in
    let digest = fun _ -> None in
    make_digestible_target ?initial_content ~build_command ~digest dependencies
end

