open Dibrawi_std

type style = Latex | Text | MathML

type token = style -> string

type expression =
  | Range_op of token * expression * expression * expression * expression
  | Fraction of expression * expression
  | Paren of expression
  | Bin_op of token * expression * expression
  | Variable of token
  | Literal of token
  | Apply of expression * expression list
  | Sup of expression * expression
  | Sub of expression * expression
  | Nil

let either l t m = function Latex -> l | Text -> t | MathML -> m
let string s = function _ -> s


let rec to_string style expr =
  match expr with
  | Range_op (op, i, f, t, s) ->
    begin match style with
    | Latex -> (sprintf "%s_{%s=%s}^{%s}{%s}" (op style)
                  (to_string style i) (to_string style f)
                  (to_string style t) (to_string style s))
    | Text -> 
      (sprintf "%s[%s=%s..%s] %s" (op style)
         (to_string style i) (to_string style f)
         (to_string style t) (to_string style s))
    | MathML -> (sprintf "\
                  <munderover>\n\
                    <mrow><mo>%s</mo></mrow>\n\
                    <mrow>%s<mo>=</mo>%s</mrow>\n\
                    <mrow>%s</mrow></munderover><mrow>%s</mrow>\n\
                  "
                   (op style) (to_string style i) (to_string style f) 
                   (to_string style t) (to_string style s))
    end
  | Fraction (a, b) ->
    let sa = to_string style a in
    let sb = to_string style b in
    begin match style with
    | Latex -> (sprintf "\\frac{%s}{%s}" sa sb)
    | Text -> (sprintf "[%s] / [%s]" sa sb)
    | MathML -> (sprintf "<mfrac>\n<mrow>%s</mrow>\n<mrow>%s</mrow>\n</mfrac>" sa sb)
    end
  | Paren f -> 
    begin match style with
    | Latex -> (sprintf "(%s)" (to_string style f))
    | Text -> (sprintf "(%s)" (to_string style f))
    | MathML -> (sprintf "\n<mo>(</mo>%s<mo>)</mo>\n" (to_string style f))
    end
  | Bin_op (op, a, b) ->
    let sop = op style in
    let sa = to_string style a in
    let sb = to_string style b in
    begin match style with
    | Latex -> (sprintf "%s %s %s" sa sop sb)
    | Text -> (sprintf "%s %s %s" sa sop sb)
    | MathML -> (sprintf "%s<mo>%s</mo>%s" sa sop sb)
    end
  | Variable v ->
    let s = v style in 
    begin match style with 
      Latex | Text-> s | MathML -> (sprintf "<mi>%s</mi>" s) end
  | Literal l -> 
    let s = l style in
    begin match style with 
      Latex | Text-> s | MathML -> (sprintf "<mn>%s</mn>" s) end
  | Apply (f, l) ->
    let sf = to_string style f and 
        sl = List.map (to_string style) l in
    begin match style with
    | Latex -> (sprintf "%s(%s)" sf (String.concat ", " sl))
    | Text -> (sprintf "%s(%s)" sf (String.concat ", " sl))
    | MathML -> 
      (sprintf "\n%s<mo>(</mo>%s<mo>)</mo>\n" sf 
         (String.concat "<mo>,</mo>" sl))
    end
  | Sup (a, b) ->
    let sa = to_string style a and sb = to_string style b in
    begin match style with
    | Latex -> (sprintf "%s^{%s}" sa sb)
    | Text -> (sprintf "%s^{%s}" sa sb)
    | MathML -> 
      (sprintf "\n<msup>\n  <mrow>%s</mrow>\n  <mrow>%s</mrow>\n</msup>"
         sa sb)
    end
  | Sub (a, b) ->
    let sa = to_string style a and sb = to_string style b in
    begin match style with
    | Latex -> (sprintf "%s_{%s}" sa sb)
    | Text -> (sprintf "%s_{%s}" sa sb)
    | MathML ->
      (sprintf "\n<msub>\n  <mrow>%s</mrow>\n  <mrow>%s</mrow>\n</msub>" 
         sa sb)
    end
  | Nil -> ""

module Render = struct

  let inline style expr =
    begin match style with
    | Latex -> (sprintf "$%s$" (to_string style expr))
    | Text -> (to_string style expr)
    | MathML -> (sprintf "<math display='inline'>%s</math>"
                   (to_string style expr))
    end

  let block style expr =
    begin match style with
    | Latex -> (sprintf "$$%s$$" (to_string style expr))
    | Text -> (to_string style expr)
    | MathML -> (sprintf "<math display='block'>%s</math>"
                   (to_string style expr))
    end

  let array_latex transform a =
    let transformed =
      List.map (fun row -> 
        (String.concat " & " (List.map transform row))) a in
    sprintf "\\begin{eqnarray*}%s\n\\end{eqnarray*}"
      (String.concat "\\\\ " transformed)

  let array_text transform a =
    let col_maxes = Array.create (Ls.length (Ls.hd a)) 0 in
    let transformed =
      Ls.map (fun row ->
        Ls.mapi (fun i cell ->
          let s = transform cell in 
          let l = String.length s in
          if l > col_maxes.(i) then col_maxes.(i) <- l;
          s
        ) row) a in
    let padded =
      Ls.map (fun row ->
        String.concat " " 
          (Ls.mapi (fun i cell ->
            cell ^ (String.make (col_maxes.(i) - (Str.length cell)) ' ')) row)
      ) transformed in
    sprintf "\n%s\n" (String.concat "\n" padded)

 let array_mathml transform a =
   let transformed =
     List.map (fun row -> 
       sprintf "<mtr>%s</mtr>\n"
         (String.concat ""
             (List.map (fun f -> 
               let s = transform f in
               sprintf "<mtd>%s</mtd>\n" s) row))) a in
   sprintf "<math display='block'><mtable>%s</mtable></math>"
     (String.concat "" transformed)

 let array style exprs =
   match style with
   | Latex -> array_latex (to_string style) exprs
   | Text -> array_text (to_string style) exprs
   | MathML -> array_mathml (to_string style) exprs
      
end


module Library = struct

  let sum_op = either "\\sum" "Sum" "&#x2211;"

  let bin_add = string "+"
  let bin_sub = string "-"
  let bin_mul = either "\\cdot" "⋅" "⋅"
  let bin_div = string "/" (* "÷" "" "÷" *)
  let bin_mod = string "mod"
  let bin_eq  = either "=" "=" "="
  let bin_ne  = either "\\neq" "!=" "≠"
  let bin_le  = either "\\le"  "<=" "≤"
  let bin_ge  = either "\\ge"  ">=" "≥"
  let bin_lt  = either "<" "<" "<"
  let bin_gt  = either ">" "<" ">"
  let bin_imply       = either "\\Rightarrow"     "=>"  "⇒"
  let bin_and         = either "\\wedge"          "/\\" "∧"
  let bin_or          = either "\\vee"            "\\/" "∨"
  let bin_equivalent  = either "\\Leftrightarrow" "<=>" "⇔"
  let bin_custom (t, l, m) = either t l m

end


module Constructors = struct
  open Library

  let sum i f t s = Range_op (sum_op, i, f, t, s)
  let frac a b = Fraction (a, b)
  let var s = Variable (string s)
  let lit s = Literal (string s)
  let cvar l t m = Variable (either l t m)
  let clit l t m = Literal (either l t m)
  let int s = Literal (string (string_of_int s))
  let float s = Literal (string (string_of_float s))

  let (+) a b = Bin_op (bin_add, a, b)
  let ( * ) a b = Bin_op (bin_mul, a, b)
  let (-) a b = Bin_op (bin_sub, a, b)
  let (/) a b = Bin_op (bin_div, a, b)
  let (mod) a b = Bin_op (bin_mod, a, b)
  let bin t l m a b = Bin_op (bin_custom (t, l, m), a, b)
  let par f = Paren f
  let app f l = Apply (f, l)
  let (==) a b = Bin_op (bin_eq , a, b)
  let (!=) a b = Bin_op (bin_ne , a, b)
  let (<=) a b = Bin_op (bin_le , a, b)
  let (>=) a b = Bin_op (bin_ge , a, b)
  let (< ) a b = Bin_op (bin_lt , a, b)
  let (> ) a b = Bin_op (bin_gt , a, b)
  let (=>  ) a b = Bin_op (bin_imply  , a, b)
  let (&&  ) a b = Bin_op (bin_and   , a, b)
  let (||  ) a b = Bin_op (bin_or    , a, b)
  let (<=> ) a b = Bin_op (bin_equivalent, a, b)
  let sup a b = Sup (a, b)
  let sub a b = Sub (a, b)
    
  let nilbin f = f Nil Nil
  let nil = Nil 
    
  let ( << ) x y = y x 
  and ( >> ) x y = x y 
    
end
