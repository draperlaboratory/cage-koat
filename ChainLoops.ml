type range = {
  start : int;
  stop : int;
}

type loopIndex = int

type sType =
| LinSeg
| LoopSeg

type segment =
| Linear of range
| Loop of (loopIndex * range)

type program = segment list

let rec listHelp = function
  | 0 -> [0]
  | x -> x :: listHelp (x - 1)

let list i =
  List.rev (listHelp i)

let getFname i = Printf.sprintf "f%i" i

let argHelper = function
  | 0 -> "a"
  | 1 -> "b"
  | 2 -> "c"
  | 3 -> "d"
  | 4 -> "e"
  | 5 -> "g"
  | 6 -> "h"
  | 7 -> "i"
  | 8 -> "j"
  | 9 -> "k"
  | 10 -> "l"
  | 11 -> "m"
  | 12 -> "n"
  | 13 -> "o"
  | 14 -> "p"
  | 15 -> "q"
  | 16 -> "r"
  | 17 -> "s"
  | 18 -> "t"
  | 19 -> "u"
  | 20 -> "v"
  | 21 -> "w"
  | 22 -> "y"
  | 23 -> "z"
  | x -> Printf.sprintf "x%i" (x - 24)

let arg i =
  Poly.fromVar (argHelper i)

let argDecrement target this i =
  let thisArg = arg i in
  if target = this
  then Poly.minus thisArg Poly.one
  else thisArg

let lefthandTerm arity i =
  let args = List.map arg (list (arity - 1)) in
  let fn = getFname i in
  { Term.fn = fn; Term.args = args; }

let straightSegment arity startIndex =
  let lhs = lefthandTerm arity startIndex in
  let rhs = lefthandTerm arity (startIndex + 1) in
  Comrule.createRule lhs [rhs] []

let closeLoop arity loopIndex loopStart thisIndex =
  let argNums = list (arity - 1) in
  let lhs = lefthandTerm arity thisIndex
  and rhs = { Term.fn = getFname loopStart;
              args = List.mapi (argDecrement loopIndex) argNums; }
  and cond = Pc.Geq (arg loopIndex, Poly.zero) in
  Comrule.createRule lhs [rhs] [cond]

let exitLoop arity loopIndex startIndex =
  let lhs = lefthandTerm arity startIndex
  and rhs = lefthandTerm arity (startIndex + 1)
  and cond = Pc.Equ (arg loopIndex, Poly.zero) in
  Comrule.createRule lhs [rhs] [cond]

let rec buildStraight arity sindex = function
  | 0 -> []
  | i -> straightSegment arity sindex :: buildStraight arity (sindex + 1) (i - 1)

let rec buildLoop arity loopIndex loopStart loopEnd = function
  | i when i = loopStart && i = loopEnd ->
    [exitLoop arity loopIndex loopStart; closeLoop arity loopIndex loopStart loopEnd]
  | i when i = loopStart ->
    exitLoop arity loopIndex loopStart :: buildLoop arity loopIndex loopStart loopEnd (i + 1)
  | i when i = loopEnd ->
        closeLoop arity loopIndex loopStart loopEnd :: []
  | i ->
    straightSegment arity i :: buildLoop arity loopIndex loopStart loopEnd (i + 1)

let rec computeArityHelper = function
  | [] -> 0
  | Linear _ :: tl -> computeArityHelper tl
  | Loop (_,r) :: tl -> 1 + computeArityHelper tl

let computeArity prgn =
  (* I want all programs to have at least arity 1, even if they're linear. *)
  max 1 (computeArityHelper prgn)

let validSegment = function
  | Linear r
  | Loop (_,r) ->
    r.start <= r.stop && r.start >= 0

let segmentToComrule arity = function
  | Loop (lInd, r) -> buildLoop arity lInd r.start r.stop r.start
  | Linear r ->  buildStraight arity r.start (r.stop - r.start)

let buildEntry arity =
  let args = List.map arg (list (arity - 1)) in
  let lhs = { Term.fn = "start"; Term.args = args}
  and rhs = { Term.fn = "f0"; Term.args = args; } in
  Comrule.createWeightedRule lhs [rhs] [] Poly.zero Poly.zero

let programToITS = function
  | [] -> failwith "Can't convert an empty program."
  | p ->
    let arity = computeArity p in
    let convSeg = segmentToComrule arity in
    List.fold_left (fun accum pSeg -> convSeg pSeg @ accum) [buildEntry arity] p

(* I/O *)

let rangeToJSON r =
  let open Yojson.Basic.Util in
  `Assoc [ ("start", `Int r.start);
           ("stop", `Int r.stop); ]

let segmentToJSON s =
  let open Yojson.Basic.Util in
  match s with
  | Linear r ->
    `Assoc [ ("type", `String "LINEAR");
             ("range", rangeToJSON r); ]
  | Loop (lInd, r) ->
    `Assoc [ ("type", `String "LOOP");
             ("range", rangeToJSON r);
             ("loopIndex", `Int lInd); ]

let programToJSON = function
  | [] -> failwith "Attempted to convert empty program to json."
  | p ->
    let open Yojson.Basic.Util in
    `Assoc [ ("program", `List (List.map segmentToJSON p)) ]

let jsonToRange j =
  let open Yojson.Basic.Util in
  { start = j |> member "start" |> to_int;
    stop = j |> member "stop" |> to_int; }

let jsonToSegType j =
  let open Yojson.Basic.Util in
  let typeString = j |> member "type" |> to_string |> String.uppercase in
  match typeString with
  | "LINEAR" -> LinSeg
  | "LOOP" -> LoopSeg
  | _ -> failwith (Printf.sprintf "Unrecognized program segment type: %s" typeString)

let jsonToSegment j =
  let open Yojson.Basic.Util in
  match jsonToSegType j with
  | LinSeg -> Linear (jsonToRange j)
  | LoopSeg -> Loop (j |> member "loopIndex" |> to_int, jsonToRange j)

let jsonToProgram j =
  let open Yojson.Basic.Util in
  j |> member "program" |> to_list |> List.map jsonToSegment

let pathToProgram filePath =
  Yojson.Basic.from_file filePath |> jsonToProgram

let programToFile filePath p =
  programToJSON p |> Yojson.Basic.to_file filePath

(*** Test Code beyond here ***)

(* some simple instances *)
let straightInstance = [Linear { start = 0; stop = 10; }]

let loopingInstance1 = [ Linear { start = 0; stop = 1; };
                         Loop (0, { start = 1; stop = 1; });
                         Linear { start = 2; stop = 10; }; ]
let loopingInstance2 = [Loop (0, {start = 0; stop = 0; })]
let loopingInstance3 = [Loop (0, {start = 1; stop = 1; })]
let loopingInstance4 = [Loop (0, {start = 0; stop = 0; });
                        Loop (1, {start = 1; stop = 1; });
                        Loop (2,{start = 2; stop = 5; });
                        Linear {start = 5; stop = 10; };]

let main () =
  let printCR cr = Printf.printf "%s\n" (Comrule.toString cr) in
  Printf.printf "Chain loops debugger\n";
  Printf.printf "\nStraight line instance:\n";
  List.iter printCR (programToITS straightInstance);
  Printf.printf "\nLoop instance 1:\n";
  List.iter printCR (programToITS loopingInstance1);
  Printf.printf "\nLoop instance 2:\n";
  List.iter printCR (programToITS loopingInstance2);
  Printf.printf "\nLoop instance 3:\n";
  List.iter printCR (programToITS loopingInstance3);
  Printf.printf "\nLoop instance 4:\n";
  List.iter printCR (programToITS loopingInstance4);
  Printf.printf "Goodbye!\n\n"


let _ = main ()
