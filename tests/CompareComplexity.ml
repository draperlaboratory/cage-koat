type complexity =
  | Unknown
  | Result of Expexp.expexp

type compare_direction =
  | Tighter
  | Looser

type compare_result =
  | Exact
  | SameMagnitude of compare_direction
  | DifferentMagnitude of compare_direction

let dir_to_string = function
  | Tighter -> "Tighter"
  | Looser -> "Looser"

let comp_to_string = function
  | Unknown -> "Unknown"
  | Result s -> Expexp.toString s

let compare_result_to_string = function
  | Exact -> "Exact"
  | SameMagnitude dir -> "Same Magnitude " ^ (dir_to_string dir)
  | DifferentMagnitude dir -> "Different Magnitude " ^ (dir_to_string dir)

let base_string = "Complexity upper bound "
let unknown_comp = '?'
exception ParseException of int * int * string

let getexp suffix =
  let buff = Lexing.from_string suffix in
  try
    Complexity_parser.exp Complexity_lexer.token buff
  with
    | Parsing.Parse_error ->
        raise
          (
            ParseException
              (
                !Complexity_lexer.line,
                !Complexity_lexer.pos,
                Printf.sprintf "Error: Parse error in %s at position %d." suffix !Complexity_lexer.pos
              )
          )
    | Complexity_lexer.Unknown ->
        raise
          (
            ParseException
              (
                !Complexity_lexer.line,
                !Complexity_lexer.pos,
                Printf.sprintf "Error: Unknown token in %s at position %d. " suffix !Complexity_lexer.pos
              )
          )

let complexity_string s =
  (* string -> complexity option *)
  let len = String.length base_string in
  try
    let prefix = String.sub s 0 len in
    if (String.compare prefix base_string) = 0 then
      let suffix = String.sub s len ((String.length s) - len) in
      if suffix.[0] = unknown_comp then
        Some Unknown
      else
        begin
          Some (Result (getexp suffix))
        end
    else None
  with Invalid_argument _ -> None


let rec find_complexity_string = function
  (* [string] -> complexity *)
  | [] -> failwith "No complexity string in string list."
  | hd::tl -> begin
    match complexity_string hd with
    | Some res -> res
    | None -> find_complexity_string tl
  end


let getDirection p1 p2 =
  let comp = Expexp.compare p1 p2 in
  if comp = 1 then Tighter
  else if comp = -1 then Looser
  else
    failwith (Printf.sprintf "Got unexpected comparison result %i" comp)


let compare older newer =
  match (older, newer) with
  | Unknown, Unknown -> Exact
  | Unknown, _ -> DifferentMagnitude Tighter
  | _, Unknown -> DifferentMagnitude Looser
  | Result oldPoly, Result newPoly ->
     if Expexp.equal oldPoly newPoly then
       Exact else
       begin
         let oldDegree = Expexp.getDegree oldPoly
         and newDegree = Expexp.getDegree newPoly in
         match oldDegree, newDegree with
         | (Expexp.Polynomial i, Expexp.Polynomial j) when i = j -> SameMagnitude (getDirection oldPoly newPoly)
         | (Expexp.Exponential i, Expexp.Exponential j) when i = j -> SameMagnitude (getDirection oldPoly newPoly)
         | _ -> DifferentMagnitude (getDirection oldPoly newPoly)
       end

let lines_of_file path =
  let lines = ref [] in
  let chan = open_in path in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    [] (* can never get here *)
  with End_of_file ->
    close_in chan;
    !lines

let comp_of_file path =
  find_complexity_string (lines_of_file path)

let compare_files path1 path2 =
  let comp1 = comp_of_file path1
  and comp2 = comp_of_file path2 in
  compare comp1 comp2

(*** The main function setup ***)

let old_path = ref ""
let new_path = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>"

let rec speclist =
  [("-help", Arg.Unit (fun () -> print_usage (); exit 1),
    "            - Display this list of options");
   ("--help", Arg.Unit (fun () -> print_usage (); exit 1), "");
   ("--old", Arg.Set_string old_path, "");
   ("--new", Arg.Set_string new_path, "");]

and print_usage () =
  Arg.usage speclist usage


let main() =
  Arg.parse speclist (fun _ -> ()) usage;
  let result = compare_files !old_path !new_path in
  Printf.printf "%s\n" (compare_result_to_string result);
  match result with
  | Exact
  | SameMagnitude _
  | DifferentMagnitude Tighter ->
     exit 0
  | _ -> begin
    Printf.printf "Result got worse!\n";
    exit 1
    end


let _ = main ()
