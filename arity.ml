let rec maximumArity = function
  | [] -> 0
  | hd::tl -> max (Term.getArity hd.Comrule.lhs) (maximumArity tl)

let pad maxArity term =
  let pel = Poly.fromVar "ArityPad" in
  let rec makePad = function
    | 0 -> []
    | x -> pel::(makePad (x - 1)) in
  let toAdd = Term.getArity term in
  let padding = makePad toAdd in
  { Term.fn = term.Term.fn;
    Term.args = term.Term.args @ padding; }

let padCR maxArity cr =
  let p = pad maxArity in
  { cr with
    Comrule.lhs = p cr.Comrule.lhs;
    Comrule.rhss = List.map p cr.Comrule.rhss ; }

let fixArity cint =
  let maxArity = maximumArity cint in
  List.map (padCR maxArity) cint

let test fname =
  let (startFun, cint) = Parser.parseCint fname SimpleT.Ctrls in
  List.iter (fun r -> Printf.eprintf "%s\n" (Comrule.toString r)) cint;
  let cint' = fixArity cint in
  List.iter (fun r -> Printf.eprintf "%s\n" (Comrule.toString r)) cint';
  cint, cint'

let main () =
  let filename = ref "" in
  Arg.parse [] (fun f -> filename := f) "";
  if !filename = "" then
    begin
      Printf.eprintf "%s\n" (Sys.argv.(0) ^ ": need a filename.");
      exit 1
    end
  else
    test !filename

let _ = main()
