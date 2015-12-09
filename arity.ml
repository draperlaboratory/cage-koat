
let test fname =
  let (startFun, cint) = Parser.parseCint fname SimpleT.Ctrls in
  List.iter (fun r -> Printf.eprintf "%s\n" (Comrule.toString r)) cint;
  let cint' = Comrule.fixArity cint in
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
