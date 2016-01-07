let filename = ref ""
let combine = ref SimpleT.Ctrls
let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>"


let rec speclist =
  [("-help", Arg.Unit (fun () -> print_usage (); exit 1),
     "            - Display this list of options");
    ("--help", Arg.Unit (fun () -> print_usage (); exit 1), "");
    ("-log", Arg.Int (fun i -> Log.logging_level := i),
     Printf.sprintf "             - Print live log (level 1) or debug (level 5) output during proof [default %i]" !Log.logging_level);
    ("--log", Arg.Int (fun i -> Log.logging_level := i), "");
  ]
and print_usage () =
  Arg.usage speclist usage


let main () =
  Arg.parse speclist (fun f -> filename := f) usage;
  if !filename = "" then
  (
    Printf.printf "%s\n" (Sys.argv.(0) ^ ": need a filename.");
    print_usage ();
    exit 1
  )
  else
    let (startFun, cint) = Cint_aux.readIn !filename in
    let noDups = Cint_aux.remdup cint in
    let fixedArity = Comrule.fixArity noDups in
    let args = Comrule.getArgs fixedArity in
    Printf.fprintf stdout "(GOAL COMPLEXITY)\n";
    Printf.fprintf stdout "(STARTTERM (FUNCTIONSYMBOLS %s))\n" startFun;
    Printf.fprintf stdout "(VAR";
    List.iter (Printf.fprintf stdout " %s") args;
    Printf.fprintf stdout ")\n(RULES\n";
    List.iter (fun cr -> Printf.fprintf stdout "  %s\n" (Comrule.toString cr)) fixedArity;
    Printf.fprintf stdout ")\n\n"

let _ = main ()
