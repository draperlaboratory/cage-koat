(* Entry point for information flow module *)

let usage = ""

let main () =
  let filename = ref "" in
  Arg.parse [] (fun f -> filename := f) usage;
  if !filename = "" then
    begin
      Printf.eprintf "Expected filename as only argument.\n";
      exit 1
    end
  else
    begin
      Printf.printf "Processing %s\n\n" !filename;
      let entrFun, system = Parser.parseCint !filename Simple.Stmts in
      Printf.printf "%s\n" (Cint.toString system);
      Printf.printf "Starting at %s\n" entrFun
    end

let _ = main()
