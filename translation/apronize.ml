(*
  author: Michael Schaper

  Runs the ApronInvariantsProcessor on the original complexity problem and
  renders the new complexity problem again to stdout.
  In short, this module provides invariant generation for its problems.
*)


module RVG     = Cintfarkaspolo.RVG
module CTRS    = Cintfarkaspolo.CTRS
module CTRSObl = Cintfarkaspolo.CTRSObl
module LSC     = Cintfarkaspolo.LSC
module TGraph  = Cintfarkaspolo.TGraph
module Rule    = Comrule

open Rule
open CTRSObl
open CTRS

module UnreachableProc = DeleteUnreachableProc.Make(RVG)
module UnsatProc = DeleteUnsatProc.Make(RVG)

IFDEF HAVE_APRON THEN
module ApronInvariantsProc = ApronInvariantsProcessor.MakeKoatProc(RVG)
END

let doUnreachableRemoval = UnreachableProc.process
let doUnsatRemoval       = UnsatProc.process

let doApronInvariants ctrsobl tgraph rvgraph =
IFDEF HAVE_APRON THEN
  ApronInvariantsProc.process ctrsobl tgraph rvgraph
ELSE
  None
END
  ;;

let apply doit proc ((ctrsobl, tgraph, rvgraph) as prob) =
  if not doit
    then
      prob
    else
      match proc ctrsobl tgraph rvgraph with
        | None           -> prob
        | Some (prob',_) -> prob'
  ;;

let (|>) v f = f v ;;

let apronize ctrsobl simplify =
  let tgraph   = TGraph.compute ctrsobl.ctrs.rules in
  (* let lscs     = LSC.computeLocalSizeComplexities ctrsobl.ctrs.rules  in
  let rvgraph  = Some (RVG.compute lscs tgraph) in *)
  let rvgraph  = None in
  (ctrsobl,tgraph,rvgraph)
  |> apply simplify doUnsatRemoval
  |> apply simplify doUnreachableRemoval
  |> apply true     doApronInvariants
  |> apply simplify doUnsatRemoval
  |> apply simplify doUnreachableRemoval
  |> (fun (nctrsobl,_,_) -> nctrsobl)
  ;;

(* main *)
let filename = ref "" ;;
let simplify = ref true ;;

let rec args =
  [
    ("-simplify", Arg.Set simplify,
     "        - see --simplify");
    ("--simplify", Arg.Set simplify,
     "        - apply additional simplifications");
    ("--help", Arg.Unit (fun () -> print_usage (); exit 1), "")
  ]
and usage          = "usage: " ^ Sys.argv.(0) ^ " filename"
and print_usage () = Arg.usage args usage
  ;;

(* pretty printing in *)
let rec ppKoat ctrs =
  "(GOAL COMPLEXITY)\n"
  ^ Printf.sprintf "(STARTTERM (FUNCTIONSYMBOLS %s))\n" ctrs.startFun
  ^ Printf.sprintf "(VAR %s)\n"  (String.concat " " (CTRS.getVars ctrs))
  ^ Printf.sprintf "(RULES\n  %s\n)" (String.concat "\n  " (List.map ppRule ctrs.rules))
and ppRule rule =
  if Poly.equal rule.Rule.lowerBound Poly.one && Poly.equal rule.Rule.upperBound Poly.one
    then Rule.toString rule
    else
      (Term.toString rule.lhs)
      ^ Printf.sprintf " -{%s,%s}> "  (Poly.toString rule.lowerBound) (Poly.toString rule.upperBound)
      ^ Printf.sprintf "Com_%i(%s)" (List.length rule.rhss) (String.concat ", " (List.map Term.toString rule.rhss))
      ^ (if rule.cond = [] then "" else " [ " ^ (Pc.toString rule.cond) ^ " ]")
  ;;

let main () =
  Arg.parse args (fun f -> filename := f) usage;
  if !filename = ""
    then
      (
        print_usage ();
        exit 1
      )
    else
      let (startfun, cint) = Parser.parseCint !filename SimpleT.Ctrls in
      let ctrsobl  = getInitialObl cint startfun Complexity.Time in
      let nctrsobl = apronize ctrsobl !simplify in
      Printf.printf "%s" (ppKoat nctrsobl.ctrs);
      exit 0
  ;;

let _ = main () ;;

