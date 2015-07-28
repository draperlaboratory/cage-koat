
module CR = Comrule

module Branches = Map.Make(String)

type head = Term.funSym

type tailElement = {
  left : Term.term;
  right : Term.term;
  guard : Pc.cond;
}

type tail = tailElement list

let (bottom : Term.term) = ("Bottom", [])

let tailElementToString (te : tailElement) =
  Printf.sprintf "%s -> %s :|: %s"
    (Term.toString te.left)
    (Term.toString te.right)
    (Pc.toString te.guard)


let tailToString (t : tail) =
  List.fold_left
    (fun accum el -> Printf.sprintf "%s\n\t%s" accum
      (tailElementToString el)) "" t

let displayMap key element = tailToString element |> Printf.eprintf "%s%s\n" key


let addBottom (t : tail) =
  let bottomGuard = List.fold_left (fun accum e ->
    (* I need to negate the guard for this to really be right *)
    e.guard @ accum) [] t
  in
  let left = (List.hd t).left in (* t is guaranteed to have at least on element. *)
  let possible = List.length bottomGuard > 0 && true in
  if possible then
    {left = left; right = bottom; guard = bottomGuard; } :: t else
    t


let rec processRelationships map = function
  | [] -> Branches.map addBottom map
  | hd::tl ->
    let (funSym, _) = hd.CR.lhs
    and tailEls = List.map (fun rhs ->
      { left = hd.CR.lhs; right = rhs; guard = hd.CR.cond; }) hd.CR.rhss in
    let prev =
      try
        Branches.find funSym map
      with Not_found -> [] in
    let toAdd = tailEls @ prev in
    assert(toAdd <> []);
    processRelationships (Branches.add funSym toAdd map) tl


let main () =
  let usage = "" in
  let filename = ref "" in
  Arg.parse [] (fun f -> filename := f) usage;
  if !filename = "" then
    begin
      Printf.eprintf "Expected filename as only argument.\n";
      exit 1
    end
  else
    begin
      Printf.printf "Detect Branch %s\n\n" !filename;
      let _, system = Parser.parseCint !filename Simple.Stmts in
      processRelationships Branches.empty system |> Branches.iter displayMap
    end

let _ = main ()
