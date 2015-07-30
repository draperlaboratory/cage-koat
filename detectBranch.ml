
module CR = Comrule
open DepStructs

module Branches = Map.Make(String)

type head = Term.funSym

type tailElement = {
  left : Term.term;
  right : Term.term;
  guard : Pc.cond;
}

type tail = tailElement list

let (bottom : Term.term) = ("Bottom", [])

let argPosInfluencesBranch argPos te =
  let inLeft = (fst te.left) = argPos.fName
  and inRight = (fst te.right) = argPos.fName in
  let argument =
    if inLeft then
      Some (List.nth (snd te.left) argPos.pos )
    else if inRight then
      Some (List.nth (snd te.right) argPos.pos )
    else None in
  match argument with
  | None -> false
  | Some plist -> Pc.shareVars plist te.guard

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
  (* we now have a conjunct of disjuncts [x0 /\ x1 /\ ... xn] *)
  let disjuncts = List.map (fun e -> Pc.negateCond e.guard) t in
  (* now we've got [x0 \/ x1 \/ ... xn, we need to make that many branches to
     bottom!*)
  let conjuncts = Pc.andOverOr disjuncts in
  let left = (List.hd t).left in (* t is guaranteed to have at least on element. *)
  let bottoms accum bottomGuard =
    let possible = List.length bottomGuard > 0 && true in
    if possible then
      {left = left; right = bottom; guard = bottomGuard; } :: accum else
     accum in
  (* we may want to reduce, combine branches eventually, but not now. *)
  List.fold_left bottoms t conjuncts

let branches _ = function
  | []
  | [_] -> false
  | _ -> true

let hasBranches map = Branches.exists branches map
let branchingOnly map = Branches.filter branches map
let branchingKeys map =
  Branches.fold (fun k el ac -> if branches k el then k::ac else ac) map []


let rec processRelationshipsInt map = function
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
    processRelationshipsInt (Branches.add funSym toAdd map) tl

let processRelationships lst = processRelationshipsInt Branches.empty lst


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
      let branchMap = processRelationships system in
      let leadToBranch = branchingKeys branchMap in
      Branches.iter displayMap branchMap;
      List.iter (Printf.eprintf "%s leads to a branch\n") leadToBranch
    end

let _ = main ()
