type argPos = {
  fName : string;
  pos : int;
}

type qual =
| Equal
| Delta
| Unkown

type ruleTrans = {
  lPos : argPos;
  rPos : argPos;
  qual : qual;
}


let qualToString = function
  | Equal -> "Equal"
  | Delta -> "Delta"
  | Unkown -> "?"

let argPosToString ap =
  Printf.sprintf "%s %i" ap.fName ap.pos

let ruleTransToString rt =
  Printf.sprintf "%s %s %s"
    (argPosToString rt.lPos)
    (qualToString rt.qual)
    (argPosToString rt.rPos)
