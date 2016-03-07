type range = {
  start : int;
  stop : int;
}

type loopIndex = int

type segment =
| Linear of range
| Loop of (loopIndex * range)

type program = segment list

(* Converts a ChainLoop program into an equivalent ITS *)
val programToITS : program -> Comrule.rule list

(* Read a program (represented as JSON) in from a file. *)
val pathToProgram: string -> program

(* Write a program out to a file using JSON for the representation *)
val programToFile: string -> program -> unit
