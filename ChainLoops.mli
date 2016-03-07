type range = {
  start : int;
  stop : int;
}

type loopIndex = int

type segment =
| Linear of range
| Loop of (loopIndex * range)

type program = segment list

val programToITS : program -> Comrule.rule list
