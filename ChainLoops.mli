type range = {
  start : int;
  stop : int;
}

type segment =
| Linear of range
| Loop of range

type program = segment list

val programToITS : program -> Comrule.rule list
