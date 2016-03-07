type range = {
  start : int;
  stop : int;
}

type Segment =
| Linear of range
| Loop of range

type program = Segment list

val programToITS : program -> rule list
