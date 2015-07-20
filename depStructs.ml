type qual =
| Equal
| Delta
| Unkown

type ruleTrans = {
  lName : string;
  lpos : int;
  rName : string;
  rpos : int;
  qual : qual;
}
