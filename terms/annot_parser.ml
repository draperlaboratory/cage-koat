open Yojson.Basic
open Yojson.Basic.Util
open Annot

(********** Example input ***************)



let parseTime (timeJson : json) =
  match timeJson with
    `String s -> s
  | _ -> failwith "Excepted the time to be a json string, but it wasn't!"

let fetchAssoc (recordFields : (string * json) list) (key : string) =
  try
    List.assoc key recordFields
  with Not_found -> failwith (Printf.sprintf "Couldn't find %s" key)

let parseComplexityString (s : string) =
  let s' = String.uppercase s in
  match s' with
  | "UNKNOWN" -> Complexity.Unknown
  | "CONSTANT" -> Complexity.P Expexp.one
  | _ ->
    let lexbuf = Lexing.from_string s in
    let result = Cint_parser.poly Cint_lexer.token lexbuf in
    Complexity.P (Expexp.Pol (Poly.construct_poly result))

let parseComplexity (complexityRep : json) =
  match complexityRep with
  | `Assoc recordFields ->
    let get = fetchAssoc recordFields in
    let sizeString = to_string (get "size")
    and upCompString = to_string (get "upperTime")
    and lowCompString = to_string (get "lowerTime")
    and upMemString = to_string (get "upperSpace")
    and lowMemString = to_string (get "lowerSpace") in
    { size = parseComplexityString sizeString;
      upperTime = parseComplexityString upCompString;
      lowerTime = parseComplexityString lowCompString;
      upperSpace = parseComplexityString upMemString;
      lowerSpace = parseComplexityString lowMemString; }
  | _ -> failwith "Excepted Complexity Representation to be an association list!"

let parseType (typeRep : json) =
  Int (* stub *)

let parseArg (argRep : json) = { name = to_string argRep; jtype = Int }
  (* match argRep with *)
  (* | `Assoc recordFields -> *)
  (*   let get = fetchAssoc recordFields in *)
  (*   let name = to_string (get "varName") *)
  (*   and typeJson = get "varType" in *)
  (*   { name = name; *)
  (*     jtype = parseType typeJson; } *)
  (* | _ -> failwith "Expected Argument Representation to be an association list!!" *)

let parseArgList (argListRep : json) =
  List.map parseArg (to_list argListRep)

let parseSecretList (secretListRep : json) =
  List.map to_int (to_list secretListRep)

let parseFunction (functionRep : json) =
  match functionRep with
  | `Assoc recordFields ->
    let get = fetchAssoc recordFields in
    let nameJson = get "name"
    and argsJson = get "args"
    and secretJson = get "secretArgs"
    and complexityJson = get "complexity"
    and purityJson = get "pure" in
    { fname = to_string nameJson;
      args = parseArgList argsJson;
      secretArgs = parseSecretList secretJson;
      complexity = parseComplexity complexityJson;
      pure = to_bool purityJson }
  | _ -> failwith "Expected Function representation to be an association list!!"


let makeSpecMap l =
  List.fold_left (fun m f -> FMap.add f.fname f m) FMap.empty l

let parseFunctionList (functionsJson : json) =
  match functionsJson with
  | `List jsonObjs -> makeSpecMap (List.map parseFunction jsonObjs)
  | _ -> failwith "Expected functions list to be of type list."

let parsePackage (jsonObj : json) =
  match jsonObj with
  | `Assoc recordFields ->
    begin
      let get = fetchAssoc recordFields in
      let timeJson = get "created"
      and packageName = get "package"
      and functionList = get "functions" in
      let time = parseTime timeJson
        and functionList = parseFunctionList functionList in
        { pname = to_string packageName;
          created  = time;
          functions = functionList; }
    end
    | _ -> failwith "Tried Package representation to be an association list!!"


let parsePackageFile fname =
  let json = Yojson.Basic.from_file ~fname fname in
  parsePackage json
