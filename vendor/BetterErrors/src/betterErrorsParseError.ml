open BetterErrorsTypes
open Helpers
open OcamlRe

(* agnostic extractors, turning err string into proper data structures *)
(* TODO: don't make these raise error *)

(* get the diffing portion of two incompatible types, columns are 0-indexed *)
let stripCommonPrefix (l1, l2) =
  let i = ref 0 in
  while !i < List.length l1 && !i < List.length l2 && List.nth l1 !i = List.nth l2 !i do
    i := !i + 1
  done;
  (Helpers.listDrop !i l1, Helpers.listDrop !i l2)

let applyToBoth f (a, b) = (f a, f b)

let typeDiff a b =
  (* look ma, functional programming! *)
  (Helpers.stringNsplit a ~by:".", Helpers.stringNsplit b ~by:".")
  |> stripCommonPrefix
  |> applyToBoth List.rev
  |> stripCommonPrefix
  |> applyToBoth List.rev
  |> applyToBoth (String.concat ".")

let splitEquivalentTypes raw =
  try Some (Helpers.stringSplit raw ~by:"=")
  with Not_found -> None

let functionArgsCount str =
  (* the func type 'a -> (int -> 'b) -> string has 2 arguments *)
  (* strip out false positive -> from nested function types passed as param *)
  let nestedFunctionTypeR = Re_pcre.regexp {|\([\s\S]+\)|} in
  let cleaned = Re_pcre.substitute ~rex:nestedFunctionTypeR ~subst:(fun _ -> "|||||") str in
  (* TODO: allow pluggable function type syntax *)
  List.length (split {|->|} cleaned) - 1

(* need: where the original expected comes from  *)
(* TODO: when it's a -> b vs b, ask if whether user forgot an argument to the
func *)
let type_IncompatibleType err _ range =
  (* the type actual and expected might be on their own line *)
  (* sometimes the error msg might equivalent types, e.g. "myType = string isn't
  compatible to bla" *)
  let allR =
    (* This regex query is brought to you by debuggex.com. Get your free
    real-time regex visualization today. *)
    {|This expression has type([\s\S]*?)but an expression was expected of type([\s\S]*?)(Type\b([\s\S]*?)|$)?((The type constructor[\s\S]*?)|$)?((The type variable[\s\S]* occurs inside ([\s\S])*)|$)|}
  in
  let extraRaw = get_match_n_maybe 3 allR err in
  let extra = match extraRaw with
    | Some a -> if String.trim a = "" then None else Some (String.trim a)
    | None -> None
  in
  let actualRaw = get_match_n 1 allR err in
  let expectedRaw = get_match_n 2 allR err in
  let (actual, actualEquivalentType) = match splitEquivalentTypes actualRaw with
    | Some (a, b) -> (String.trim a, Some (String.trim b))
    | None -> (String.trim actualRaw, None)
  in
  let (expected, expectedEquivalentType) = match splitEquivalentTypes expectedRaw with
    | Some (a, b) -> (String.trim a, Some (String.trim b))
    | None -> (String.trim expectedRaw, None)
  in
  Type_IncompatibleType {
    actual = actual;
    expected = expected;
    differingPortion = typeDiff actual expected;
    (* TODO: actually use this *)
    actualEquivalentType;
    expectedEquivalentType;
    extra;
  }

(* TODO: differing portion data structure a-la diff table *)
let type_MismatchTypeArguments err _ _ =
  let allR = {|The constructor ([\w\.]*) *expects[\s]*(\d+) *argument\(s\),\s*but is applied here to (\d+) argument\(s\)|} in
  let typeConstructor = get_match_n 1 allR err in
  let expectedCount = int_of_string @@ get_match_n 2 allR err in
  let actualCount = int_of_string @@ get_match_n 3 allR err in
  Type_MismatchTypeArguments {
    typeConstructor = typeConstructor;
    expectedCount = expectedCount;
    actualCount = actualCount;
  }

(* need: if it's e.g. a module function, which part is not found? Module?
Function? *)
let type_UnboundValue err _ _ =
  let unboundValueR = {|Unbound value ([\w\.]*)|} in
  let unboundValue = get_match unboundValueR err in
  (* TODO: there might be more than one suggestion *)
  let suggestionR = {|Unbound value [\w\.]*[\s\S]Hint: Did you mean ([\s\S]+)\?|} in
  let suggestions =
    get_match_maybe suggestionR err
    |> Helpers.optionMap (Re_pcre.split ~rex:(Re_pcre.regexp {|, | or |}))
  in
  Type_UnboundValue {
    unboundValue = unboundValue;
    suggestions = suggestions;
  }

let type_SignatureMismatch err cachedContent = raise Not_found
let type_SignatureItemMissing err cachedContent = raise Not_found

let type_UnboundModule err _ _ =
  let unboundModuleR = {|Unbound module ([\w\.]*)|} in
  let unboundModule = get_match unboundModuleR err in
  let suggestionR = {|Unbound module [\w\.]*[\s\S]Hint: Did you mean (\S+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
  Type_UnboundModule {
    unboundModule = unboundModule;
    suggestion = suggestion;
  }

(* need: if there's a hint, show which record type it is *)
let type_UnboundRecordField err _ _ =
  let recordFieldR = {|Unbound record field (\w+)|} in
  let recordField = get_match recordFieldR err in
  let suggestionR = {|Hint: Did you mean (\w+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
    Type_UnboundRecordField {
      recordField = recordField;
      suggestion = suggestion
    }

let type_UnboundConstructor err cachedContent = raise Not_found

let type_UnboundTypeConstructor err _ _ =
  let constructorR = {|Unbound type constructor ([\w\.]+)|} in
  let constructor = get_match constructorR err in
  let suggestionR = {|Hint: Did you mean ([\w\.]+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
    Type_UnboundTypeConstructor {
      namespacedConstructor = constructor;
      suggestion = suggestion
    }

(* need: number of arguments actually applied to it, and what they are *)
(* need: number of args the function asks, and what types they are *)
let type_AppliedTooMany err _ _ =
  let functionTypeR = {|This function has type([\s\S]+)It is applied to too many arguments; maybe you forgot a `;'.|} in
  let functionType = String.trim (get_match functionTypeR err) in
  Type_AppliedTooMany {
    functionType = functionType;
    expectedArgCount = functionArgsCount functionType;
  }

let type_RecordFieldNotInExpression err cachedContent range = raise Not_found
let type_RecordFieldError err cachedContent range = raise Not_found
let type_FieldNotBelong err cachedContent range = raise Not_found

let type_NotAFunction err _ range =
  let actualR = {|This expression has type([\s\S]+)This is not a function; it cannot be applied.|} in
  let actual = String.trim (get_match actualR err) in
  Type_NotAFunction {
    actual = actual;
  }

(* TODO: apparently syntax error can be followed by more indications *)
(* need: way, way more information, I can't even *)
let file_SyntaxError err cachedContent range =
  let allR = Re_pcre.regexp {|Syntax error|} in
  (* raise the same error than if we failed to match *)
  if not (Re_pcre.pmatch ~rex:allR err) then
    raise Not_found
  else
    let hintR = {|Syntax error:([\s\S]+)|} in
    let hint = get_match_maybe hintR err in
    (* assuming on the same row *)
    let ((startRow, startColumn), (_, endColumn)) = range in
    File_SyntaxError {
      hint = Helpers.optionMap String.trim hint;
      offendingString = Helpers.stringSlice
        ~first:startColumn
        ~last:endColumn
        (List.nth cachedContent startRow);
    }

let build_InconsistentAssumptions err cachedContent range = raise Not_found

(* need: list of legal characters *)
let file_IllegalCharacter err _ _ =
  let characterR = {|Illegal character \(([\s\S]+)\)|} in
  let character = get_match characterR err in
  File_IllegalCharacter {
    character = character;
  }


let parsers = [
  type_MismatchTypeArguments;
  type_UnboundValue;
  type_SignatureMismatch;
  type_SignatureItemMissing;
  type_UnboundModule;
  type_UnboundRecordField;
  type_UnboundConstructor;
  type_UnboundTypeConstructor;
  type_AppliedTooMany;
  type_RecordFieldNotInExpression;
  type_RecordFieldError;
  type_FieldNotBelong;
  type_IncompatibleType;
  type_NotAFunction;
  file_SyntaxError;
  build_InconsistentAssumptions;
  file_IllegalCharacter;
]

let goodFileNameR = Re_pcre.regexp {|^[a-zA-Z]|}
let cannotFindFileRStr = {|Cannot find file ([\s\S]+)|}
let unboundModuleRStr = {|Unbound module ([\s\S]+)|}

(* not pluggable yet (unlike `customErrorParsers` below)  *)
let specialParserThatChecksWhetherFileEvenExists filePath errorBody =
  match filePath with
  | "_none_" -> (
    (* TODO: test this *)
    match errorBody with
    | None -> None (* unrecognized? We're mainly trying to catch the case below *)
    | Some err -> (
      match get_match_maybe cannotFindFileRStr err with
      | None -> None (* unrecognized again? We're mainly trying to catch the case below *)
      | Some fileName -> Some (ErrorFile (NoneFile fileName))
    )
  )
  | "command line" -> (
    (* TODO: test this *)
    match errorBody with
    | None -> None (* unrecognized? We're mainly trying to catch the case below *)
    | Some err -> (
      match get_match_maybe unboundModuleRStr err with
      | None -> None (* unrecognized? We're mainly trying to catch the case below *)
      | Some moduleName -> Some (ErrorFile (CommandLine moduleName))
    )
  )
  | _ when String.length filePath > 0 && not (Re_pcre.pmatch ~rex:goodFileNameR (Filename.basename filePath)) ->
    Some (ErrorFile (BadFileName filePath))
  | _ -> None

let parse ~customErrorParsers ~errorBody ~cachedContent ~range =
  try
    (* custom parsers go first *)
    customErrorParsers @ parsers |> Helpers.listFindMap (fun parse ->
      try Some (parse errorBody cachedContent range)
      with _ -> None)
  with Not_found -> Error_CatchAll errorBody
