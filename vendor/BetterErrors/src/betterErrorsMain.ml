open BetterErrorsTypes
open Helpers
open OcamlRe

(* the compiler output might point to an error that spans across many lines;
however, instead of indicating from (startRow, startColumn) to (endRow,
endColumn), it'll indicate (startRow, startColumn, endColumn) where endColumn
might belong to a different row! We normalize and find the row here *)

(* the compiler line number is 1-indexed, and col number is 0-indexed but the
endColumn for an error goes past the last "expected" endColumn, e.g. if it's
`typ a = string`
instead of saying it's from 0 to 2, it shows as 0 to 3. This is also kinda
expected, since you get easy column count through 3 - 0 *)

(* we'll use 0-indexed. It's a reporter (printer)'s job to normalize to
1-indexed if it desires so *)
let normalizeCompilerLineColsToRange ~fileLines ~lineRaw ~col1Raw ~col2Raw =
  (* accept strings to constraint usage to parse directly from raw data *)
  let line = (int_of_string lineRaw) in
  let fileLength = List.length fileLines in
  let isOCamlBeingBadAndPointingToALineBeyondFileLength = line > fileLength in
  let (col1, col2) = if isOCamlBeingBadAndPointingToALineBeyondFileLength then
    let lastDamnReachableSpotInTheFile =
      String.length @@ List.nth fileLines (fileLength - 1)
    in (lastDamnReachableSpotInTheFile - 1, lastDamnReachableSpotInTheFile)
  else
    match (col1Raw, col2Raw) with
    | (Some a, Some b) -> (int_of_string a, int_of_string b)
    (* some error msgs don't have column numbers; we normal them to 0 here *)
    | _ -> (0, 0)
  in
  let startRow = if isOCamlBeingBadAndPointingToALineBeyondFileLength then
    fileLength - 1
  else
    line - 1
  in
  let currentLine = List.nth fileLines startRow in
  let numberOfCharsBetweenStartAndEndColumn = col2 - col1 in
  let numberOfCharsLeftToCoverOnStartingRow =
    (* +1 bc ocaml looooves to count new line as a char below when the error
    spans multiple lines*)
    (String.length currentLine) - col1 + 1
  in
  if numberOfCharsBetweenStartAndEndColumn <= numberOfCharsLeftToCoverOnStartingRow then
    ((startRow, col1), (startRow, col2))
  else
    let howManyCharsLeftToCoverOnSubsequentLines =
      ref (numberOfCharsBetweenStartAndEndColumn - numberOfCharsLeftToCoverOnStartingRow)
    in
    let suddenlyFunctionalProgrammingOutOfNowhere =
      fileLines
      |> Helpers.listDrop (startRow + 1)
      |> List.map String.length
      |> Helpers.listTakeWhile (fun numberOfCharsOnThisLine ->
        if !howManyCharsLeftToCoverOnSubsequentLines > numberOfCharsOnThisLine then
          (howManyCharsLeftToCoverOnSubsequentLines :=
            !howManyCharsLeftToCoverOnSubsequentLines - numberOfCharsOnThisLine - 1;
          true)
        else false)
    in
    let howManyMoreRowsCoveredSinceStartRow =
      1 + List.length suddenlyFunctionalProgrammingOutOfNowhere
    in
    ((startRow, col1),
    (startRow + howManyMoreRowsCoveredSinceStartRow, !howManyCharsLeftToCoverOnSubsequentLines))

(* has the side-effect of reading the file *)
let extractFromFileMatch fileMatch = Re_pcre.(
  match fileMatch with
  | [Delim _; Group (_, filePath); Group (_, lineNum); col1; col2; Text body] ->
    let cachedContent = Helpers.fileLinesOfExn filePath in
    (* sometimes there's only line, but no characters *)
    let (col1Raw, col2Raw) = match (col1, col2) with
      | (Group (_, c1), Group (_, c2)) ->
        (* bug: https://github.com/mmottl/pcre-ocaml/issues/5 *)
        if String.trim c1 = "" || String.trim c2 = "" then (None, None)
        else (Some c1, Some c2)
      | _ -> (None, None)
    in
    (
      filePath,
      cachedContent,
      (normalizeCompilerLineColsToRange
        ~fileLines:cachedContent
        ~lineRaw:lineNum
        ~col1Raw:col1Raw
        ~col2Raw:col2Raw
      ),
      (* important, otherwise leaves random blank lines that defies some of
      our regex logic, maybe *)
      String.trim body
    )
  | _ -> raise (invalid_arg "Couldn't extract error")
)

(* debug helper *)
let printFullSplitResult = List.iteri (fun i x ->
  print_int i;
  print_endline "";
  Re_pcre.(
    match x with
    | Delim a -> print_endline @@ "Delim " ^ a
    | Group (_, a) -> print_endline @@ "Group " ^ a
    | Text a -> print_endline @@ "Text " ^ a
    | NoGroup -> print_endline @@ "NoGroup"
  )
)

let fileR = Re_pcre.regexp
  ~flags:[Re_pcre.(`MULTILINE)]
  {|^File "([\s\S]+?)", line (\d+)(?:, characters (\d+)-(\d+))?:$|}

let hasErrorOrWarningR = Re_pcre.regexp
  ~flags:[Re_pcre.(`MULTILINE)]
  {|^(Error|Warning \d+): |}

let hasIndentationR = Re_pcre.regexp
  ~flags:[Re_pcre.(`MULTILINE)]
  {|^       +|}

(* TODO: make the below work. the "Here is an example..." is followed by even more lines of hints *)
(* let hasHintRStr = {|^(Hint: Did you mean |Here is an example of a value that is not matched:)|} *)
(* let hasHintRStr = {|^(Here is an example of a value that is not matched:|Hint: Did you mean )|} *)
let hasHintRStr = {|^Hint: Did you mean |}
let hasHintR = Re_pcre.regexp
  ~flags:[Re_pcre.(`MULTILINE)]
  hasHintRStr

(* TODO: check if following tags are used
- Unparsable
 *)
let parse ~customErrorParsers err =
  (* we know whatever err is, it starts with "File: ..." because that's how `parse`
  is used *)
  let err = String.trim err in
  try
    let open Re_pcre in
    match full_split ~rex:fileR err with
    | [Delim _; Group (_, filePath); Group (_, lineNum); col1; col2; Text body] -> (
      (* important, otherwise leaves random blank lines that defies some of
      our regex logic, maybe *)
      let body = String.trim body in
      let errorCapture = get_match_maybe {|^Error: ([\s\S]+)|} body in
      match BetterErrorsParseError.specialParserThatChecksWhetherFileEvenExists filePath errorCapture with
      | Some err -> err
      | None ->
        let cachedContent = Helpers.fileLinesOfExn filePath in
        (* sometimes there's only line, but no characters *)
        let (col1Raw, col2Raw) = match (col1, col2) with
          | (Group (_, c1), Group (_, c2)) ->
            (* bug: https://github.com/mmottl/pcre-ocaml/issues/5 *)
            if String.trim c1 = "" || String.trim c2 = "" then raise (Invalid_argument "HUHUHUH")
            else (Some c1, Some c2)
          | _ -> (None, None)
        in
        let range = normalizeCompilerLineColsToRange
          ~fileLines:cachedContent
          ~lineRaw:lineNum
          ~col1Raw:col1Raw
          ~col2Raw:col2Raw
        in
        let warningCapture =
          match execMaybe {|^Warning (\d+): ([\s\S]+)|} body with
          | None -> (None, None)
          | Some capture -> (getSubstringMaybe capture 1, getSubstringMaybe capture 2)
        in (
          match (errorCapture, warningCapture) with
          | (Some errorBody, (None, None)) ->
            ErrorContent {
              filePath;
              cachedContent;
              range;
              parsedContent = BetterErrorsParseError.parse
                ~customErrorParsers
                ~errorBody
                ~cachedContent
                ~range;
            }
          | (None, (Some code, Some warningBody)) ->
            Warning {
              filePath;
              cachedContent;
              range;
              parsedContent = {
                code = int_of_string code;
                warningType = ParseWarning.parse code warningBody cachedContent range;
              };
            }
          | _ -> raise (Invalid_argument err)
        ) (* not an error, not a warning. False alarm? *)
      )
      | _ -> Unparsable err
  with _ -> Unparsable err

(* let parse ~customErrorParsers err =
  try
    parse ~customErrorParsers err
    |> TerminalReporter.prettyPrintParsedResult
  with _ ->
    (* final fallback, just print *)
    Printf.sprintf "Something went wrong during error parsing.\n%s" err *)

let line_stream_of_channel channel =
  Stream.from
    (fun _ -> try Some (input_line channel) with | End_of_file -> None)

(* entry point, for convenience purposes for now. Theoretically the parser and
the reporters are decoupled *)
let parseFromStdin ~customErrorParsers =
  let errBuffer = ref "" in
  try
    (line_stream_of_channel stdin) |> Stream.iter (fun line ->
      match (
        errBuffer.contents,
        Re_pcre.pmatch ~rex:fileR line,
        Re_pcre.pmatch ~rex:hasErrorOrWarningR line,
        Re_pcre.pmatch ~rex:hasIndentationR line
      ) with
      | ("", false, false, false) ->
        (* no error, just stream on the line *)
        print_endline line
      | ("", true, _, _) | ("", _, true, _) | ("", _, _, true) -> (
        (* the beginning of a new error! *)
        errBuffer := line ^ "\n";
        (* don't parse it yet. Maybe the error's continuing on the next line *)
      )
      | (_, true, _, _) -> (
        (* we have a file match, AND the current errBuffer isn't empty? We'll
        just assume here that this is also the beginning of a new error, unless
        a single error might span many (non-indented, god forbid) fileNames.
        Print out the current (previous) error and keep accumulating *)
        parse ~customErrorParsers errBuffer.contents
        |> TerminalReporter.prettyPrintParsedResult
        |> print_endline;
        errBuffer := line ^ "\n"
      )
      | (_, _, _, true) | (_, _, true, _)->
        (* buffer not empty, and we're seeing an error/indentation line. This is
        the continuation of a currently streaming error/warning *)
        errBuffer := errBuffer.contents ^ line ^ "\n";
      | (_, false, false, false) -> (
        (* woah this case was previously forgotten but caught by the compiler.
        Man I don't ever wanna write an if-else anymore *)

        (* buffer not empty, and no indentation and not an error/file line? This
        means the previous error might have ended. We say "might" because some
        errors provide non-indented messages... here's one such case *)
        if Re_pcre.pmatch ~rex:hasHintR line then (
          errBuffer := errBuffer.contents ^ line ^ "\n";
          parse ~customErrorParsers errBuffer.contents
          |> TerminalReporter.prettyPrintParsedResult
          |> print_endline;
          errBuffer := ""
        ) else (
          parse ~customErrorParsers errBuffer.contents
          |> TerminalReporter.prettyPrintParsedResult
          |> print_endline;
          errBuffer := line ^ "\n"
        )
      )
    );
    (* might have accumulated a few more lines *)
    if (String.trim errBuffer.contents) <> "" then (
      parse ~customErrorParsers errBuffer.contents
      |> TerminalReporter.prettyPrintParsedResult
      |> print_endline;
    );
    close_in stdin
  with | e -> (close_in stdin; raise e)
