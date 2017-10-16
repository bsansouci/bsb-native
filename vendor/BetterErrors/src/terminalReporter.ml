open BetterErrorsTypes
open Helpers

let numberOfDigits n =
  let digits = ref 1 in
  let nn = ref n in
  while !nn / 10 > 0 do
    nn := !nn / 10;
    digits := !digits + 1
  done;
  !digits

let pad ?(ch=' ') content n =
  (String.make (n - (String.length content)) ch) ^ content

(* row and col 0-indexed; endColumn is 1 past the actual end. See
Main.compilerLineColsToRange *)
let _printFile ~highlightColor:color ~highlight:((startRow, startColumn), (endRow, endColumn)) content =
  let sep = " | " in
  let displayedStartRow = max 0 (startRow - 3) in
  (* we display no more than 3 lines after startRow. Some endRow are rly far
  away *)
  let displayedEndRow = min (List.length content - 1) (startRow + 3) in
  let lineNumWidth = numberOfDigits (List.length content) in
  let result = ref "" in
  for i = displayedStartRow to displayedEndRow do
    let currLine = List.nth content i in
      if i >= startRow && i <= endRow then
        if startRow = endRow then
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~color ~first:startColumn ~last:endColumn currLine) ^ "\n"
        else if i = startRow then
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~color ~first:startColumn currLine) ^ "\n"
        else if i = endRow then
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~color ~last:endColumn currLine) ^ "\n"
        else
          result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth)
            ^ sep ^ (highlight ~color currLine) ^ "\n"
      else
        result := !result ^ (pad (string_of_int (i + 1)) lineNumWidth) ^ sep ^ currLine ^ "\n"
  done;
  !result

let printFile ?(isWarning=false) {cachedContent; filePath; range} =
  let ((startRow, startColumn), (endRow, endColumn)) = range in
  let filePathDisplay = if startRow = endRow then
      cyan @@ sp
        "%s:%d %d-%d\n"
        filePath
        (startRow + 1)
        startColumn
        endColumn
    else
      cyan @@ sp
        "%s:%d:%d-%d:%d\n"
        filePath
        (startRow + 1)
        startColumn
        (endRow + 1)
        endColumn
  in filePathDisplay ^ _printFile
    ~highlightColor:(if isWarning then yellowUnderlined else redUnderlined)
    ~highlight:range
    cachedContent

let prettyPrintParsedResult (result: result) =
  match result with
  | Unparsable str ->
    (* output the line without any decoration around. We previously had some
    cute little ascii red x mark to say "we couldn't parse this but there's
    probably an error". But it's very possible that this line's a continuation
    of a previous error, just that we couldn't parse it. So we try to bolt this
    line right after our supposedly parsed and pretty-printed error to make them
    look like one printed error. *)
    (* the effing length we'd go for better errors... someone gimme a cookie *)
    str
  | ErrorFile NonexistentFile -> ""
  | ErrorFile (CommandLine moduleName) ->
    sp "%s: module `%s` not found." (red "Error") moduleName
  | ErrorFile (NoneFile filename) ->
    (* TODO: test case for this. Forgot how to repro it *)
    if Filename.check_suffix filename ".cmo" then
      sp
        "%s: Cannot find file %s. Cmo files are artifacts the compiler looks for when compiling/linking dependent files."
        (red "Error")
        (cyan filename)
    else sp "%s: Cannot find file %s." (red "Error") (cyan filename)
  | ErrorFile (BadFileName filepath) ->
    sp
      "%s\n\n%s 24: \"%s\" isn't a valid file name; OCaml file names are often turned into modules, which need to start with a capitalized letter."
      (cyan filepath)
      (yellow "Warning")
      (Filename.basename filepath)
  | ErrorContent withFileInfo ->
    sp "%s\n%s: %s" (printFile withFileInfo) (red "Error") (ReportError.report withFileInfo.parsedContent)
  | Warning withFileInfo ->
    sp
      "%s\n%s %d: %s"
      (printFile ~isWarning:true withFileInfo)
      (yellow "Warning")
      withFileInfo.parsedContent.code
      (ReportWarning.report withFileInfo.parsedContent.code withFileInfo.parsedContent.warningType)
