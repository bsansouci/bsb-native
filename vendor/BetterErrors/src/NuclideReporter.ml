(* open BetterErrorsTypes
open Atom

let diagnosticMessage typee content filePath range originalData =
  let open NuclideDiagnostic.Message in
  (* no project wide error/warning for now? *)
  FileDiagnosticMessage {
    scope = `file;
    providerName = "Merlin";
    typee = typee;
    (* absolute path. Is this right? *)
    filePath = Filename.concat (Sys.getcwd ()) filePath;
    text = Some content;
    html = None;
    range = Some range;
    trace = None;
    originalData;
  }

let toNuclideList errorsAndWarnings =
  List.map2 (fun decryptedContent original ->
    let open NuclideDiagnostic in
    match original with
    | BetterErrorsTypes.Error {filePath; range} ->
      diagnosticMessage
        Error
        decryptedContent
        filePath
        range
        original
    | BetterErrorsTypes.Warning {filePath; range} ->
      diagnosticMessage
        Warning
        decryptedContent
        filePath
        range
        original
  )
  (TerminalReporter.decryptAssumingErrorsAndWarnings errorsAndWarnings)
  errorsAndWarnings

(* FOR JORDAN. This is similar to BetterErrorsTypes.results and the rest *)
type nuclideResult =
  | NoErrorNorWarning of string
  | Unparsable of string
  | ErrorsAndWarnings of BetterErrorsTypes.errorOrWarning NuclideDiagnostic.Message.t list

let convert (content: result): nuclideResult =
  match content with
  | BetterErrorsTypes.NoErrorNorWarning content -> NoErrorNorWarning content
  | BetterErrorsTypes.Unparsable content -> Unparsable content
  | BetterErrorsTypes.ErrorsAndWarnings errorsAndWarnings ->
    ErrorsAndWarnings (toNuclideList errorsAndWarnings) *)
