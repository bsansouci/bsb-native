open BetterErrorsTypes
open Helpers

let report code parsedContent =
  match parsedContent with
  | Warning_CatchAll message -> message
  | Warning_PatternNotExhaustive {unmatched} ->
    "this match doesn't cover all possible values of the variant.\n"
    ^
    (match unmatched with
    | [oneVariant] -> sp "The case `%s` is not matched" oneVariant
    | many -> sp "These cases are not matched:\n%s" (mapcat "\n" (sp "- `%s`") many))
  | Warning_OptionalArgumentNotErased {argumentName} ->
    (sp
      "`%s` is an optional argument at last position; calling the function by omitting %s might be confused with currying.\n"
      argumentName
      argumentName)
      ^ "The rule: an optional argument is erased as soon as the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in."
  |  _ -> "huh"
