(* records that are only used by their variant tag of similar name below. We
need inline record type declarations... *)
type mismatchTypeArguments = {
  typeConstructor: string;
  expectedCount: int;
  actualCount: int;
}
type unboundValue = {
  unboundValue: string;
  suggestions: string list option;
}
type signatureMismatch = {constructor: string; expectedCount: int; observedCount: int}
type signatureItemMissing = {constructor: string; expectedCount: int; observedCount: int}

type unboundModule = {
  unboundModule: string;
  suggestion: string option;
}

type unboundConstructor = {constructor: string; expectedCount: int; observedCount: int}

type unboundTypeConstructor = {
  namespacedConstructor: string;
  suggestion: string option;
}
type appliedTooMany = {
  functionType: string;
  expectedArgCount: int;
}

type recordFieldNotInExpression = {constructor: string; expectedCount: int; observedCount: int}
type recordFieldError = {constructor: string; expectedCount: int; observedCount: int}
type inconsistentAssumptions = {constructor: string; expectedCount: int; observedCount: int}
type catchAll = {
  warningCode: int;
  message: string;
}
type unusedVariable = {constructor: string; expectedCount: int; observedCount: int}

type fieldNotBelong = {
  actual: string;
  expected: string;
}
type incompatibleType = {
  actual: string;
  expected: string;
  differingPortion: string * string;
  actualEquivalentType: string option;
  expectedEquivalentType: string option;
  extra: string option;
}
type notAFunction = {
  actual: string;
}
type syntaxError = {
  offendingString: string;
  hint: string option;
}
type illegalCharacter = {
  character: string;
}
type patternNotExhaustive = {
  unmatched: string list;
}
type unparsableButWithFileInfo = {
  error: string;
}
type unboundRecordField = {
  recordField: string;
  suggestion: string option;
}
type optionalArgumentNotErased = {
  argumentName: string;
}

(* -------------------------- *)

type warningType =
  | Warning_UnusedVariable of unusedVariable
  | Warning_PatternNotExhaustive of patternNotExhaustive
  | Warning_PatternUnused of unusedVariable
  | Warning_OptionalArgumentNotErased of optionalArgumentNotErased
  | Warning_CatchAll of string

type error =
  | Type_MismatchTypeArguments of mismatchTypeArguments
  | Type_UnboundValue of unboundValue
  | Type_SignatureMismatch of signatureMismatch
  | Type_SignatureItemMissing of signatureItemMissing
  | Type_UnboundModule of unboundModule
  | Type_UnboundRecordField of unboundRecordField
  | Type_UnboundConstructor of unboundConstructor
  | Type_UnboundTypeConstructor of unboundTypeConstructor
  | Type_AppliedTooMany of appliedTooMany
  | Type_RecordFieldNotInExpression of recordFieldNotInExpression
  | Type_RecordFieldError of recordFieldError
  (* might be the same thing as above? jordan wrote "record expression" instead
  of "pattern" *)
  | Type_RecordFieldNotBelong of recordFieldError
  | Type_FieldNotBelong of fieldNotBelong
  | Type_IncompatibleType of incompatibleType
  | Type_NotAFunction of notAFunction
  | File_SyntaxError of syntaxError
  | Build_InconsistentAssumptions of inconsistentAssumptions
  | File_IllegalCharacter of illegalCharacter
  | Error_CatchAll of string

type fileError =
  | NoneFile of string
  | NonexistentFile
  | CommandLine of string
  | BadFileName of string

type warning = {
  code: int;
  warningType: warningType;
}
type 'a withFileInfo = {
  filePath: string;
  cachedContent: string list;
  range: Atom.Range.t;
  parsedContent: 'a;
}
type result =
  | Unparsable of string
  | ErrorFile of fileError
  | ErrorContent of error withFileInfo
  | Warning of warning withFileInfo
