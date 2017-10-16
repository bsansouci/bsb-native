module Range = struct
  (* (startRow, startColumn), (endRow, endColumn) *)
  type t = (int * int) * (int * int)
  let emptyRange = ((0, 0), (0, 0))
end

module NuclideDiagnostic = struct
  type filePath = string

  type diagnosticType =
    | Error
    | Warning

  module Trace = struct
    type t = {
      typee: [ `trace]; (* Used to differentiate different records with the same shape *)
      text: string option; (* The error message *)
      html: string option; (* The error message in HTML *)
      filePath: filePath; (* Full path to the file in which the error happened *)
      range: Range.t option; (* Range where the error happened (row and column) *)
    }
  end

  module Message = struct
    type 'a fileDiagnosticMessage = {
      scope: [ `file]; (* See Trace *)
      providerName: string; (* See Trace *)
      typee: diagnosticType; (* Whether it's a warning or an error *)
      filePath: filePath; (* See Trace *)
      text: string option; (* See Trace *)
      html: string option; (* See Trace *)
      range: Range.t option; (* See Trace *)
      trace: Trace.t array option; (* Maybe one file only ever has one FileDiagnosticMessage and many traces *)
      originalData: 'a;
    }
    type 'a projectDiagnosticMessage = {
      scope: [ `project]; (* See above *)
      providerName: string; (* See above *)
      typee: diagnosticType; (* See above *)
      text: string option; (* See above *)
      html: string option; (* See above *)
      range: Range.t option; (* See above *)
      trace: Trace.t array option; (* See above *)
      originalData: 'a;
    }
    type 'a t =
      | FileDiagnosticMessage of 'a fileDiagnosticMessage
      | ProjectDiagnosticMessage of 'a projectDiagnosticMessage
  end
end
