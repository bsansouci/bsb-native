module Atom
= struct
#1 "Atom.ml"
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

end
module BetterErrorsTypes
= struct
#1 "betterErrorsTypes.ml"
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

end
module Re_fmt
= struct
#1 "re_fmt.ml"
(** Very small tooling for format printers. *)

include Format

(* Only in the stdlib since 4.02, so we copy. *)
let rec list pp ppf = function
  | [] -> ()
  | [v] -> pp ppf v
  | v :: vs ->
    pp ppf v;
    pp_print_space ppf ();
    list pp ppf vs

let str = pp_print_string
let sexp fmt s pp x = fprintf fmt "@[<3>(%s@ %a)@]" s pp x
let pair pp1 pp2 fmt (v1,v2) =
  pp1 fmt v1; pp_print_space fmt () ; pp2 fmt v2
let triple pp1 pp2 pp3 fmt (v1, v2, v3) =
  pp1 fmt v1; pp_print_space fmt () ;
  pp2 fmt v2; pp_print_space fmt () ;
  pp3 fmt v3
let int = pp_print_int
let optint fmt = function
  | None -> ()
  | Some i -> fprintf fmt "@ %d" i

end
module Re_cset : sig 
#1 "re_cset.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(* Character sets, represented as sorted list of intervals *)

type c = int
type t

val iter : t -> f:(c -> c -> unit) -> unit

val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val offset : int -> t -> t

val empty : t
val single : c -> t
val seq : c -> c -> t
val add : c -> t -> t

val mem : c -> t -> bool

type hash
val hash : t -> hash

val pp : Format.formatter -> t -> unit

val one_char : t -> c option

val fold_right : t -> init:'acc -> f:(c * c -> 'acc ->  'acc) -> 'acc

val hash_rec : t -> int

module CSetMap : Map.S with type key = int * t

val cany : t

val csingle : char -> t

val is_empty : t -> bool

val prepend : t -> 'a list -> (t * 'a list) list -> (t * 'a list) list

val pick : t -> c

end = struct
#1 "re_cset.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

type c = int
type t = (c * c) list

let rec union l l' =
  match l, l' with
    _, [] -> l
  | [], _ -> l'
  | (c1, c2)::r, (c1', c2')::r' ->
    if c2 + 1 < c1' then
      (c1, c2)::union r l'
    else if c2' + 1 < c1 then
      (c1', c2')::union l r'
    else if c2 < c2' then
      union r ((min c1 c1', c2')::r')
    else
      union ((min c1 c1', c2)::r) r'

let rec inter l l' =
  match l, l' with
    _, [] -> []
  | [], _ -> []
  | (c1, c2)::r, (c1', c2')::r' ->
    if c2 < c1' then
      inter r l'
    else if c2' < c1 then
      inter l r'
    else if c2 < c2' then
      (max c1 c1', c2)::inter r l'
    else
      (max c1 c1', c2')::inter l r'

let rec diff l l' =
  match l, l' with
    _, [] -> l
  | [], _ -> []
  | (c1, c2)::r, (c1', c2')::r' ->
    if c2 < c1' then
      (c1, c2)::diff r l'
    else if c2' < c1 then
      diff l r'
    else
      let r'' = if c2' < c2 then (c2' + 1, c2) :: r else r in
      if c1 < c1' then
        (c1, c1' - 1)::diff r'' r'
      else
        diff r'' r'

let single c = [c, c]

let add c l = union (single c) l

let seq c c' = if c <= c' then [c, c'] else [c', c]

let rec offset o l =
  match l with
    []            -> []
  | (c1, c2) :: r -> (c1 + o, c2 + o) :: offset o r

let empty = []

let rec mem (c : int) s =
  match s with
    []              -> false
  | (c1, c2) :: rem -> if c <= c2 then c >= c1 else mem c rem

(****)

type hash = int

let rec hash_rec = function
  | []        -> 0
  | (i, j)::r -> i + 13 * j + 257 * hash_rec r
let hash l = (hash_rec l) land 0x3FFFFFFF

(****)

let print_one ch (c1, c2) =
  if c1 = c2 then
    Format.fprintf ch "%d" c1
  else
    Format.fprintf ch "%d-%d" c1 c2

let pp = Re_fmt.list print_one

let rec iter t ~f =
  match t with
  | [] -> ()
  | (x, y)::xs ->
    f x y;
    iter xs  ~f

let one_char = function
  | [i, j] when i = j -> Some i
  | _ -> None


module CSetMap = Map.Make (struct
    type t = int * (int * int) list
    let compare (i, u) (j, v) =
      let c = compare i j in
      if c <> 0
      then c
      else compare u v
  end)

let fold_right t ~init ~f = List.fold_right f t init

let csingle c = single (Char.code c)

let cany = [0, 255]

let is_empty = function
  | [] -> true
  | _ -> false

let rec prepend s x l =
  match s, l with
  | [], _ -> l
  | _r, [] -> []
  | (_c, c') :: r, ([d, _d'], _x') :: _r' when c' < d -> prepend r x l
  | (c, c') :: r, ([d, d'], x') :: r' ->
    if c <= d then begin
      if c' < d'
      then ([d, c'], x @ x') :: prepend r x (([c' + 1, d'], x') :: r')
      else ([d, d'], x @ x') :: prepend s x r'
    end else begin
      if c > d'
      then ([d, d'], x') :: prepend s x r'
      else ([d, c - 1], x') :: prepend s x (([c, d'], x') :: r')
    end
  | _ -> assert false

let pick = function
  | [] -> invalid_arg "Re_cset.pick"
  | (x, _)::_ -> x

end
module Re_automata : sig 
#1 "re_automata.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(* Regular expressions *)

type category = int
type mark = int

type sem = [ `Longest | `Shortest | `First ]
type rep_kind = [ `Greedy | `Non_greedy ]

val pp_sem : Format.formatter -> sem -> unit
val pp_rep_kind : Format.formatter -> rep_kind -> unit

module Pmark : sig
  type t = private int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val gen : unit -> t
  val pp : Format.formatter -> t -> unit
end

type expr
val is_eps : expr -> bool
val pp : Format.formatter -> expr -> unit

type ids
val create_ids : unit -> ids

val cst : ids -> Re_cset.t -> expr
val empty : ids -> expr
val alt : ids -> expr list -> expr
val seq : ids -> sem -> expr -> expr -> expr
val eps : ids -> expr
val rep : ids -> rep_kind -> sem -> expr -> expr
val mark : ids -> mark -> expr
val pmark : ids -> Pmark.t -> expr
val erase : ids -> mark -> mark -> expr
val before : ids -> category -> expr
val after : ids -> category -> expr

val rename : ids -> expr -> expr

(****)

module PmarkSet : Set.S with type elt = Pmark.t

(* States of the automata *)

type idx = int
module Marks : sig
  type t =
    { marks: (mark * idx) list
    ; pmarks: PmarkSet.t }
end

module E : sig
  type t
  val pp : Format.formatter -> t -> unit
end

type hash
type mark_infos = int array
type status = Failed | Match of mark_infos * PmarkSet.t | Running

module State : sig
  type t =
    { idx: idx
    ; category: category
    ; desc: E.t list
    ; mutable status: status option
    ; hash: hash }
  val dummy : t
  val create : category -> expr -> t
  module Table : Hashtbl.S with type key = t
end

(****)

(* Computation of the states following a given state *)

type working_area
val create_working_area : unit -> working_area
val index_count : working_area -> int

val delta : working_area -> category -> Re_cset.c -> State.t -> State.t
val deriv :
  working_area -> Re_cset.t -> (category * Re_cset.t) list -> State.t ->
  (Re_cset.t * State.t) list

(****)

val status : State.t -> status

end = struct
#1 "re_automata.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

module Cset = Re_cset

type sem = [ `Longest | `Shortest | `First ]

type rep_kind = [ `Greedy | `Non_greedy ]

type category = int
type mark = int
type idx = int

module Pmark : sig
  type t = private int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val gen : unit -> t
  val pp : Format.formatter -> t -> unit
end
= struct
  type t = int
  let equal (x : int) (y : int) = x = y
  let compare (x : int) (y : int) = compare x y
  let r = ref 0
  let gen () = incr r ; !r

  let pp = Format.pp_print_int
end

type expr = { id : int; def : def }

and def =
    Cst of Cset.t
  | Alt of expr list
  | Seq of sem * expr * expr
  | Eps
  | Rep of rep_kind * sem * expr
  | Mark of int
  | Erase of int * int
  | Before of category
  | After of category
  | Pmark of Pmark.t

module PmarkSet = Set.Make(Pmark)

let hash_combine h accu = accu * 65599 + h

module Marks = struct
  type t =
    { marks : (int * int) list
    ; pmarks : PmarkSet.t }

  let empty = { marks = [] ; pmarks = PmarkSet.empty }

  let rec merge_marks_offset old = function
    | [] ->
      old
    | (i, v) :: rem ->
      let nw' = merge_marks_offset (List.remove_assq i old) rem in
      if v = -2 then
        nw'
      else
        (i, v) :: nw'

  let merge old nw =
    { marks = merge_marks_offset old.marks nw.marks
    ; pmarks = PmarkSet.union old.pmarks nw.pmarks }

  let rec hash_marks_offset l accu =
    match l with
      []          -> accu
    | (a, i) :: r -> hash_marks_offset r (hash_combine a (hash_combine i accu))

  let hash m accu =
    hash_marks_offset m.marks (hash_combine (Hashtbl.hash m.pmarks) accu)

  let rec marks_set_idx idx = function
    | (a, -1) :: rem ->
      (a, idx) :: marks_set_idx idx rem
    | marks ->
      marks

  let marks_set_idx marks idx =
    { marks with marks = marks_set_idx idx marks.marks }

  let pp_marks ch t =
    match t.marks with
    | [] ->
      ()
    | (a, i) :: r ->
      Format.fprintf ch "%d-%d" a i;
      List.iter (fun (a, i) -> Format.fprintf ch " %d-%d" a i) r
end

(****)

let pp_sem ch k =
  Format.pp_print_string ch
    (match k with
       `Shortest -> "short"
     | `Longest  -> "long"
     | `First    -> "first")


let pp_rep_kind fmt = function
  | `Greedy -> Format.pp_print_string fmt "Greedy"
  | `Non_greedy -> Format.pp_print_string fmt "Non_greedy"

let rec pp ch e =
  let open Re_fmt in
  match e.def with
    Cst l ->
    sexp ch "cst" Cset.pp l;
  | Alt l ->
    sexp ch "alt" (list pp) l
  | Seq (k, e, e') ->
    sexp ch "seq" (triple pp_sem pp pp) (k, e, e')
  | Eps ->
    str ch "eps"
  | Rep (_rk, k, e) ->
    sexp ch "rep" (pair pp_sem pp) (k, e)
  | Mark i ->
    sexp ch "mark" int i
  | Pmark i ->
    sexp ch "pmark" int (i :> int)
  | Erase (b, e) ->
    sexp ch "erase" (pair int int) (b, e)
  | Before c ->
    sexp ch "before" int c
  | After c ->
    sexp ch "after" int c


(****)

let rec first f = function
  | [] ->
    None
  | x :: r ->
    match f x with
      None          -> first f r
    | Some _ as res -> res

(****)

type ids = int ref
let create_ids () = ref 0

let eps_expr = { id = 0; def = Eps }

let mk_expr ids def =
  incr ids;
  { id = !ids; def = def }

let empty ids = mk_expr ids (Alt [])

let cst ids s =
  if Re_cset.is_empty s
  then empty ids
  else mk_expr ids (Cst s)

let alt ids = function
  | []  -> empty ids
  | [c] -> c
  | l   -> mk_expr ids (Alt l)

let seq ids kind x y =
  match x.def, y.def with
    Alt [], _                 -> x
  | _, Alt []                 -> y
  | Eps, _                    -> y
  | _, Eps when kind = `First -> x
  | _                         -> mk_expr ids (Seq (kind, x, y))

let is_eps expr =
  match expr.def with
  | Eps -> true
  | _ -> false

let eps ids = mk_expr ids Eps

let rep ids kind sem x = mk_expr ids (Rep (kind, sem, x))

let mark ids m = mk_expr ids (Mark m)

let pmark ids i = mk_expr ids (Pmark i)

let erase ids m m' = mk_expr ids (Erase (m, m'))

let before ids c = mk_expr ids (Before c)

let after ids c = mk_expr ids (After c)

(****)

let rec rename ids x =
  match x.def with
    Cst _ | Eps | Mark _ | Pmark _ | Erase _ | Before _ | After _ ->
    mk_expr ids x.def
  | Alt l ->
    mk_expr ids (Alt (List.map (rename ids) l))
  | Seq (k, y, z) ->
    mk_expr ids (Seq (k, rename ids y, rename ids z))
  | Rep (g, k, y) ->
    mk_expr ids (Rep (g, k, rename ids y))

(****)

type hash = int
type mark_infos = int array
type status = Failed | Match of mark_infos * PmarkSet.t | Running

module E = struct
  type t =
    | TSeq of t list * expr * sem
    | TExp of Marks.t * expr
    | TMatch of Marks.t

  let rec equal l1 l2 =
    match l1, l2 with
    | [], [] ->
      true
    | TSeq (l1', e1, _) :: r1, TSeq (l2', e2, _) :: r2 ->
      e1.id = e2.id && equal l1' l2' && equal r1 r2
    | TExp (marks1, e1) :: r1, TExp (marks2, e2) :: r2 ->
      e1.id = e2.id && marks1 = marks2 && equal r1 r2
    | TMatch marks1 :: r1, TMatch marks2 :: r2 ->
      marks1 = marks2 && equal r1 r2
    | _ ->
      false

  let rec hash l accu =
    match l with
    | [] ->
      accu
    | TSeq (l', e, _) :: r ->
      hash r (hash_combine 0x172a1bce (hash_combine e.id (hash l' accu)))
    | TExp (marks, e) :: r ->
      hash r
        (hash_combine 0x2b4c0d77 (hash_combine e.id (Marks.hash marks accu)))
    | TMatch marks :: r ->
      hash r (hash_combine 0x1c205ad5 (Marks.hash marks accu))

  let texp marks x = TExp (marks, x)

  let tseq kind x y rem =
    match x with
      []                              -> rem
    | [TExp (marks, {def = Eps ; _})] -> TExp (marks, y) :: rem
    | _                               -> TSeq (x, y, kind) :: rem

  let rec print_state_rec ch e y =
    match e with
    | TMatch marks ->
      Format.fprintf ch "@[<2>(Match@ %a)@]" Marks.pp_marks marks
    | TSeq (l', x, _kind) ->
      Format.fprintf ch "@[<2>(Seq@ ";
      print_state_lst ch l' x;
      Format.fprintf ch " %a)@]" pp x
    | TExp (marks, {def = Eps; _}) ->
      Format.fprintf ch "(Exp %d (%a) (eps))" y.id Marks.pp_marks marks
    | TExp (marks, x) ->
      Format.fprintf ch "(Exp %d (%a) %a)" x.id Marks.pp_marks marks pp x

  and print_state_lst ch l y =
    match l with
      [] ->
      Format.fprintf ch "()"
    | e :: rem ->
      print_state_rec ch e y;
      List.iter
        (fun e ->
           Format.fprintf ch " | ";
           print_state_rec ch e y)
        rem

  let pp ch t = print_state_lst ch [t] { id = 0; def = Eps }
end

module State = struct
  type t =
    { idx: idx
    ; category: category
    ; desc: E.t list
    ; mutable status: status option
    ; hash: hash }

  let dummy =
    { idx = -1
    ; category = -1
    ; desc = []
    ; status = None
    ; hash = -1 }

  let hash idx cat desc =
    E.hash desc (hash_combine idx (hash_combine cat 0)) land 0x3FFFFFFF

  let mk idx cat desc = 
    { idx
    ; category = cat
    ; desc
    ; status = None
    ; hash = hash idx cat desc}

  let create cat e = mk 0 cat [E.TExp (Marks.empty, e)]

  let equal x y =
    (x.hash : int) = y.hash && (x.idx : int) = y.idx &&
    (x.category : int) = y.category && E.equal x.desc y.desc

  let compare x y =
    let c = compare (x.hash : int) y.hash in
    if c <> 0 then c else
      let c = compare (x.category : int) y.category in
      if c <> 0 then c else
        compare x.desc y.desc

  type t' = t
  module Table = Hashtbl.Make(
    struct
      type t = t'
      let equal = equal
      let hash t = t.hash
    end)
end

(**** Find a free index ****)

type working_area = bool array ref

let create_working_area () = ref [| false |]

let index_count w = Array.length !w

let reset_table a = Array.fill a 0 (Array.length a) false

let rec mark_used_indices tbl =
  List.iter (function
      | E.TSeq (l, _, _) -> mark_used_indices tbl l
      | E.TExp (marks, _)
      | E.TMatch marks ->
        List.iter (fun (_, i) -> if i >= 0 then tbl.(i) <- true)
          marks.Marks.marks)

let rec find_free tbl idx len =
  if idx = len || not tbl.(idx) then idx else find_free tbl (idx + 1) len

let free_index tbl_ref l =
  let tbl = !tbl_ref in
  reset_table tbl;
  mark_used_indices tbl l;
  let len = Array.length tbl in
  let idx = find_free tbl 0 len in
  if idx = len then tbl_ref := Array.make (2 * len) false;
  idx

(**** Computation of the next state ****)

let remove_matches = List.filter (function E.TMatch _ -> false | _ -> true)

let rec split_at_match_rec l' = function
  | []            -> assert false
  | E.TMatch _ :: r -> (List.rev l', remove_matches r)
  | x :: r        -> split_at_match_rec (x :: l') r

let split_at_match l = split_at_match_rec [] l

let rec remove_duplicates prev l y =
  match l with
    [] ->
    ([], prev)
  | E.TMatch _ as x :: _ -> (* Truncate after first match *)
    ([x], prev)
  | E.TSeq (l', x, kind) :: r ->
    let (l'', prev') = remove_duplicates prev l' x in
    let (r', prev'') = remove_duplicates prev' r y in
    (E.tseq kind l'' x r', prev'')
  | E.TExp (_marks, {def = Eps; _}) as e :: r ->
    if List.memq y.id prev then
      remove_duplicates prev r y
    else
      let (r', prev') = remove_duplicates (y.id :: prev) r y in
      (e :: r', prev')
  | E.TExp (_marks, x) as e :: r ->
    if List.memq x.id prev then
      remove_duplicates prev r y
    else
      let (r', prev') = remove_duplicates (x.id :: prev) r y in
      (e :: r', prev')

let rec set_idx idx = function
  | [] ->
    []
  | E.TMatch marks :: r ->
    E.TMatch (Marks.marks_set_idx marks idx) :: set_idx idx r
  | E.TSeq (l', x, kind) :: r ->
    E.TSeq (set_idx idx l', x, kind) :: set_idx idx r
  | E.TExp (marks, x) :: r ->
    E.TExp ((Marks.marks_set_idx marks idx), x) :: set_idx idx r

let filter_marks b e marks =
  {marks with Marks.marks = List.filter (fun (i, _) -> i < b || i > e) marks.Marks.marks }

let rec delta_1 marks c cat' cat x rem =
  (*Format.eprintf "%d@." x.id;*)
  match x.def with
    Cst s ->
    if Cset.mem c s then E.texp marks eps_expr :: rem else rem
  | Alt l ->
    delta_2 marks c cat' cat l rem
  | Seq (kind, y, z) ->
    let y' = delta_1 marks c cat' cat y [] in
    delta_seq c cat' cat kind y' z rem
  | Rep (rep_kind, kind, y) ->
    let y' = delta_1 marks c cat' cat y [] in
    let (y'', marks') =
      match
        first
          (function E.TMatch marks -> Some marks | _ -> None) y'
      with
        None        -> (y', marks)
      | Some marks' -> (remove_matches y', marks')
    in
    begin match rep_kind with
        `Greedy     -> E.tseq kind y'' x (E.TMatch marks' :: rem)
      | `Non_greedy -> E.TMatch marks :: E.tseq kind y'' x rem
    end
  | Eps ->
    E.TMatch marks :: rem
  | Mark i ->
    let marks = { marks with Marks.marks = (i, -1) :: List.remove_assq i marks.Marks.marks } in
    E.TMatch marks :: rem
  | Pmark i ->
    let marks = { marks with Marks.pmarks = PmarkSet.add i marks.Marks.pmarks } in
    E.TMatch marks :: rem
  | Erase (b, e) ->
    E.TMatch (filter_marks b e marks) :: rem
  | Before cat'' ->
    if cat land cat'' <> 0 then E.TMatch marks :: rem else rem
  | After cat'' ->
    if cat' land cat'' <> 0 then E.TMatch marks :: rem else rem

and delta_2 marks c cat' cat l rem =
  match l with
    []     -> rem
  | y :: r -> delta_1 marks c cat' cat y (delta_2 marks c cat' cat r rem)

and delta_seq c cat' cat kind y z rem =
  match
    first (function E.TMatch marks -> Some marks | _ -> None) y
  with
    None ->
    E.tseq kind y z rem
  | Some marks ->
    match kind with
      `Longest ->
      E.tseq kind (remove_matches y) z (delta_1 marks c cat' cat z rem)
    | `Shortest ->
      delta_1 marks c cat' cat z (E.tseq kind (remove_matches y) z rem)
    | `First ->
      let (y', y'') = split_at_match y in
      E.tseq kind y' z (delta_1 marks c cat' cat z (E.tseq kind y'' z rem))

let rec delta_3 c cat' cat x rem =
  match x with
    E.TSeq (y, z, kind) ->
    let y' = delta_4 c cat' cat y [] in
    delta_seq c cat' cat kind y' z rem
  | E.TExp (marks, e) ->
    delta_1 marks c cat' cat e rem
  | E.TMatch _ ->
    x :: rem

and delta_4 c cat' cat l rem =
  match l with
    []     -> rem
  | y :: r -> delta_3 c cat' cat y (delta_4 c cat' cat r rem)

let delta tbl_ref cat' char st =
  let (expr', _) =
    remove_duplicates [] (delta_4 char st.State.category cat' st.State.desc [])
      eps_expr in
  let idx = free_index tbl_ref expr' in
  let expr'' = set_idx idx expr' in
  State.mk idx cat' expr''

(****)

let rec red_tr = function
  | [] | [_] as l ->
    l
  | ((s1, st1) as tr1) :: ((s2, st2) as tr2) :: rem ->
    if State.equal st1 st2 then
      red_tr ((Cset.union s1 s2, st1) :: rem)
    else
      tr1 :: red_tr (tr2 :: rem)

let simpl_tr l =
  List.sort
    (fun (s1, _) (s2, _) -> compare s1 s2)
    (red_tr (List.sort (fun (_, st1) (_, st2) -> State.compare st1 st2) l))

(****)

let prepend_deriv = List.fold_right (fun (s, x) l -> Cset.prepend s x l)

let rec restrict s = function
  | [] -> []
  | (s', x') :: rem ->
    let s'' = Cset.inter s s' in
    if Cset.is_empty s''
    then restrict s rem
    else (s'', x') :: restrict s rem

let rec remove_marks b e rem =
  if b > e then rem else remove_marks b (e - 1) ((e, -2) :: rem)

let rec prepend_marks_expr m = function
  | E.TSeq (l, e', s) -> E.TSeq (prepend_marks_expr_lst m l, e', s)
  | E.TExp (m', e')   -> E.TExp (Marks.merge m m', e')
  | E.TMatch m'       -> E.TMatch (Marks.merge m m')

and prepend_marks_expr_lst m l =
  List.map (prepend_marks_expr m) l

let prepend_marks m =
  List.map (fun (s, x) -> (s, prepend_marks_expr_lst m x))

let rec deriv_1 all_chars categories marks cat x rem =
  match x.def with
  | Cst s ->
    Cset.prepend s [E.texp marks eps_expr] rem
  | Alt l ->
    deriv_2 all_chars categories marks cat l rem
  | Seq (kind, y, z) ->
    let y' = deriv_1 all_chars categories marks cat y [(all_chars, [])] in
    deriv_seq all_chars categories cat kind y' z rem
  | Rep (rep_kind, kind, y) ->
    let y' = deriv_1 all_chars categories marks cat y [(all_chars, [])] in
    List.fold_right
      (fun (s, z) rem ->
         let (z', marks') =
           match
             first
               (function E.TMatch marks -> Some marks | _ -> None)
               z
           with
             None        -> (z, marks)
           | Some marks' -> (remove_matches z, marks')
         in
         Cset.prepend s
           (match rep_kind with
              `Greedy     -> E.tseq kind z' x [E.TMatch marks']
            | `Non_greedy -> E.TMatch marks :: E.tseq kind z' x [])
           rem)
      y' rem
  | Eps ->
    Cset.prepend all_chars [E.TMatch marks] rem
  | Mark i ->
    Cset.prepend all_chars [E.TMatch {marks with Marks.marks = ((i, -1) :: List.remove_assq i marks.Marks.marks)}] rem
  | Pmark _ ->
    Cset.prepend all_chars [E.TMatch marks] rem
  | Erase (b, e) ->
    Cset.prepend all_chars
      [E.TMatch {marks with Marks.marks = (remove_marks b e (filter_marks b e marks).Marks.marks)}] rem
  | Before cat' ->
    Cset.prepend (List.assq cat' categories) [E.TMatch marks] rem
  | After cat' ->
    if cat land cat' <> 0 then Cset.prepend all_chars [E.TMatch marks] rem else rem

and deriv_2 all_chars categories marks cat l rem =
  match l with
    []     -> rem
  | y :: r -> deriv_1 all_chars categories marks cat y
                (deriv_2 all_chars categories marks cat r rem)

and deriv_seq all_chars categories cat kind y z rem =
  if
    List.exists
      (fun (_s, xl) ->
         List.exists (function E.TMatch _ -> true | _ -> false) xl)
      y
  then
    let z' = deriv_1 all_chars categories Marks.empty cat z [(all_chars, [])] in
    List.fold_right
      (fun (s, y) rem ->
         match
           first (function E.TMatch marks -> Some marks | _ -> None)
             y
         with
           None ->
           Cset.prepend s (E.tseq kind y z []) rem
         | Some marks ->
           let z'' = prepend_marks marks z' in
           match kind with
             `Longest ->
             Cset.prepend s (E.tseq kind (remove_matches y) z []) (
               prepend_deriv (restrict s z'') rem)
           | `Shortest ->
             prepend_deriv (restrict s z'') (
               Cset.prepend s (E.tseq kind (remove_matches y) z []) rem)
           | `First ->
             let (y', y'') = split_at_match y in
             Cset.prepend s (E.tseq kind y' z []) (
               prepend_deriv (restrict s z'') (
                 Cset.prepend s (E.tseq kind y'' z []) rem)))
      y rem
  else
    List.fold_right
      (fun (s, xl) rem -> Cset.prepend s (E.tseq kind xl z []) rem) y rem

let rec deriv_3 all_chars categories cat x rem =
  match x with
    E.TSeq (y, z, kind) ->
    let y' = deriv_4 all_chars categories cat y [(all_chars, [])] in
    deriv_seq all_chars categories cat kind y' z rem
  | E.TExp (marks, e) ->
    deriv_1 all_chars categories marks cat e rem
  | E.TMatch _ ->
    Cset.prepend all_chars [x] rem

and deriv_4 all_chars categories cat l rem =
  match l with
    []     -> rem
  | y :: r -> deriv_3 all_chars categories cat y
                (deriv_4 all_chars categories cat r rem)

let deriv tbl_ref all_chars categories st =
  let der = deriv_4 all_chars categories st.State.category st.State.desc
      [(all_chars, [])] in
  simpl_tr (
    List.fold_right (fun (s, expr) rem ->
        let (expr', _) = remove_duplicates [] expr eps_expr in
(*
Format.eprintf "@[<3>@[%a@]: %a / %a@]@." Cset.print s print_state expr print_state expr';
*)
        let idx = free_index tbl_ref expr' in
        let expr'' = set_idx idx expr' in
        List.fold_right (fun (cat', s') rem ->
            let s'' = Cset.inter s s' in
            if Cset.is_empty s''
            then rem
            else (s'', State.mk idx cat' expr'') :: rem)
          categories rem) der [])

(****)

let flatten_match m =
  let ma = List.fold_left (fun ma (i, _) -> max ma i) (-1) m in
  let res = Array.make (ma + 1) (-1) in
  List.iter (fun (i, v) -> res.(i) <- v) m;
  res

let status s =
  match s.State.status with
    Some st ->
    st
  | None ->
    let st =
      match s.State.desc with
        []              -> Failed
      | E.TMatch m :: _ -> Match (flatten_match m.Marks.marks, m.Marks.pmarks)
      | _               -> Running
    in
    s.State.status <- Some st;
    st

end
module Re : sig 
#1 "re.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(** Module [Re]: regular expressions commons *)

type t
(** Regular expression *)

type re
(** Compiled regular expression *)

type groups
(** Information about groups in a match. *)

(** {2 Compilation and execution of a regular expression} *)

val compile : t -> re
(** Compile a regular expression into an executable version that can be
    used to match strings, e.g. with {!exec}. *)

val exec :
  ?pos:int ->    (* Default: 0 *)
  ?len:int ->    (* Default: -1 (until end of string) *)
  re -> string -> groups
(** [exec re str] matches [str] against the compiled expression [re],
    and returns the matched groups if any.
    @param pos optional beginning of the string (default 0)
    @param len length of the substring of [str] that can be matched (default [-1],
      meaning to the end of the string
    @raise Not_found if the regular expression can't be found in [str]
*)

val exec_opt :
  ?pos:int ->    (* Default: 0 *)
  ?len:int ->    (* Default: -1 (until end of string) *)
  re -> string -> groups option
(** Similar to {!exec}, but returns an option instead of using an exception. *)

val execp :
  ?pos:int ->    (* Default: 0 *)
  ?len:int ->    (* Default: -1 (until end of string) *)
  re -> string -> bool
(** Similar to {!exec}, but returns [true] if the expression matches,
    and [false] if it doesn't *)

val exec_partial :
  ?pos:int ->    (* Default: 0 *)
  ?len:int ->    (* Default: -1 (until end of string) *)
  re -> string -> [ `Full | `Partial | `Mismatch ]
(** More detailed version of {!exec_p} *)

(** Manipulate matching groups. *)
module Group : sig

  type t = groups
  (** Information about groups in a match. *)

  val get : t -> int -> string
  (** Raise [Not_found] if the group did not match *)

  val offset : t -> int -> int * int
  (** Raise [Not_found] if the group did not match *)

  val start : t -> int -> int
  (** Return the start of the match. Raise [Not_found] if the group did not match. *)

  val stop : t -> int -> int
  (** Return the end of the match. Raise [Not_found] if the group did not match. *)

  val all : t -> string array
  (** Return the empty string for each group which did not match *)

  val all_offset : t -> (int * int) array
  (** Return [(-1,-1)] for each group which did not match *)

  val test : t -> int -> bool
  (** Test whether a group matched *)

  val nb_groups : t -> int
  (** Returns the total number of groups defined - matched or not.
      This function is experimental. *)

  val pp : Format.formatter -> t -> unit

end

(** Marks *)
module Mark : sig

  type t
  (** Mark id *)

  val test : Group.t -> t -> bool
  (** Tell if a mark was matched. *)

  module Set : Set.S with type elt = t

  val all : Group.t -> Set.t
  (** Return all the mark matched. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int

end

(** {2 High Level Operations} *)

type 'a gen = unit -> 'a option

val all :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> Group.t list
(** Repeatedly calls {!exec} on the given string, starting at given
    position and length.*)

val all_gen :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> Group.t gen
(** Same as {!all} but returns a generator *)

val matches :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> string list
(** Same as {!all}, but extracts the matched substring rather than
    returning the whole group. This basically iterates over matched
    strings *)

val matches_gen :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> string gen
(** Same as {!matches}, but returns a generator. *)

val split :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> string list
(** [split re s] splits [s] into chunks separated by [re]. It yields
    the chunks themselves, not the separator. For instance
    this can be used with a whitespace-matching re such as ["[\t ]+"]. *)

val split_gen :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> string gen

type split_token =
  [ `Text of string  (** Text between delimiters *)
  | `Delim of Group.t (** Delimiter *)
  ]

val split_full :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> split_token list

val split_full_gen :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  re -> string -> split_token gen

val replace :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  ?all:bool ->   (** Default: true. Otherwise only replace first occurrence *)
  re ->          (** matched groups *)
  f:(Group.t -> string) ->  (* how to replace *)
  string ->     (** string to replace in *)
  string
(** [replace ~all re ~f s] iterates on [s], and replaces every occurrence
    of [re] with [f substring] where [substring] is the current match.
    If [all = false], then only the first occurrence of [re] is replaced. *)

val replace_string :
  ?pos:int ->    (** Default: 0 *)
  ?len:int ->
  ?all:bool ->   (** Default: true. Otherwise only replace first occurrence *)
  re ->          (** matched groups *)
  by:string ->   (** replacement string *)
  string ->      (** string to replace in *)
  string
(** [replace_string ~all re ~by s] iterates on [s], and replaces every
    occurrence of [re] with [by]. If [all = false], then only the first
    occurrence of [re] is replaced. *)

(** {2 String expressions (literal match)} *)

val str : string -> t
val char : char -> t

(** {2 Basic operations on regular expressions} *)

val alt : t list -> t
(** Alternative *)

val seq : t list -> t
(** Sequence *)

val empty : t
(** Match nothing *)

val epsilon : t
(** Empty word *)

val rep : t -> t
(** 0 or more matches *)

val rep1 : t -> t
(** 1 or more matches *)

val repn : t -> int -> int option -> t
(** [repn re i j] matches [re] at least [i] times
    and at most [j] times, bounds included.
    [j = None] means no upper bound.
*)

val opt : t -> t
(** 0 or 1 matches *)

(** {2 String, line, word} *)

val bol : t
(** Beginning of line *)

val eol : t
(** End of line *)

val bow : t
(** Beginning of word *)

val eow : t
(** End of word *)

val bos : t
(** Beginning of string *)

val eos : t
(** End of string *)

val leol : t
(** Last end of line or end of string *)

val start : t
(** Initial position *)

val stop : t
(** Final position *)

val word : t -> t
(** Word *)

val not_boundary : t
(** Not at a word boundary *)

val whole_string : t -> t
(** Only matches the whole string *)

(** {2 Match semantics} *)

val longest : t -> t
(** Longest match *)

val shortest : t -> t
(** Shortest match *)

val first : t -> t
(** First match *)

(** {2 Repeated match modifiers} *)

val greedy : t -> t
(** Greedy *)

val non_greedy : t -> t
(** Non-greedy *)

(** {2 Groups (or submatches)} *)

val group : t -> t
(** Delimit a group *)

val no_group : t -> t
(** Remove all groups *)

val nest : t -> t
(** when matching against [nest e], only the group matching in the
       last match of e will be considered as matching *)



val mark : t -> Mark.t * t
(** Mark a regexp. the markid can then be used to know if this regexp was used. *)

(** {2 Character sets} *)

val set : string -> t
(** Any character of the string *)

val rg : char -> char -> t
(** Character ranges *)

val inter : t list -> t
(** Intersection of character sets *)

val diff : t -> t -> t
(** Difference of character sets *)

val compl : t list -> t
(** Complement of union *)

(** {2 Predefined character sets} *)

val any : t
(** Any character *)

val notnl : t
(** Any character but a newline *)

val alnum : t
val wordc : t
val alpha : t
val ascii : t
val blank : t
val cntrl : t
val digit : t
val graph : t
val lower : t
val print : t
val punct : t
val space : t
val upper : t
val xdigit : t

(** {2 Case modifiers} *)

val case : t -> t
(** Case sensitive matching *)

val no_case : t -> t
(** Case insensitive matching *)

(****)

(** {2 Internal debugging}  *)

val pp : Format.formatter -> t -> unit

val pp_re : Format.formatter -> re -> unit

(** Alias for {!pp_re}. Deprecated *)
val print_re : Format.formatter -> re -> unit

(** {2 Experimental functions}. *)

val witness : t -> string
(** [witness r] generates a string [s] such that [execp (compile r) s] is
    true *)

(** {2 Deprecated functions} *)

type substrings = Group.t
(** Alias for {!Group.t}. Deprecated *)

val get : Group.t -> int -> string
(** Same as {!Group.get}. Deprecated *)

val get_ofs : Group.t -> int -> int * int
(** Same as {!Group.offset}. Deprecated *)

val get_all : Group.t -> string array
(** Same as {!Group.all}. Deprecated *)

val get_all_ofs : Group.t -> (int * int) array
(** Same as {!Group.all_offset}. Deprecated *)

val test : Group.t -> int -> bool
(** Same as {!Group.test}. Deprecated *)

type markid = Mark.t
(** Alias for {!Mark.t}. Deprecated *)

val marked : Group.t -> Mark.t -> bool
(** Same as {!Mark.test}. Deprecated *)

val mark_set : Group.t -> Mark.Set.t
(** Same as {!Mark.all}. Deprecated *)

end = struct
#1 "re.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

module Cset = Re_cset
module Automata = Re_automata
module MarkSet = Automata.PmarkSet

let rec iter n f v = if n = 0 then v else iter (n - 1) f (f v)

(****)

let unknown = -2
let break = -3

(* Result of a successful match. *)
type groups =
  { s : string
  (* Input string. Matched strings are substrings of s *)

  ; marks : Automata.mark_infos
  (* Mapping from group indices to positions in gpos. group i has positions 2*i
     - 1, 2*i + 1 in gpos. If the group wasn't matched, then its corresponding
     values in marks will be -1,-1 *)

  ; pmarks : MarkSet.t
  (* Marks positions. i.e. those marks created with Re.marks *)

  ; gpos : int array
  (* Group positions. Adjacent elements are (start, stop) of group match.
     indexed by the values in marks. So group i in an re would be the substring:

     start = t.gpos.(marks.(2*i)) - 1
     stop = t.gpos.(marks.(2*i + 1)) - 1 *)

  ; gcount : int
  (* Number of groups the regular expression contains. Matched or not *) }

type match_info =
  | Match of groups
  | Failed
  | Running

type state =
  { idx : int;
    (* Index of the current position in the position table.
       Not yet computed transitions point to a dummy state where
       [idx] is set to [unknown];
       If [idx] is set to [break] for states that either always
       succeed or always fail. *)
    real_idx : int;
    (* The real index, in case [idx] is set to [break] *)
    next : state array;
    (* Transition table, indexed by color *)
    mutable final :
      (Automata.category *
       (Automata.idx * Automata.status)) list;
    (* Mapping from the category of the next character to
       - the index where the next position should be saved
       - possibly, the list of marks (and the corresponding indices)
         corresponding to the best match *)
    desc : Automata.State.t
    (* Description of this state of the automata *) }

(* Automata (compiled regular expression) *)
type re =
  { initial : Automata.expr;
    (* The whole regular expression *)
    mutable initial_states : (Automata.category * state) list;
    (* Initial states, indexed by initial category *)
    cols : Bytes.t;
    (* Color table *)
    col_repr : Bytes.t;
    (* Table from colors to one character of this color *)
    ncol : int;
    (* Number of colors. *)
    lnl : int;
    (* Color of the last newline *)
    tbl : Automata.working_area;
    (* Temporary table used to compute the first available index
       when computing a new state *)
    states : state Automata.State.Table.t;
    (* States of the deterministic automata *)
    group_count : int
    (* Number of groups in the regular expression *) }

let pp_re ch re = Automata.pp ch re.initial

let print_re = pp_re

(* Information used during matching *)
type info =
  { re : re;
    (* The automata *)
    i_cols : Bytes.t;
    (* Color table ([x.i_cols = x.re.cols])
       Shortcut used for performance reasons *)
    mutable positions : int array;
    (* Array of mark positions
       The mark are off by one for performance reasons *)
    pos : int;
    (* Position where the match is started *)
    last : int
    (* Position where the match should stop *) }


(****)

let cat_inexistant = 1
let cat_letter = 2
let cat_not_letter = 4
let cat_newline = 8
let cat_lastnewline = 16
let cat_search_boundary = 32

let category re c =
  if c = -1 then
    cat_inexistant
    (* Special category for the last newline *)
  else if c = re.lnl then
    cat_lastnewline lor cat_newline lor cat_not_letter
  else
    match Bytes.get re.col_repr c with
    (* Should match [cword] definition *)
      'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\170' | '\181' | '\186'
    | '\192'..'\214' | '\216'..'\246' | '\248'..'\255' ->
      cat_letter
    | '\n' ->
      cat_not_letter lor cat_newline
    | _ ->
      cat_not_letter

(****)

let dummy_next = [||]

let unknown_state =
  { idx = unknown; real_idx = 0;
    next = dummy_next; final = [];
    desc = Automata.State.dummy }

let mk_state ncol desc =
  let break_state =
    match Automata.status desc with
    | Automata.Running -> false
    | Automata.Failed
    | Automata.Match _ -> true
  in
  { idx = if break_state then break else desc.Automata.State.idx;
    real_idx = desc.Automata.State.idx;
    next = if break_state then dummy_next else Array.make ncol unknown_state;
    final = [];
    desc = desc }

let find_state re desc =
  try
    Automata.State.Table.find re.states desc
  with Not_found ->
    let st = mk_state re.ncol desc in
    Automata.State.Table.add re.states desc st;
    st

(**** Match with marks ****)

let delta info cat c st =
  let desc = Automata.delta info.re.tbl cat c st.desc in
  let len = Array.length info.positions in
  if desc.Automata.State.idx = len && len > 0 then begin
    let pos = info.positions in
    info.positions <- Array.make (2 * len) 0;
    Array.blit pos 0 info.positions 0 len
  end;
  desc

let validate info (s:string) pos st =
  let c = Char.code (Bytes.get info.i_cols (Char.code s.[pos])) in
  let cat = category info.re c in
  let desc' = delta info cat c st in
  let st' = find_state info.re desc' in
  st.next.(c) <- st'

(*
let rec loop info s pos st =
  if pos < info.last then
    let st' = st.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
    let idx = st'.idx in
    if idx >= 0 then begin
      info.positions.(idx) <- pos;
      loop info s (pos + 1) st'
    end else if idx = break then begin
      info.positions.(st'.real_idx) <- pos;
      st'
    end else begin (* Unknown *)
      validate info s pos st;
      loop info s pos st
    end
  else
    st
*)

let rec loop info (s:string) pos st =
  if pos < info.last then
    let st' = st.next.(Char.code (Bytes.get info.i_cols (Char.code s.[pos]))) in
    loop2 info s pos st st'
  else
    st

and loop2 info s pos st st' =
  if st'.idx >= 0 then begin
    let pos = pos + 1 in
    if pos < info.last then begin
      (* It is important to place these reads before the write *)
      (* But then, we don't have enough registers left to store the
         right position.  So, we store the position plus one. *)
      let st'' = st'.next.(Char.code (Bytes.get info.i_cols (Char.code s.[pos]))) in
      info.positions.(st'.idx) <- pos;
      loop2 info s pos st' st''
    end else begin
      info.positions.(st'.idx) <- pos;
      st'
    end
  end else if st'.idx = break then begin
    info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    validate info s pos st;
    loop info s pos st
  end

let rec loop_no_mark info s pos last st =
  if pos < last then
    let st' = st.next.(Char.code (Bytes.get info.i_cols (Char.code s.[pos]))) in
    if st'.idx >= 0 then
      loop_no_mark info s (pos + 1) last st'
    else if st'.idx = break then
      st'
    else begin (* Unknown *)
      validate info s pos st;
      loop_no_mark info s pos last st
    end
  else
    st

let final info st cat =
  try
    List.assq cat st.final
  with Not_found ->
    let st' = delta info cat (-1) st in
    let res = (st'.Automata.State.idx, Automata.status st') in
    st.final <- (cat, res) :: st.final;
    res

let find_initial_state re cat =
  try
    List.assq cat re.initial_states
  with Not_found ->
    let st = find_state re (Automata.State.create cat re.initial) in
    re.initial_states <- (cat, st) :: re.initial_states;
    st

let get_color re (s:string) pos =
  if pos < 0 then
    -1
  else
    let slen = String.length s in
    if pos >= slen then
      -1
    else if pos = slen - 1 && re.lnl <> -1 && s.[pos] = '\n' then
      (* Special case for the last newline *)
      re.lnl
    else
      Char.code (Bytes.get re.cols (Char.code s.[pos]))

let rec handle_last_newline info pos st groups =
  let st' = st.next.(info.re.lnl) in
  if st'.idx >= 0 then begin
    if groups then info.positions.(st'.idx) <- pos + 1;
    st'
  end else if st'.idx = break then begin
    if groups then info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    let c = info.re.lnl in
    let real_c = Char.code (Bytes.get info.i_cols (Char.code '\n')) in
    let cat = category info.re c in
    let desc' = delta info cat real_c st in
    let st' = find_state info.re desc' in
    st.next.(c) <- st';
    handle_last_newline info pos st groups
  end

let rec scan_str info (s:string) initial_state groups =
  let pos = info.pos in
  let last = info.last in
  if (last = String.length s
      && info.re.lnl <> -1
      && last > pos
      && String.get s (last - 1) = '\n')
  then begin
    let info = { info with last = last - 1 } in
    let st = scan_str info s initial_state groups in
    if st.idx = break then
      st
    else
      handle_last_newline info (last - 1) st groups
  end else if groups then
    loop info s pos initial_state
  else
    loop_no_mark info s pos last initial_state

let match_str ~groups ~partial re s ~pos ~len =
  let slen = String.length s in
  let last = if len = -1 then slen else pos + len in
  let info =
    { re = re; i_cols = re.cols; pos = pos; last = last;
      positions =
        if groups then begin
          let n = Automata.index_count re.tbl + 1 in
          if n <= 10 then
            [|0;0;0;0;0;0;0;0;0;0|]
          else
            Array.make n 0
        end else
          [||] }
  in
  let initial_cat =
    if pos = 0 then
      cat_search_boundary lor cat_inexistant
    else
      cat_search_boundary lor category re (get_color re s (pos - 1)) in
  let initial_state = find_initial_state re initial_cat in
  let st = scan_str info s initial_state groups in
  let res =
    if st.idx = break || partial then
      Automata.status st.desc
    else
      let final_cat =
        if last = slen then
          cat_search_boundary lor cat_inexistant
        else
          cat_search_boundary lor category re (get_color re s last) in
      let (idx, res) = final info st final_cat in
      if groups then info.positions.(idx) <- last + 1;
      res
  in
  match res with
    Automata.Match (marks, pmarks) ->
    Match { s ; marks; pmarks ; gpos = info.positions; gcount = re.group_count}
  | Automata.Failed -> Failed
  | Automata.Running -> Running

let mk_re init cols col_repr ncol lnl group_count =
  { initial = init;
    initial_states = [];
    cols = cols;
    col_repr = col_repr;
    ncol = ncol;
    lnl = lnl;
    tbl = Automata.create_working_area ();
    states = Automata.State.Table.create 97;
    group_count = group_count }

(**** Character sets ****)

let cseq c c' = Cset.seq (Char.code c) (Char.code c')
let cadd c s = Cset.add (Char.code c) s

let trans_set cache cm s =
  match Cset.one_char s with
  | Some i -> Cset.csingle (Bytes.get cm i)
  | None ->
    let v = (Cset.hash_rec s, s) in
    try
      Cset.CSetMap.find v !cache
    with Not_found ->
      let l =
        Cset.fold_right
          s
          ~f:(fun (i, j) l -> Cset.union (cseq (Bytes.get cm i)
                                            (Bytes.get cm j)) l)
          ~init:Cset.empty
      in
      cache := Cset.CSetMap.add v l !cache;
      l

(****)

type regexp =
    Set of Cset.t
  | Sequence of regexp list
  | Alternative of regexp list
  | Repeat of regexp * int * int option
  | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str
  | Last_end_of_line | Start | Stop
  | Sem of Automata.sem * regexp
  | Sem_greedy of Automata.rep_kind * regexp
  | Group of regexp | No_group of regexp | Nest of regexp
  | Case of regexp | No_case of regexp
  | Intersection of regexp list
  | Complement of regexp list
  | Difference of regexp * regexp
  | Pmark of Automata.Pmark.t * regexp

let rec pp fmt t =
  let open Re_fmt in
  let var s re = sexp fmt s pp re in
  let seq s rel = sexp fmt s (list pp) rel in
  match t with
  | Set s ->  sexp fmt "Set" Cset.pp s
  | Sequence sq -> seq "Sequence" sq
  | Alternative alt -> seq "Alternative" alt
  | Repeat (re, start, stop) ->
    let pp' fmt () = fprintf fmt "%a@ %d%a" pp re   start   optint stop in
    sexp fmt "Repeat" pp' ()
  | Beg_of_line      -> str fmt "Beg_of_line"
  | End_of_line      -> str fmt "End_of_line"
  | Beg_of_word      -> str fmt "Beg_of_word"
  | End_of_word      -> str fmt "End_of_word"
  | Not_bound        -> str fmt "Not_bound"
  | Beg_of_str       -> str fmt "Beg_of_str"
  | End_of_str       -> str fmt "End_of_str"
  | Last_end_of_line -> str fmt "Last_end_of_line"
  | Start            -> str fmt "Start"
  | Stop             -> str fmt "Stop"
  | Sem (sem, re)    ->
    sexp fmt "Sem" (pair Automata.pp_sem pp) (sem, re)
  | Sem_greedy (k, re) ->
    sexp fmt "Sem_greedy" (pair Automata.pp_rep_kind pp) (k, re)
  | Group c        -> var "Group" c
  | No_group c     -> var "No_group" c
  | Nest c         -> var "Nest" c
  | Case c         -> var "Case" c
  | No_case c      -> var "No_case" c
  | Intersection c -> seq "Intersection" c
  | Complement c   -> seq "Complement" c
  | Difference (a, b) -> sexp fmt "Difference" (pair pp pp) (a, b)
  | Pmark (m, r)      -> sexp fmt "Pmark" (pair Automata.Pmark.pp pp) (m, r)

let rec is_charset = function
  | Set _ ->
    true
  | Alternative l | Intersection l | Complement l ->
    List.for_all is_charset l
  | Difference (r, r') ->
    is_charset r && is_charset r'
  | Sem (_, r) | Sem_greedy (_, r)
  | No_group r | Case r | No_case r ->
    is_charset r
  | Sequence _ | Repeat _ | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Beg_of_str | End_of_str
  | Not_bound | Last_end_of_line | Start | Stop
  | Group _ | Nest _ | Pmark (_,_)->
    false

(**** Colormap ****)

(*XXX Use a better algorithm allowing non-contiguous regions? *)
let split s cm =
  Re_cset.iter s ~f:(fun i j ->
      Bytes.set cm i '\001';
      Bytes.set cm (j + 1) '\001';
    )

let cupper =
  Cset.union (cseq 'A' 'Z')
    (Cset.union (cseq '\192' '\214') (cseq '\216' '\222'))
let clower = Cset.offset 32 cupper
let calpha =
  List.fold_right cadd ['\170'; '\181'; '\186'; '\223'; '\255']
    (Cset.union clower cupper)
let cdigit = cseq '0' '9'
let calnum = Cset.union calpha cdigit
let cword = cadd '_' calnum

let colorize c regexp =
  let lnl = ref false in
  let rec colorize regexp =
    match regexp with
      Set s                     -> split s c
    | Sequence l                -> List.iter colorize l
    | Alternative l             -> List.iter colorize l
    | Repeat (r, _, _)          -> colorize r
    | Beg_of_line | End_of_line -> split (Cset.csingle '\n') c
    | Beg_of_word | End_of_word
    | Not_bound                 -> split cword c
    | Beg_of_str | End_of_str
    | Start | Stop              -> ()
    | Last_end_of_line          -> lnl := true
    | Sem (_, r)
    | Sem_greedy (_, r)
    | Group r | No_group r
    | Nest r | Pmark (_,r)     -> colorize r
    | Case _ | No_case _
    | Intersection _
    | Complement _
    | Difference _              -> assert false
  in
  colorize regexp;
  !lnl

let make_cmap () = Bytes.make 257 '\000'

let flatten_cmap cm =
  let c = Bytes.create 256 in
  let col_repr = Bytes.create 256 in
  let v = ref 0 in
  Bytes.set c 0 '\000';
  Bytes.set col_repr 0 '\000';
  for i = 1 to 255 do
    if Bytes.get cm i <> '\000' then incr v;
    Bytes.set c i (Char.chr !v);
    Bytes.set col_repr !v (Char.chr i)
  done;
  (c, Bytes.sub col_repr 0 (!v + 1), !v + 1)

(**** Compilation ****)

let rec equal x1 x2 =
  match x1, x2 with
    Set s1, Set s2 ->
    s1 = s2
  | Sequence l1, Sequence l2 ->
    eq_list l1 l2
  | Alternative l1, Alternative l2 ->
    eq_list l1 l2
  | Repeat (x1', i1, j1), Repeat (x2', i2, j2) ->
    i1 = i2 && j1 = j2 && equal x1' x2'
  | Beg_of_line, Beg_of_line
  | End_of_line, End_of_line
  | Beg_of_word, Beg_of_word
  | End_of_word, End_of_word
  | Not_bound, Not_bound
  | Beg_of_str, Beg_of_str
  | End_of_str, End_of_str
  | Last_end_of_line, Last_end_of_line
  | Start, Start
  | Stop, Stop ->
    true
  | Sem (sem1, x1'), Sem (sem2, x2') ->
    sem1 = sem2 && equal x1' x2'
  | Sem_greedy (k1, x1'), Sem_greedy (k2, x2') ->
    k1 = k2 && equal x1' x2'
  | Group _, Group _ -> (* Do not merge groups! *)
    false
  | No_group x1', No_group x2' ->
    equal x1' x2'
  | Nest x1', Nest x2' ->
    equal x1' x2'
  | Case x1', Case x2' ->
    equal x1' x2'
  | No_case x1', No_case x2' ->
    equal x1' x2'
  | Intersection l1, Intersection l2 ->
    eq_list l1 l2
  | Complement l1, Complement l2 ->
    eq_list l1 l2
  | Difference (x1', x1''), Difference (x2', x2'') ->
    equal x1' x2' && equal x1'' x2''
  | Pmark (m1, r1), Pmark (m2, r2) ->
    Automata.Pmark.equal m1 m2 && equal r1 r2
  | _ ->
    false

and eq_list l1 l2 =
  match l1, l2 with
    [], [] ->
    true
  | x1 :: r1, x2 :: r2 ->
    equal x1 x2 && eq_list r1 r2
  | _ ->
    false

let sequence = function
  | [x] -> x
  | l   -> Sequence l

let rec merge_sequences = function
  | [] ->
    []
  | Alternative l' :: r ->
    merge_sequences (l' @ r)
  | Sequence (x :: y) :: r ->
    begin match merge_sequences r with
        Sequence (x' :: y') :: r' when equal x x' ->
        Sequence [x; Alternative [sequence y; sequence y']] :: r'
      | r' ->
        Sequence (x :: y) :: r'
    end
  | x :: r ->
    x :: merge_sequences r

module A = Automata

let enforce_kind ids kind kind' cr =
  match kind, kind' with
    `First, `First -> cr
  | `First, k       -> A.seq ids k cr (A.eps ids)
  |  _               -> cr

(* XXX should probably compute a category mask *)
let rec translate ids kind ign_group ign_case greedy pos cache c = function
  | Set s ->
    (A.cst ids (trans_set cache c s), kind)
  | Sequence l ->
    (trans_seq ids kind ign_group ign_case greedy pos cache c l, kind)
  | Alternative l ->
    begin match merge_sequences l with
        [r'] ->
        let (cr, kind') =
          translate ids kind ign_group ign_case greedy pos cache c r' in
        (enforce_kind ids kind kind' cr, kind)
      | merged_sequences ->
        (A.alt ids
           (List.map
              (fun r' ->
                 let (cr, kind') =
                   translate ids kind ign_group ign_case greedy
                     pos cache c r' in
                 enforce_kind ids kind kind' cr)
              merged_sequences),
         kind)
    end
  | Repeat (r', i, j) ->
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r' in
    let rem =
      match j with
        None ->
        A.rep ids greedy kind' cr
      | Some j ->
        let f =
          match greedy with
            `Greedy ->
            fun rem ->
              A.alt ids
                [A.seq ids kind' (A.rename ids cr) rem; A.eps ids]
          | `Non_greedy ->
            fun rem ->
              A.alt ids
                [A.eps ids; A.seq ids kind' (A.rename ids cr) rem]
        in
        iter (j - i) f (A.eps ids)
    in
    (iter i (fun rem -> A.seq ids kind' (A.rename ids cr) rem) rem, kind)
  | Beg_of_line ->
    (A.after ids (cat_inexistant lor cat_newline), kind)
  | End_of_line ->
    (A.before ids (cat_inexistant lor cat_newline), kind)
  | Beg_of_word ->
    (A.seq ids `First
       (A.after ids (cat_inexistant lor cat_not_letter))
       (A.before ids (cat_inexistant lor cat_letter)),
     kind)
  | End_of_word ->
    (A.seq ids `First
       (A.after ids (cat_inexistant lor cat_letter))
       (A.before ids (cat_inexistant lor cat_not_letter)),
     kind)
  | Not_bound ->
    (A.alt ids [A.seq ids `First
                  (A.after ids cat_letter)
                  (A.before ids cat_letter);
                A.seq ids `First
                  (A.after ids cat_letter)
                  (A.before ids cat_letter)],
     kind)
  | Beg_of_str ->
    (A.after ids cat_inexistant, kind)
  | End_of_str ->
    (A.before ids cat_inexistant, kind)
  | Last_end_of_line ->
    (A.before ids (cat_inexistant lor cat_lastnewline), kind)
  | Start ->
    (A.after ids cat_search_boundary, kind)
  | Stop ->
    (A.before ids cat_search_boundary, kind)
  | Sem (kind', r') ->
    let (cr, kind'') =
      translate ids kind' ign_group ign_case greedy pos cache c r' in
    (enforce_kind ids kind' kind'' cr,
     kind')
  | Sem_greedy (greedy', r') ->
    translate ids kind ign_group ign_case greedy' pos cache c r'
  | Group r' ->
    if ign_group then
      translate ids kind ign_group ign_case greedy pos cache c r'
    else
      let p = !pos in
      pos := !pos + 2;
      let (cr, kind') =
        translate ids kind ign_group ign_case greedy pos cache c r' in
      (A.seq ids `First (A.mark ids p) (
          A.seq ids `First cr (A.mark ids (p + 1))),
       kind')
  | No_group r' ->
    translate ids kind true ign_case greedy pos cache c r'
  | Nest r' ->
    let b = !pos in
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r'
    in
    let e = !pos - 1 in
    if e < b then
      (cr, kind')
    else
      (A.seq ids `First (A.erase ids b e) cr, kind')
  | Difference _ | Complement _ | Intersection _ | No_case _ | Case _ ->
    assert false
  | Pmark (i, r') ->
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r' in
    (A.seq ids `First (A.pmark ids i) cr, kind')

and trans_seq ids kind ign_group ign_case greedy pos cache c = function
  | [] ->
    A.eps ids
  | [r] ->
    let (cr', kind') =
      translate ids kind ign_group ign_case greedy pos cache c r in
    enforce_kind ids kind kind' cr'
  | r :: rem ->
    let (cr', kind') =
      translate ids kind ign_group ign_case greedy pos cache c r in
    let cr'' =
      trans_seq ids kind ign_group ign_case greedy pos cache c rem in
    if A.is_eps cr'' then
      cr'
    else if A.is_eps cr' then
      cr''
    else
      A.seq ids kind' cr' cr''

(**** Case ****)

let case_insens s =
  Cset.union s (Cset.union (Cset.offset 32 (Cset.inter s cupper))
                  (Cset.offset (-32) (Cset.inter s clower)))

let as_set = function
  | Set s -> s
  | _     -> assert false

(* XXX Should split alternatives into (1) charsets and (2) more
   complex regular expressions; alternative should therefore probably
   be flatten here *)
let rec handle_case ign_case = function
  | Set s ->
    Set (if ign_case then case_insens s else s)
  | Sequence l ->
    Sequence (List.map (handle_case ign_case) l)
  | Alternative l ->
    let l' = List.map (handle_case ign_case) l in
    if is_charset (Alternative l') then
      Set (List.fold_left (fun s r -> Cset.union s (as_set r)) Cset.empty l')
    else
      Alternative l'
  | Repeat (r, i, j) ->
    Repeat (handle_case ign_case r, i, j)
  | Beg_of_line | End_of_line | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str | Last_end_of_line | Start | Stop as r ->
    r
  | Sem (k, r) ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Sem (k, r')
  | Sem_greedy (k, r) ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Sem_greedy (k, r')
  | Group r ->
    Group (handle_case ign_case r)
  | No_group r ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else No_group r'
  | Nest r ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Nest r'
  | Case r ->
    handle_case false r
  | No_case r ->
    handle_case true r
  | Intersection l ->
    let l' = List.map (fun r -> handle_case ign_case r) l in
    Set (List.fold_left (fun s r -> Cset.inter s (as_set r)) Cset.cany l')
  | Complement l ->
    let l' = List.map (fun r -> handle_case ign_case r) l in
    Set (Cset.diff Cset.cany
           (List.fold_left (fun s r -> Cset.union s (as_set r))
              Cset.empty l'))
  | Difference (r, r') ->
    Set (Cset.inter (as_set (handle_case ign_case r))
           (Cset.diff Cset.cany (as_set (handle_case ign_case r'))))
  | Pmark (i,r) -> Pmark (i,handle_case ign_case r)

(****)

let compile_1 regexp =
  let regexp = handle_case false regexp in
  let c = make_cmap () in
  let need_lnl = colorize c regexp in
  let (col, col_repr, ncol) = flatten_cmap c in
  let lnl = if need_lnl then ncol else -1 in
  let ncol = if need_lnl then ncol + 1 else ncol in
  let ids = A.create_ids () in
  let pos = ref 0 in
  let (r, kind) =
    translate ids
      `First false false `Greedy pos (ref Cset.CSetMap.empty) col regexp in
  let r = enforce_kind ids `First kind r in
  (*Format.eprintf "<%d %d>@." !ids ncol;*)
  mk_re r col col_repr ncol lnl (!pos / 2)

(****)

let rec anchored = function
  | Sequence l ->
    List.exists anchored l
  | Alternative l ->
    List.for_all anchored l
  | Repeat (r, i, _) ->
    i > 0 && anchored r
  | Set _ | Beg_of_line | End_of_line | Beg_of_word | End_of_word
  | Not_bound | End_of_str | Last_end_of_line | Stop
  | Intersection _ | Complement _ | Difference _ ->
    false
  | Beg_of_str | Start ->
    true
  | Sem (_, r) | Sem_greedy (_, r) | Group r | No_group r | Nest r
  | Case r | No_case r | Pmark (_, r) ->
    anchored r

(****)

type t = regexp

let str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := Set (Cset.csingle s.[i]) :: !l
  done;
  Sequence !l
let char c = Set (Cset.csingle c)

let alt = function
  | [r] -> r
  | l   -> Alternative l
let seq = function
  | [r] -> r
  | l   -> Sequence l

let empty = alt []
let epsilon = seq []
let repn r i j =
  if i < 0 then invalid_arg "Re.repn";
  begin match j with
    | Some j when j < i -> invalid_arg "Re.repn"
    | _ -> ()
  end;
  Repeat (r, i, j)
let rep r = repn r 0 None
let rep1 r = repn r 1 None
let opt r = repn r 0 (Some 1)
let bol = Beg_of_line
let eol = End_of_line
let bow = Beg_of_word
let eow = End_of_word
let word r = seq [bow; r; eow]
let not_boundary = Not_bound
let bos = Beg_of_str
let eos = End_of_str
let whole_string r = seq [bos; r; eos]
let leol = Last_end_of_line
let start = Start
let stop = Stop
let longest r = Sem (`Longest, r)
let shortest r = Sem (`Shortest, r)
let first r = Sem (`First, r)
let greedy r = Sem_greedy (`Greedy, r)
let non_greedy r = Sem_greedy (`Non_greedy, r)
let group r = Group r
let no_group r = No_group r
let nest r = Nest r
let mark r = let i = Automata.Pmark.gen () in (i,Pmark (i,r))

let set str =
  let s = ref Cset.empty in
  for i = 0 to String.length str - 1 do
    s := Cset.union (Cset.csingle str.[i]) !s
  done;
  Set !s

let rg c c' = Set (cseq c c')

let inter l =
  let r = Intersection l in
  if is_charset r then
    r
  else
    invalid_arg "Re.inter"

let compl l =
  let r = Complement l in
  if is_charset r then
    r
  else
    invalid_arg "Re.compl"

let diff r r' =
  let r'' = Difference (r, r') in
  if is_charset r'' then
    r''
  else
    invalid_arg "Re.diff"

let any = Set Cset.cany
let notnl = Set (Cset.diff Cset.cany (Cset.csingle '\n'))

let lower = alt [rg 'a' 'z'; char '\181'; rg '\223' '\246'; rg '\248' '\255']
let upper = alt [rg 'A' 'Z'; rg '\192' '\214'; rg '\216' '\222']
let alpha = alt [lower; upper; char '\170'; char '\186']
let digit = rg '0' '9'
let alnum = alt [alpha; digit]
let wordc = alt [alnum; char '_']
let ascii = rg '\000' '\127'
let blank = set "\t "
let cntrl = alt [rg '\000' '\031'; rg '\127' '\159']
let graph = alt [rg '\033' '\126'; rg '\160' '\255']
let print = alt [rg '\032' '\126'; rg '\160' '\255']
let punct =
  alt [rg '\033' '\047'; rg '\058' '\064'; rg '\091' '\096';
       rg '\123' '\126'; rg '\160' '\169'; rg '\171' '\180';
       rg '\182' '\185'; rg '\187' '\191'; char '\215'; char '\247']
let space = alt [char ' '; rg '\009' '\013']
let xdigit = alt [digit; rg 'a' 'f'; rg 'A' 'F']

let case r = Case r
let no_case r = No_case r

(****)

let compile r =
  compile_1 (
    if anchored r then
      group r
    else
      seq [shortest (rep any); group r]
  )

let exec_internal name ?(pos=0) ?(len = -1) ~groups re s =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg name;
  match_str ~groups ~partial:false re s ~pos ~len

let exec ?pos ?len re s =
  match exec_internal "Re.exec" ?pos ?len ~groups:true re s with
    Match substr -> substr
  | _            -> raise Not_found

let exec_opt ?pos ?len re s =
  match exec_internal "Re.exec_opt" ?pos ?len ~groups:true re s with
    Match substr -> Some substr
  | _            -> None

let execp ?pos ?len re s =
  match exec_internal ~groups:false "Re.execp" ?pos ?len re s with
    Match _substr -> true
  | _             -> false

let exec_partial ?pos ?len re s =
  match exec_internal ~groups:false "Re.exec_partial" ?pos ?len re s with
    Match _ -> `Full
  | Running -> `Partial
  | Failed  -> `Mismatch

module Group = struct

  type t = groups

  let offset t i =
    if 2 * i + 1 >= Array.length t.marks then raise Not_found;
    let m1 = t.marks.(2 * i) in
    if m1 = -1 then raise Not_found;
    let p1 = t.gpos.(m1) - 1 in
    let p2 = t.gpos.(t.marks.(2 * i + 1)) - 1 in
    (p1, p2)

  let get t i =
    let (p1, p2) = offset t i in
    String.sub t.s p1 (p2 - p1)

  let start subs i = fst (offset subs i)

  let stop subs i = snd (offset subs i)

  let test t i =
    if 2 * i >= Array.length t.marks then
      false
    else
      let idx = t.marks.(2 * i) in
      idx <> -1

  let dummy_offset = (-1, -1)

  let all_offset t =
    let res = Array.make t.gcount dummy_offset in
    for i = 0 to Array.length t.marks / 2 - 1 do
      let m1 = t.marks.(2 * i) in
      if m1 <> -1 then begin
        let p1 = t.gpos.(m1) in
        let p2 = t.gpos.(t.marks.(2 * i + 1)) in
        res.(i) <- (p1 - 1, p2 - 1)
      end
    done;
    res

  let dummy_string = ""

  let all t =
    let res = Array.make t.gcount dummy_string in
    for i = 0 to Array.length t.marks / 2 - 1 do
      let m1 = t.marks.(2 * i) in
      if m1 <> -1 then begin
        let p1 = t.gpos.(m1) in
        let p2 = t.gpos.(t.marks.(2 * i + 1)) in
        res.(i) <- String.sub t.s (p1 - 1) (p2 - p1)
      end
    done;
    res

  let pp fmt t =
    let matches =
      let offsets = all_offset t in
      let strs = all t in
      Array.to_list (
        Array.init (Array.length strs) (fun i -> strs.(i), offsets.(i))
      ) in
    let open Re_fmt in
    let pp_match fmt (str, (start, stop)) =
      fprintf fmt "@[(%s (%d %d))@]" str start stop in
    sexp fmt "Group" (list pp_match) matches

  let nb_groups t = t.gcount
end

module Mark = struct

  type t = Automata.Pmark.t

  let test {pmarks ; _} p =
    Automata.PmarkSet.mem p pmarks

  let all s = s.pmarks

  module Set = MarkSet

  let equal = Automata.Pmark.equal

  let compare = Automata.Pmark.compare

end

type 'a gen = unit -> 'a option

let all_gen ?(pos=0) ?len re s =
  if pos < 0 then invalid_arg "Re.all";
  (* index of the first position we do not consider.
     !pos < limit is an invariant *)
  let limit = match len with
    | None -> String.length s
    | Some l ->
      if l<0 || pos+l > String.length s then invalid_arg "Re.all";
      pos+l
  in
  (* iterate on matches. When a match is found, search for the next
     one just after its end *)
  let pos = ref pos in
  fun () ->
    if !pos >= limit
    then None  (* no more matches *)
    else
      match match_str ~groups:true ~partial:false re s
              ~pos:!pos ~len:(limit - !pos) with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        pos := if p1=p2 then p2+1 else p2;
        Some substr
      | Running
      | Failed -> None

let all ?pos ?len re s =
  let l = ref [] in
  let g = all_gen ?pos ?len re s in
  let rec iter () = match g() with
    | None -> List.rev !l
    | Some sub -> l := sub :: !l; iter ()
  in iter ()

let matches_gen ?pos ?len re s =
  let g = all_gen ?pos ?len re s in
  fun () ->
    match g() with
    | None -> None
    | Some sub -> Some (Group.get sub 0)

let matches ?pos ?len re s =
  let l = ref [] in
  let g = all_gen ?pos ?len re s in
  let rec iter () = match g() with
    | None -> List.rev !l
    | Some sub -> l := Group.get sub 0 :: !l; iter ()
  in iter ()

type split_token =
  [ `Text of string
  | `Delim of groups
  ]

let split_full_gen ?(pos=0) ?len re s =
  if pos < 0 then invalid_arg "Re.split";
  let limit = match len with
    | None -> String.length s
    | Some l ->
      if l<0 || pos+l > String.length s then invalid_arg "Re.split";
      pos+l
  in
  (* i: start of delimited string
     pos: first position after last match of [re]
     limit: first index we ignore (!pos < limit is an invariant) *)
  let pos0 = pos in
  let state = ref `Idle in
  let i = ref pos and pos = ref pos in
  let next () = match !state with
    | `Idle when !pos >= limit ->
      if !i < limit then (
        let sub = String.sub s !i (limit - !i) in
        incr i;
        Some (`Text sub)
      ) else None
    | `Idle ->
      begin match match_str ~groups:true ~partial:false re s ~pos:!pos
                    ~len:(limit - !pos) with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        pos := if p1=p2 then p2+1 else p2;
        let old_i = !i in
        i := p2;
        if p1 > pos0 then (
          (* string does not start by a delimiter *)
          let text = String.sub s old_i (p1 - old_i) in
          state := `Yield (`Delim substr);
          Some (`Text text)
        ) else Some (`Delim substr)
      | Running -> None
      | Failed ->
        if !i < limit
        then (
          let text = String.sub s !i (limit - !i) in
          i := limit;
          Some (`Text text)  (* yield last string *)
        ) else
          None
      end
    | `Yield x ->
      state := `Idle;
      Some x
  in next

let split_full ?pos ?len re s =
  let l = ref [] in
  let g = split_full_gen ?pos ?len re s in
  let rec iter () = match g() with
    | None -> List.rev !l
    | Some s -> l := s :: !l; iter ()
  in iter ()

let split_gen ?pos ?len re s =
  let g = split_full_gen ?pos ?len re s in
  let rec next() = match g()  with
    | None -> None
    | Some (`Delim _) -> next()
    | Some (`Text s) -> Some s
  in next

let split ?pos ?len re s =
  let l = ref [] in
  let g = split_full_gen ?pos ?len re s in
  let rec iter () = match g() with
    | None -> List.rev !l
    | Some (`Delim _) -> iter()
    | Some (`Text s) -> l := s :: !l; iter ()
  in iter ()

let replace ?(pos=0) ?len ?(all=true) re ~f s =
  if pos < 0 then invalid_arg "Re.replace";
  let limit = match len with
    | None -> String.length s
    | Some l ->
      if l<0 || pos+l > String.length s then invalid_arg "Re.replace";
      pos+l
  in
  (* buffer into which we write the result *)
  let buf = Buffer.create (String.length s) in
  (* iterate on matched substrings. *)
  let rec iter pos =
    if pos < limit
    then
      match match_str ~groups:true ~partial:false re s ~pos ~len:(limit-pos) with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        (* add string between previous match and current match *)
        Buffer.add_substring buf s pos (p1-pos);
        (* what should we replace the matched group with? *)
        let replacing = f substr in
        Buffer.add_string buf replacing;
        if all then
          (* if we matched a non-char e.g. ^ we must manually advance by 1 *)
          iter (
            if p1=p2 then (
              (* a non char could be past the end of string. e.g. $ *)
              if p2 < limit then Buffer.add_char buf s.[p2];
              p2+1
            ) else
              p2)
        else
          Buffer.add_substring buf s p2 (limit-p2)
      | Running -> ()
      | Failed ->
        Buffer.add_substring buf s pos (limit-pos)
  in
  iter pos;
  Buffer.contents buf

let replace_string ?pos ?len ?all re ~by s =
  replace ?pos ?len ?all re s ~f:(fun _ -> by)

let witness t =
  let rec witness = function
    | Set c -> String.make 1 (Char.chr (Cset.pick c))
    | Sequence xs -> String.concat "" (List.map witness xs)
    | Alternative (x :: _) -> witness x
    | Alternative [] -> assert false
    | Repeat (r, from, _to) ->
      let w = witness r in
      let b = Buffer.create (String.length w * from) in
      for _i=1 to from do
        Buffer.add_string b w
      done;
      Buffer.contents b
    | No_case r -> witness r
    | Intersection _
    | Complement _
    | Difference (_, _) -> assert false
    | Group r
    | No_group r
    | Nest r
    | Sem (_, r)
    | Pmark (_, r)
    | Case r
    | Sem_greedy (_, r) -> witness r
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | Last_end_of_line
    | Start
    | Stop
    | End_of_str -> "" in
  witness (handle_case false t)

(** {2 Deprecated functions} *)

type substrings = groups

let get = Group.get
let get_ofs = Group.offset
let get_all = Group.all
let get_all_ofs = Group.all_offset
let test = Group.test

type markid = Mark.t

let marked = Mark.test
let mark_set = Mark.all

(**********************************)

(*
Information about the previous character:
- does not exists
- is a letter
- is not a letter
- is a newline
- is last newline

Beginning of word:
- previous is not a letter or does not exist
- current is a letter or does not exist

End of word:
- previous is a letter or does not exist
- current is not a letter or does not exist

Beginning of line:
- previous is a newline or does not exist

Beginning of buffer:
- previous does not exist

End of buffer
- current does not exist

End of line
- current is a newline or does not exist
*)

(*
Rep: e = T,e | ()
  - semantics of the comma (shortest/longest/first)
  - semantics of the union (greedy/non-greedy)

Bounded repetition
  a{0,3} = (a,(a,a?)?)?
*)

end
module Re_emacs : sig 
#1 "re_emacs.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(** Emacs-style regular expressions *)

exception Parse_error
exception Not_supported
(** Errors that can be raised during the parsing of the regular expression *)

val re : ?case:bool -> string -> Re.t
(** Parsing of an Emacs-style regular expression *)

val compile : Re.t -> Re.re
(** Regular expression compilation *)

val compile_pat : ?case:bool -> string -> Re.re
(** Same as [Re.compile] *)


end = struct
#1 "re_emacs.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

exception Parse_error
exception Not_supported

let parse s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let test2 c c' = !i + 1 < l && s.[!i] = c && s.[!i + 1] = c' in
  let accept c = let r = test c in if r then incr i; r in
  let accept2 c c' = let r = test2 c c' in if r then i := !i + 2; r in
  let get () = let r = s.[!i] in incr i; r in

  let rec regexp () = regexp' (branch ())
  and regexp' left =
    if accept2 '\\' '|' then regexp' (Re.alt [left; branch ()]) else left
  and branch () = branch' []
  and branch' left =
    if eos () || test2 '\\' '|' || test2 '\\' ')' then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*' then Re.rep r else
    if accept '+' then Re.rep1 r else
    if accept '?' then Re.opt r else
    r
  and atom () =
    if accept '.' then begin
      Re.notnl
    end else if accept '^' then begin
      Re.bol
    end else if accept '$' then begin
      Re.eol
    end else if accept '[' then begin
      if accept '^' then
        Re.compl (bracket [])
      else
        Re.alt (bracket [])
    end else if accept '\\' then begin
      if accept '(' then begin
        let r = regexp () in
        if not (accept2 '\\' ')') then raise Parse_error;
        Re.group r
      end else if accept '`' then
        Re.bos
      else if accept '\'' then
        Re.eos
      else if accept '=' then
        Re.start
      else if accept 'b' then
        Re.alt [Re.bow; Re.eow]
      else if accept 'B' then
        Re.not_boundary
      else if accept '<' then
        Re.bow
      else if accept '>' then
        Re.eow
      else if accept 'w' then
        Re.alt [Re.alnum; Re.char '_']
      else if accept 'W' then
        Re.compl [Re.alnum; Re.char '_']
      else begin
        if eos () then raise Parse_error;
        match get () with
          '*' | '+' | '?' | '[' | ']' | '.' | '^' | '$' | '\\' as c ->
            Re.char c
        | '0' .. '9' ->
            raise Not_supported
        | _ ->
            raise Parse_error
      end
    end else begin
      if eos () then raise Parse_error;
      match get () with
        '*' | '+' | '?' -> raise Parse_error
      |        c        -> Re.char c
    end
  and bracket s =
    if s <> [] && accept ']' then s else begin
      let c = char () in
      if accept '-' then begin
        if accept ']' then Re.char c :: Re.char '-' :: s else begin
          let c' = char () in
          bracket (Re.rg c c' :: s)
        end
      end else
        bracket (Re.char c :: s)
    end
  and char () =
    if eos () then raise Parse_error;
    get ()
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res

let re ?(case = true) s = let r = parse s in if case then r else Re.no_case r

let compile = Re.compile
let compile_pat ?(case = true) s = compile (re ~case s)

end
module Re_glob : sig 
#1 "re_glob.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(** Shell-style regular expressions *)

exception Parse_error

val glob :
  ?anchored:bool ->
  ?pathname:bool ->
  ?period:bool ->
  ?expand_braces:bool ->
  string ->
  Re.t
(** Implements the semantics of shells patterns. The returned regular
    expression is unanchored by default.

    Character '*' matches any sequence of characters and character
    '?' matches a single character.
    A sequence '[...]' matches any one of the enclosed characters.
    A sequence '[^...]' or '[!...]' matches any character *but* the enclosed characters.
    A backslash escapes the following character.  The last character of the string cannot
    be a backslash.

    [anchored] controls whether the regular expression will only match entire
    strings. Defaults to false.

    [pathname]: If this flag is set, match a slash in string only with a slash in pattern
    and not by an asterisk ('*') or a question mark ('?') metacharacter, nor by a bracket
    expression ('[]') containing a slash. Defaults to true.

    [period]: If this flag is set, a leading period in string has to be matched exactly by
    a period in pattern. A period is considered to be leading if it is the first
    character in string, or if both [pathname] is set and the period immediately follows a
    slash. Defaults to true.

    If [expand_braces] is true, braced sets will expand into multiple globs,
    e.g. a\{x,y\}b\{1,2\} matches axb1, axb2, ayb1, ayb2.  As specified for bash, brace
    expansion is purely textual and can be nested. Defaults to false. *)

val glob' : ?anchored:bool -> bool -> string -> Re.t
(** Same, but allows to choose whether dots at the beginning of a
    file name need to be explicitly matched (true) or not (false)

    @deprecated Use [glob ~period].
*)

val globx : ?anchored:bool -> string -> Re.t
(** This version of [glob] also recognizes the pattern \{..,..\}

    @deprecated Prefer [glob ~expand_braces:true].
*)

val globx' : ?anchored:bool -> bool -> string -> Re.t
(** This version of [glob'] also recognizes the pattern \{..,..\}

    @deprecated Prefer [glob ~expand_braces:true ~period].
*)

end = struct
#1 "re_glob.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

exception Parse_error

type enclosed =
  | Char of char
  | Range of char * char

type piece =
  | Exactly of char
  | Any_of of enclosed list
  | Any_but of enclosed list
  | One
  | Many

type t = piece list

let of_string s : t =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let read c =
    let r = not (eos ()) && s.[!i] = c in
    if r then incr i;
    r
  in

  let char () =
    ignore (read '\\' : bool);
    if eos () then raise Parse_error;
    let r = s.[!i] in
    incr i;
    r
  in

  let enclosed () : enclosed list =
    let rec loop s =
      (* This returns the list in reverse order, but order isn't important anyway *)
      if s <> [] && read ']'
      then s
      else
        let c = char () in
        if not (read '-')
        then loop (Char c :: s)
        else if read ']'
        then Char c :: Char '-' :: s
        else
          let c' = char () in
          loop (Range (c, c') :: s)
    in
    loop []
  in

  let piece () =
    if read '*'
    then Many
    else if read '?'
    then One
    else if not (read '[')
    then Exactly (char ())
    else if read '^' || read '!'
    then Any_but (enclosed ())
    else Any_of (enclosed ())
  in

  let rec loop pieces =
    if eos ()
    then List.rev pieces
    else loop (piece () :: pieces)
  in

  loop []

let mul l l' =
  List.flatten (List.map (fun s -> List.map (fun s' -> s ^ s') l') l)

let explode str =
  let l = String.length str in
  let rec expl inner s i acc beg =
    if i >= l then begin
      if inner then raise Parse_error;
      (mul beg [String.sub str s (i - s)], i)
    end else
      match str.[i] with
      | '\\' -> expl inner s (i + 2) acc beg
      | '{' ->
        let (t, i') = expl true (i + 1) (i + 1) [] [""] in
        expl inner i' i' acc
          (mul beg (mul [String.sub str s (i - s)] t))
      | ',' when inner ->
        expl inner (i + 1) (i + 1)
          (mul beg [String.sub str s (i - s)] @ acc) [""]
      | '}' when inner ->
        (mul beg [String.sub str s (i - s)] @ acc, i + 1)
      | _ ->
        expl inner s (i + 1) acc beg
  in
  List.rev (fst (expl false 0 0 [] [""]))

module State = struct
  type t = {
    re_pieces                : Re.t list;  (* last piece at head of list. *)
    remaining                : piece list; (* last piece at tail of list. *)
    am_at_start_of_pattern   : bool;       (* true at start of pattern *)
    am_at_start_of_component : bool;       (* true at start of pattern or immediately
                                              after '/' *)
    pathname                 : bool;
    period                   : bool;
  }

  let create ~period ~pathname remaining =
    {
      re_pieces = [];
      am_at_start_of_pattern = true;
      am_at_start_of_component = true;
      pathname;
      period;
      remaining;
    }

  let explicit_period t =
    t.period && (
      t.am_at_start_of_pattern ||
      (t.am_at_start_of_component && t.pathname)
    )

  let explicit_slash t = t.pathname

  let append ?(am_at_start_of_component=false) t piece =
    { t with
      re_pieces = piece :: t.re_pieces;
      am_at_start_of_pattern = false;
      am_at_start_of_component;
    }

  let to_re t = Re.seq (List.rev t.re_pieces)

  let next t =
    match t.remaining with
    | [] -> None
    | piece :: remaining -> Some (piece, { t with remaining })
end

let one ~explicit_slash ~explicit_period =
  Re.(compl (
    List.concat [
      if explicit_slash  then [char '/'] else [];
      if explicit_period then [char '.'] else [];
    ]
  ))

let enclosed enclosed =
  match enclosed with
  | Char c -> Re.char c
  | Range (low, high) -> Re.rg low high

let enclosed_set ~explicit_slash ~explicit_period kind set =
  let set = List.map enclosed set in
  let enclosure =
    match kind with
    | `Any_of -> Re.alt set
    | `Any_but -> Re.compl set
  in
  Re.inter [enclosure; one ~explicit_slash ~explicit_period]

let exactly state c =
  State.append state (Re.char c) ~am_at_start_of_component:(c = '/')

let many (state : State.t) =
  let explicit_slash = State.explicit_slash state in
  let explicit_period = State.explicit_period state in
  (* Whether we must explicitly match period depends on the surrounding characters, but
     slashes are easy to explicit match. This conditional splits out some simple cases.
  *)
  if not explicit_period then begin
    State.append state (Re.rep (one ~explicit_slash ~explicit_period))
  end else if not explicit_slash then begin
    (* In this state, we explicitly match periods only at the very beginning *)
    State.append state (Re.opt (
      Re.seq [
        one         ~explicit_slash:false ~explicit_period;
        Re.rep (one ~explicit_slash:false ~explicit_period:false);
      ]
    ))
  end else begin
    let not_empty =
      Re.seq [
        one         ~explicit_slash:true ~explicit_period:true;
        Re.rep (one ~explicit_slash:true ~explicit_period:false);
      ]
    in
    (* [maybe_empty] is the default translation of Many, except in some special cases.
    *)
    let maybe_empty = Re.opt not_empty in
    let enclosed_set state kind set =
      State.append state (Re.alt [
        enclosed_set kind set ~explicit_slash:true ~explicit_period:true;
        Re.seq [
          not_empty;
          (* Since [not_empty] matched, subsequent dots are not leading. *)
          enclosed_set kind set ~explicit_slash:true ~explicit_period:false;
        ];
      ])
    in
    let rec lookahead state =
      match State.next state with
      | None -> State.append state maybe_empty
      (* glob ** === glob * . *)
      | Some (Many, state) -> lookahead state
      | Some (Exactly c, state) ->
        let state =
          State.append state
            (if c = '.'
             then not_empty
             else maybe_empty)
        in
        exactly state c
      (* glob *? === glob ?* *)
      | Some (One, state) -> State.append state not_empty
      | Some (Any_of enclosed, state) -> enclosed_set state `Any_of enclosed
      | Some (Any_but enclosed, state) -> enclosed_set state `Any_but enclosed
    in
    lookahead state
  end

let piece state piece =
  let explicit_slash = State.explicit_slash state in
  let explicit_period = State.explicit_period state in
  match piece with
  | One -> State.append state (one ~explicit_slash ~explicit_period)
  | Many -> many state
  | Any_of enclosed ->
    State.append state (enclosed_set `Any_of ~explicit_slash ~explicit_period enclosed)
  | Any_but enclosed ->
    State.append state (enclosed_set `Any_but ~explicit_slash ~explicit_period enclosed)
  | Exactly c -> exactly state c

let glob ~pathname ~period glob =
  let rec loop state =
    match State.next state with
    | None -> State.to_re state
    | Some (p, state) -> loop (piece state p)
  in
  loop (State.create ~pathname ~period glob)

let glob
      ?(anchored = false)
      ?(pathname = true)
      ?(period = true)
      ?(expand_braces = false)
      s
  =
  let to_re s =
    let re = glob ~pathname ~period (of_string s) in
    if anchored
    then Re.whole_string re
    else re
  in
  if expand_braces
  then Re.alt (List.map to_re (explode s))
  else to_re s

let glob' ?anchored period s = glob ?anchored ~period s

let globx ?anchored s = glob ?anchored ~expand_braces:true s

let globx' ?anchored period s = glob ?anchored ~expand_braces:true ~period s

end
module Re_perl : sig 
#1 "re_perl.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(** Perl-style regular expressions *)

exception Parse_error
exception Not_supported
(** Errors that can be raised during the parsing of the regular expression *)


type opt =
  [ `Ungreedy | `Dotall | `Dollar_endonly
  | `Multiline | `Anchored | `Caseless ]

val re : ?opts:opt list -> string -> Re.t
(** Parsing of a Perl-style regular expression *)

val compile : Re.t -> Re.re
(** Regular expression compilation *)

val compile_pat : ?opts:opt list -> string -> Re.re
(** (Same as [Re.compile]) *)

end = struct
#1 "re_perl.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

exception Parse_error
exception Not_supported

let posix_class_of_string = function
  | "alnum"  -> Re.alnum
  | "ascii"  -> Re.ascii
  | "blank"  -> Re.blank
  | "cntrl"  -> Re.cntrl
  | "digit"  -> Re.digit
  | "lower"  -> Re.lower
  | "print"  -> Re.print
  | "space"  -> Re.space
  | "upper"  -> Re.upper
  | "word"   -> Re.wordc
  | "punct"  -> Re.punct
  | "graph"  -> Re.graph
  | "xdigit" -> Re.xdigit
  | class_   -> invalid_arg ("Invalid pcre class: " ^ class_)

let posix_class_strings =
  [ "alnum" ; "ascii" ; "blank"
  ; "cntrl" ; "digit" ; "lower"
  ; "print" ; "space" ; "upper"
  ; "word"  ; "punct" ; "graph"
  ; "xdigit" ]

let parse multiline dollar_endonly dotall ungreedy s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let accept_s s' =
    let len = String.length s' in
    try
      for j = 0 to len - 1 do
        try if s'.[j] <> s.[!i + j] then raise Exit
        with _ -> raise Exit
      done;
      i := !i + len;
      true
    with Exit -> false in
  let get () = let r = s.[!i] in incr i; r in
  let unget () = decr i in
  let greedy_mod r =
    let gr = accept '?' in
    let gr = if ungreedy then not gr else gr in
    if gr then Re.non_greedy r else Re.greedy r
  in
  let rec regexp () = regexp' (branch ())
  and regexp' left =
    if accept '|' then regexp' (Re.alt [left; branch ()]) else left
  and branch () = branch' []
  and branch' left =
    if eos () || test '|' || test ')' then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*' then greedy_mod (Re.rep r) else
    if accept '+' then greedy_mod (Re.rep1 r) else
    if accept '?' then greedy_mod (Re.opt r) else
    if accept '{' then
      match integer () with
        Some i ->
          let j = if accept ',' then integer () else Some i in
          if not (accept '}') then raise Parse_error;
          begin match j with
            Some j when j < i -> raise Parse_error | _ -> ()
          end;
          greedy_mod (Re.repn r i j)
      | None ->
          unget (); r
    else
      r
  and atom () =
    if accept '.' then begin
      if dotall then Re.any else Re.notnl
    end else if accept '(' then begin
      if accept '?' then begin
        if accept ':' then begin
          let r = regexp () in
          if not (accept ')') then raise Parse_error;
          r
        end else if accept '#' then begin
          comment ()
        end else
          raise Parse_error
      end else begin
        let r = regexp () in
        if not (accept ')') then raise Parse_error;
        Re.group r
      end
    end else
    if accept '^' then begin
      if multiline then Re.bol else Re.bos
    end else if accept '$' then begin
      if multiline then Re.eol else if dollar_endonly then Re.leol else Re.eos
    end else if accept '[' then begin
      if accept '^' then
        Re.compl (bracket [])
      else
        Re.alt (bracket [])
    end else if accept '\\' then begin
(* XXX
   - Back-references
   - \cx (control-x), \e, \f, \n, \r, \t, \xhh, \ddd
*)
      if eos () then raise Parse_error;
      match get () with
        'w' ->
          Re.alt [Re.alnum; Re.char '_']
      | 'W' ->
          Re.compl [Re.alnum; Re.char '_']
      | 's' ->
          Re.space
      | 'S' ->
          Re.compl [Re.space]
      | 'd' ->
          Re.digit
      | 'D' ->
          Re.compl [Re.digit]
      | 'b' ->
          Re.alt [Re.bow; Re.eow]
      | 'B' ->
          Re.not_boundary
      | 'A' ->
          Re.bos
      | 'Z' ->
          Re.leol
      | 'z' ->
          Re.eos
      | 'G' ->
          Re.start
      | 'a'..'z' | 'A'..'Z' ->
          raise Parse_error
      | '0'..'9' ->
          raise Not_supported
      | c ->
          Re.char c
    end else begin
      if eos () then raise Parse_error;
      match get () with
        '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
      |                 c            -> Re.char c
    end
  and integer () =
    if eos () then None else
    match get () with
      '0'..'9' as d -> integer' (Char.code d - Char.code '0')
    |     _        -> unget (); None
  and integer' i =
    if eos () then Some i else
    match get () with
      '0'..'9' as d ->
        let i' = 10 * i + (Char.code d - Char.code '0') in
        if i' < i then raise Parse_error;
        integer' i'
    | _ ->
        unget (); Some i
  and bracket s =
    if s <> [] && accept ']' then s else begin
      match char () with
      | `Char c ->
        if accept '-' then begin
          if accept ']' then Re.char c :: Re.char '-' :: s else begin
            match char () with
              `Char c' ->
              bracket (Re.rg c c' :: s)
            | `Set st' ->
              Re.char c :: Re.char '-' :: st' :: s
          end
        end else
          bracket (Re.char c :: s)
      | `Set st -> bracket (st :: s)
    end
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if c = '[' then begin
      if accept '=' then raise Not_supported;
      if accept ':' then
        let compl = accept '^' in
        let cls =
          try List.find accept_s posix_class_strings
          with Not_found -> raise Parse_error in
        if not (accept_s ":]") then raise Parse_error;
        let re =
          let posix_class = posix_class_of_string cls in
          if compl then Re.compl [posix_class] else posix_class in
        `Set (re)
      else if accept '.' then begin
        if eos () then raise Parse_error;
        let c = get () in
        if not (accept '.') then raise Not_supported;
        if not (accept ']') then raise Parse_error;
        `Char c
      end else
        `Char c
    end else if c = '\\' then begin
      let c = get () in
(* XXX
   \127, ...
*)
      match c with
        'b' -> `Char '\008'
      | 'n' -> `Char '\n' (*XXX*)
      | 'r' -> `Char '\r' (*XXX*)
      | 't' -> `Char '\t' (*XXX*)
      | 'w' -> `Set (Re.alt [Re.alnum; Re.char '_'])
      | 'W' -> `Set (Re.compl [Re.alnum; Re.char '_'])
      | 's' -> `Set (Re.space)
      | 'S' -> `Set (Re.compl [Re.space])
      | 'd' -> `Set (Re.digit)
      | 'D' -> `Set (Re.compl [Re.digit])
      | 'a'..'z' | 'A'..'Z' ->
          raise Parse_error
      | '0'..'9' ->
          raise Not_supported
      | _ ->
          `Char c
    end else
      `Char c
  and comment () =
    if accept ')' then Re.epsilon else begin incr i; comment () end
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res

type opt =
  [ `Ungreedy | `Dotall | `Dollar_endonly
  | `Multiline | `Anchored | `Caseless ]

let re  ?(opts = []) s =
  let r =
    parse
      (List.memq `Multiline opts) (List.memq `Dollar_endonly opts)
      (List.memq `Dotall opts) (List.memq `Ungreedy opts)
      s
  in
  let r = if List.memq `Anchored opts then Re.seq [Re.start; r] else r in
  let r = if List.memq `Caseless opts then Re.no_case r else r in
  r

let compile = Re.compile
let compile_pat ?(opts = []) s = compile (re ~opts s)

end
module Re_pcre : sig 
#1 "re_pcre.mli"
type regexp = Re.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type groups = Re.groups

(** Result of a {!Pcre.full_split} *)
type split_result =
  | Text  of string       (** Text part of splitted string *)
  | Delim of string       (** Delimiter part of splitted string *)
  | Group of int * string (** Subgroup of matched delimiter (subgroup_nr, subgroup_str) *)
  | NoGroup               (** Unmatched subgroup *)

val re : ?flags:(flag list) -> string -> Re.t
(** [re ~flags s] creates the regexp [s] using the pcre syntax. *)

val regexp : ?flags:(flag list) -> string -> regexp
(** [re ~flags s] compiles the regexp [s] using the pcre syntax. *)

val extract : rex:regexp -> string -> string array
(** [extract ~rex s] executes [rex] on [s] and returns the matching groups. *)

val exec : rex:regexp -> ?pos:int -> string -> groups
(** Equivalent to {!Re.exec}. *)

val get_substring : groups -> int -> string
(** Equivalent to {!Re.Group.get}. *)

val get_substring_ofs : groups -> int -> int * int
(** Equivalent to {!Re.Group.offset}. *)

val pmatch : rex:regexp -> string -> bool
(** Equivalent to {!Re.execp}. *)

val substitute : rex:Re.re -> subst:(string -> string) -> string -> string

val full_split : ?max:int -> rex:regexp -> string -> split_result list

val split : rex:regexp -> string -> string list

val quote : string -> string

(** {2 Deprecated} *)

type substrings = Re.groups

end = struct
#1 "re_pcre.ml"
type regexp = Re.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type split_result =
  | Text  of string
  | Delim of string
  | Group of int * string
  | NoGroup

type groups = Re.groups

let re ?(flags = []) pat =
  let opts = List.map (function
    | `CASELESS -> `Caseless
    | `MULTILINE -> `Multiline
    | `ANCHORED -> `Anchored
  ) flags in
  Re_perl.re ~opts pat

let regexp ?flags pat = Re.compile (re ?flags pat)

let extract ~rex s =
  Re.Group.all (Re.exec rex s)

let exec ~rex ?pos s =
  Re.exec rex ?pos s

let get_substring s i =
  Re.Group.get s i

let get_substring_ofs s i =
  Re.Group.offset s i

let pmatch ~rex s =
  Re.execp rex s

let substitute ~rex ~subst str =
  let b = Buffer.create 1024 in
  let rec loop pos =
    if pos >= String.length str then
      Buffer.contents b
    else if Re.execp ~pos rex str then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.Group.offset ss 0 in
      let pat = Re.Group.get ss 0 in
      Buffer.add_substring b str pos (start - pos);
      Buffer.add_string b (subst pat);
      loop fin
    ) else (
      Buffer.add_substring b str pos (String.length str - pos);
      loop (String.length str)
    )
  in
  loop 0

let split ~rex str =
  let rec loop accu pos =
    if pos >= String.length str then
      List.rev accu
    else if Re.execp ~pos rex str then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.Group.offset ss 0 in
      let s = String.sub str pos (start - pos) in
      loop (s :: accu) fin
    ) else (
      let s = String.sub str pos (String.length str - pos) in
      loop (s :: accu) (String.length str)
    ) in
  loop [] 0

(* From PCRE *)
let string_unsafe_sub s ofs len =
  let r = Bytes.create len in
  Bytes.unsafe_blit s ofs r 0 len;
  Bytes.unsafe_to_string r

let quote s =
  let len = String.length s in
  let buf = Bytes.create (len lsl 1) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | '\\' | '^' | '$' | '.' | '[' | '|'
    | '('  | ')' | '?' | '*' | '+' | '{' as c ->
      Bytes.unsafe_set buf !pos '\\';
      incr pos;
      Bytes.unsafe_set buf !pos c; incr pos
    | c -> Bytes.unsafe_set buf !pos c; incr pos
  done;
  string_unsafe_sub buf 0 !pos

let full_split ?(max=0) ~rex s =
  if String.length s = 0 then []
  else if max = 1 then [Text s]
  else
    let results = Re.split_full rex s in
    let matches =
      List.map (function
        | `Text s -> [Text s]
        | `Delim d ->
          let matches = Re.Group.all_offset d in
          let delim = Re.Group.get d 0 in
          (Delim delim)::(
            let l = ref [] in
            for i = 1 to Array.length matches - 1 do
              l :=
                (if matches.(i) = (-1, -1)
                 then NoGroup
                 else Group (i, Re.Group.get d i))
                ::(!l)
            done;
            List.rev !l)) results in
    List.concat matches


type substrings = Re.groups

end
module Re_posix : sig 
#1 "re_posix.mli"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(**
References:
  - {{: http://www.opengroup.org/onlinepubs/007908799/xbd/re.html} re}
  - {{: http://www.opengroup.org/onlinepubs/007908799/xsh/regcomp.html} regcomp}

Example of how to use this module (to parse some IRC logs):

{[
type msg = {
  time:string;
  author:string;
  content:string;
}

let re = Re.compile (Re_posix.re "([^:].*:[^:]*:[^:]{2})<.([^>]+)> (.+)$")

(* parse a line *)
let match_line line =
  try
    let substrings = Re.exec re line in
    let groups = Re.get_all substrings in
    (* groups can be obtained directly by index within [substrings] *)
    Some {time=groups.(1); author=groups.(2); content=groups.(3)}
  with Not_found ->
    None (* regex didn't match *)
]}
*)

(** XXX Character classes *)

exception Parse_error
exception Not_supported
(** Errors that can be raised during the parsing of the regular expression *)

type opt = [`ICase | `NoSub | `Newline]

val re : ?opts:(opt list) -> string -> Re.t
(** Parsing of a Posix extended regular expression *)

val compile : Re.t -> Re.re
(** Regular expression compilation *)

val compile_pat : ?opts:(opt list) -> string -> Re.re
(** [compile r] is defined as [Re.compile (Re.longest r)] *)

(*
Deviation from the standard / ambiguities in the standard
---------------------------------------------------------
We tested the behavior of the Linux library (glibc) and the Solaris
library.

(1) An expression [efg] should be parsed as [(ef)g].
    All implementations parse it as [e(fg)].
(2) When matching the pattern "((a)|b)*" against the string "ab",
    the sub-expression "((a)|b)" should match "b", and the
    sub-expression "(a)" should not match anything.
    In both implementation, the sub-expression "(a)" matches "a".
(3) When matching the pattern "(aa?)*" against the string "aaa", it is
    not clear whether the final match of the sub-expression "(aa?)"  is
    the last "a" (all matches of the sub-expression are successively
    maximized), or "aa" (the final match is maximized).
    Both implementations implements the first case.
(4) When matching the pattern "((a?)|b)*" against the string "ab",
    the sub-expression "((a?)|b)" should match the empty string at the
    end of the string (it is better to match the empty string than to
    match nothing).
    In both implementations, this sub-expression matches "b".
    (Strangely, in the Linux implementation, the sub-expression "(a?)"
     correctly matches the empty string at the end of the string)

This library behaves the same way as the other libraries for all
points, except for (2) and (4) where it follows the standard.

The behavior of this library in theses four cases may change in future
releases.
*)

end = struct
#1 "re_posix.ml"
(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(*
What we could (should?) do:
- a* ==> longest ((shortest (no_group a)* ), a | ())  (!!!)
- abc understood as (ab)c
- "((a?)|b)" against "ab" should not bind the first subpattern to anything

Note that it should be possible to handle "(((ab)c)d)e" efficiently
*)

exception Parse_error
exception Not_supported

let parse newline s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let get () = let r = s.[!i] in incr i; r in
  let unget () = decr i in

  let rec regexp () = regexp' (branch ())
  and regexp' left =
    if accept '|' then regexp' (Re.alt [left; branch ()]) else left
  and branch () = branch' []
  and branch' left =
    if eos () || test '|' || test ')' then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*' then Re.rep (Re.nest r) else
    if accept '+' then Re.rep1 (Re.nest r) else
    if accept '?' then Re.opt r else
    if accept '{' then
      match integer () with
        Some i ->
          let j = if accept ',' then integer () else Some i in
          if not (accept '}') then raise Parse_error;
          begin match j with
            Some j when j < i -> raise Parse_error | _ -> ()
          end;
          Re.repn (Re.nest r) i j
      | None ->
          unget (); r
    else
      r
  and atom () =
    if accept '.' then begin
      if newline then Re.notnl else Re.any
    end else if accept '(' then begin
      let r = regexp () in
      if not (accept ')') then raise Parse_error;
      Re.group r
    end else
    if accept '^' then begin
      if newline then Re.bol else Re.bos
    end else if accept '$' then begin
      if newline then Re.eol else Re.eos
    end else if accept '[' then begin
      if accept '^' then
        Re.diff (Re.compl (bracket [])) (Re.char '\n')
      else
        Re.alt (bracket [])
    end else
    if accept '\\' then begin
      if eos () then raise Parse_error;
      match get () with
        '|' | '(' | ')' | '*' | '+' | '?'
      | '[' | '.' | '^' | '$' | '{' | '\\' as c -> Re.char c
      |                 _                       -> raise Parse_error
    end else begin
      if eos () then raise Parse_error;
      match get () with
        '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
      |                 c            -> Re.char c
    end
  and integer () =
    if eos () then None else
    match get () with
      '0'..'9' as d -> integer' (Char.code d - Char.code '0')
    |     _        -> unget (); None
  and integer' i =
    if eos () then Some i else
    match get () with
      '0'..'9' as d ->
        let i' = 10 * i + (Char.code d - Char.code '0') in
        if i' < i then raise Parse_error;
        integer' i'
    | _ ->
        unget (); Some i
  and bracket s =
    if s <> [] && accept ']' then s else begin
      let c = char () in
      if accept '-' then begin
        if accept ']' then Re.char c :: Re.char '-' :: s else begin
          let c' = char () in
          bracket (Re.rg c c' :: s)
        end
      end else
        bracket (Re.char c :: s)
    end
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if c = '[' then begin
      if accept '=' then raise Not_supported
      else if accept ':' then begin
        raise Not_supported (*XXX*)
      end else if accept '.' then begin
        if eos () then raise Parse_error;
        let c = get () in
        if not (accept '.') then raise Not_supported;
        if not (accept ']') then raise Parse_error;
        c
      end else
        c
    end else
      c
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res

type opt = [`ICase | `NoSub | `Newline]

let re ?(opts = []) s =
  let r = parse (List.memq `Newline opts) s in
  let r = if List.memq `ICase opts then Re.no_case r else r in
  let r = if List.memq `NoSub opts then Re.no_group r else r in
  r

let compile re = Re.compile (Re.longest re)
let compile_pat ?(opts = []) s = compile (re ~opts s)

end
module Re_str : sig 
#1 "re_str.mli"
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  linking exception.                                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: re_str.mli,v 1.1 2002/01/16 14:16:04 vouillon Exp $ *)

(** Module [Str]: regular expressions and high-level string processing *)

(** {2 Regular expressions} *)

type regexp
(** The type of compiled regular expressions. *)

val regexp: string -> regexp
(** Compile a regular expression. The syntax for regular expressions
           is the same as in Gnu Emacs. The special characters are
           [$^.*+?[]]. The following constructs are recognized:
    -          [.     ] matches any character except newline
    -          [*     ] (postfix) matches the previous expression zero, one or
                    several times
    -          [+     ] (postfix) matches the previous expression one or
                    several times
    -          [?     ] (postfix) matches the previous expression once or
                    not at all
    -          [[..]  ] character set; ranges are denoted with [-], as in [[a-z]];
                    an initial [^], as in [[^0-9]], complements the set
    -          [^     ] matches at beginning of line
    -          [$     ] matches at end of line
    -          [\|    ] (infix) alternative between two expressions
    -          [\(..\)] grouping and naming of the enclosed expression
    -          [\1    ] the text matched by the first [\(...\)] expression
                    ([\2] for the second expression, etc)
    -          [\b    ] matches word boundaries
    -          [\     ] quotes special characters. *)

val regexp_case_fold: string -> regexp
(** Same as [regexp], but the compiled expression will match text
           in a case-insensitive way: uppercase and lowercase letters will
           be considered equivalent. *)

val quote: string -> string
(** [Str.quote s] returns a regexp string that matches exactly
    [s] and nothing else. *)

val regexp_string: string -> regexp
val regexp_string_case_fold: string -> regexp
(** [Str.regexp_string s] returns a regular expression
    that matches exactly [s] and nothing else.
    [Str.regexp_string_case_fold] is similar, but the regexp
    matches in a case-insensitive way. *)

(** {2 String matching and searching} *)

val string_match: regexp -> string -> int -> bool
(** [string_match r s start] tests whether the characters in [s]
           starting at position [start] match the regular expression [r].
           The first character of a string has position [0], as usual. *)

val search_forward: regexp -> string -> int -> int
(** [search_forward r s start] searches the string [s] for a substring
           matching the regular expression [r]. The search starts at position
           [start] and proceeds towards the end of the string.
           Return the position of the first character of the matched
           substring, or raise [Not_found] if no substring matches. *)

val search_backward: regexp -> string -> int -> int
(** Same as [search_forward], but the search proceeds towards the
           beginning of the string. *)

val string_partial_match: regexp -> string -> int -> bool
(** Similar to [string_match], but succeeds whenever the argument
           string is a prefix of a string that matches.  This includes
           the case of a true complete match. *)

val matched_string: string -> string
(** [matched_string s] returns the substring of [s] that was matched
           by the latest [string_match], [search_forward] or [search_backward].
           The user must make sure that the parameter [s] is the same string
           that was passed to the matching or searching function. *)

val match_beginning: unit -> int
val match_end: unit -> int
(** [match_beginning ()] returns the position of the first character
    of the substring that was matched by [string_match],
    [search_forward] or [search_backward]. [match_end ()] returns
    the position of the character following the last character of
    the matched substring.  *)

val matched_group: int -> string -> string
(** [matched_group n s] returns the substring of [s] that was matched
    by the [n]th group [\(...\)] of the regular expression during
    the latest [string_match], [search_forward] or [search_backward].
    The user must make sure that the parameter [s] is the same string
    that was passed to the matching or searching function.
    [matched_group n s] raises [Not_found] if the [n]th group
    of the regular expression was not matched.  This can happen
    with groups inside alternatives [\|], options [?]
    or repetitions [*].  For instance, the empty string will match
    [\(a\)*], but [matched_group 1 ""] will raise [Not_found]
    because the first group itself was not matched. *)

val group_beginning: int -> int
val group_end: int -> int
(** [group_beginning n] returns the position of the first character
    of the substring that was matched by the [n]th group of
    the regular expression. [group_end n] returns
    the position of the character following the last character of
    the matched substring.  Both functions raise [Not_found]
    if the [n]th group of the regular expression
    was not matched. *)

(** {2 Replacement} *)

val global_replace: regexp -> string -> string -> string
(** [global_replace regexp templ s] returns a string identical to [s],
    except that all substrings of [s] that match [regexp] have been
    replaced by [templ]. The replacement template [templ] can contain
    [\1], [\2], etc; these sequences will be replaced by the text
    matched by the corresponding group in the regular expression.
    [\0] stands for the text matched by the whole regular expression. *)

val replace_first: regexp -> string -> string -> string
(** Same as [global_replace], except that only the first substring
   matching the regular expression is replaced. *)

val global_substitute: regexp -> (string -> string) -> string -> string
(** [global_substitute regexp subst s] returns a string identical
           to [s], except that all substrings of [s] that match [regexp]
           have been replaced by the result of function [subst]. The
           function [subst] is called once for each matching substring,
           and receives [s] (the whole text) as argument. *)

val substitute_first: regexp -> (string -> string) -> string -> string
(** Same as [global_substitute], except that only the first substring
           matching the regular expression is replaced. *)

val replace_matched : string -> string -> string
(** [replace_matched repl s] returns the replacement text [repl]
    in which [\1], [\2], etc. have been replaced by the text
    matched by the corresponding groups in the most recent matching
    operation.  [s] must be the same string that was matched during
    this matching operation. *)

(** {2 Splitting} *)

val split: regexp -> string -> string list
(** [split r s] splits [s] into substrings, taking as delimiters
    the substrings that match [r], and returns the list of substrings.
    For instance, [split (regexp "[ \t]+") s] splits [s] into
    blank-separated words.  An occurrence of the delimiter at the
    beginning and at the end of the string is ignored. *)

val bounded_split: regexp -> string -> int -> string list
(** Same as [split], but splits into at most [n] substrings,
    where [n] is the extra integer parameter. *)

val split_delim: regexp -> string -> string list
val bounded_split_delim: regexp -> string -> int -> string list
(** Same as [split] and [bounded_split], but occurrences of the
    delimiter at the beginning and at the end of the string are
    recognized and returned as empty strings in the result.
    For instance, [split_delim (regexp " ") " abc "]
    returns [[""; "abc"; ""]], while [split] with the same
    arguments returns [["abc"]]. *)

type split_result = Text of string | Delim of string

val full_split: regexp -> string -> split_result list
val bounded_full_split: regexp -> string -> int -> split_result list
(** Same as [split_delim] and [bounded_split_delim], but returns
    the delimiters as well as the substrings contained between
    delimiters.  The former are tagged [Delim] in the result list;
    the latter are tagged [Text].  For instance,
    [full_split (regexp "[{}]") "{ab}"] returns
    [[Delim "{"; Text "ab"; Delim "}"]]. *)

(** {2 Extracting substrings} *)

val string_before: string -> int -> string
(** [string_before s n] returns the substring of all characters of [s]
   that precede position [n] (excluding the character at
   position [n]). *)

val string_after: string -> int -> string
(** [string_after s n] returns the substring of all characters of [s]
    that follow position [n] (including the character at
    position [n]). *)

val first_chars: string -> int -> string
(** [first_chars s n] returns the first [n] characters of [s].
    This is the same function as [string_before]. *)

val last_chars: string -> int -> string
(** [last_chars s n] returns the last [n] characters of [s]. *)

end = struct
#1 "re_str.ml"
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  linking exception.                                                 *)
(*                                                                     *)
(***********************************************************************)

(* Modified by Jerome.Vouillon@pps.jussieu.fr for integration in RE *)

(* $Id: re_str.ml,v 1.3 2002/07/03 15:47:54 vouillon Exp $ *)

type regexp =
  { re : Re.t;
    mutable mtch : Re.re option;
    mutable srch : Re.re option }

let compile_regexp s c =
  { re = Re_emacs.re ~case:(not c) s;
    mtch = None;
    srch = None }

let rec get_mtch re =
  match re.mtch with
    Some r -> r
  | None   ->
    re.mtch <- Some (Re.compile (Re.seq [Re.start; re.re]));
    get_mtch re

let rec get_srch re =
  match re.srch with
    Some r -> r
  | None   ->
    re.srch <- Some (Re.compile re.re);
    get_srch re

let state = ref None

let string_match re s p =
  try
    state := Some (Re.exec ~pos:p (get_mtch re) s);
    true
  with Not_found ->
    state := None;
    false

let string_partial_match re s p =
  match
    Re.exec_partial ~pos:p (get_mtch re) s
  with
    `Full     -> string_match re s p
  | `Partial  -> true
  | `Mismatch -> false

let search_forward re s p =
  try
    let res = Re.exec ~pos:p (get_srch re) s in
    state := Some res;
    fst (Re.Group.offset res 0)
  with Not_found ->
    state := None;
    raise Not_found

let rec search_backward re s p =
  try
    let res = Re.exec ~pos:p (get_mtch re) s in
    state := Some res;
    p
  with Not_found ->
    state := None;
    if p = 0 then
      raise Not_found
    else
      search_backward re s (p - 1)

let valid_group n =
  n >= 0 && n < 10 && (
    match !state with
    | None -> false
    | Some m -> n < Re.Group.nb_groups m
  )

let beginning_group i =
  match !state with
    Some m -> fst (Re.Group.offset m i)
  | None   -> raise Not_found

let end_group i =
  match !state with
    Some m -> snd (Re.Group.offset m i)
  | None   -> raise Not_found

let get_len i =
  match !state with
    None   -> 0
  | Some m ->
    try
      let (b, e) = Re.Group.offset m i in
      e - b
    with Not_found ->
      0

let rec repl_length repl p q len =
  if p < len then begin
    if repl.[p] <> '\\' then
      repl_length repl (p + 1) (q + 1) len
    else begin
      let p = p + 1 in
      if p = len then failwith "Str.replace: illegal backslash sequence";
      let q =
        match repl.[p] with
        | '\\' -> q + 1
        | '0' .. '9' as c -> q + get_len (Char.code c - Char.code '0')
        | _ -> q + 2 in
      repl_length repl (p + 1) q len
    end
  end else
    q

let rec replace orig repl p res q len =
  if p < len then begin
    let c = repl.[p] in
    if c <> '\\' then begin
      Bytes.set res q c;
      replace orig repl (p + 1) res (q + 1) len
    end else begin
      match repl.[p + 1] with
        '\\' ->
        Bytes.set res q '\\';
        replace orig repl (p + 2) res (q + 1) len
      | '0' .. '9' as c ->
        let d =
          try
            match !state with
              None ->
              raise Not_found
            | Some m ->
              let (b, e) = Re.Group.offset m (Char.code c - Char.code '0') in
              let d = e - b in
              if d > 0 then String.blit orig b res q d;
              d
          with Not_found ->
            0
        in
        replace orig repl (p + 2) res (q + d) len
      | c ->
        Bytes.set res q '\\';
        Bytes.set res (q + 1) c;
        replace orig repl (p + 2) res (q + 2) len
    end
  end

let replacement_text repl orig =
  let len = String.length repl in
  let res = Bytes.create (repl_length repl 0 0 len) in
  replace orig repl 0 res 0 (String.length repl);
  Bytes.unsafe_to_string res

let quote s =
  let len = String.length s in
  let buf = Buffer.create (2 * len) in
  for i = 0 to len - 1 do
    match s.[i] with
      '[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$' as c ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf c
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let string_before s n = String.sub s 0 n

let string_after s n = String.sub s n (String.length s - n)

let first_chars s n = String.sub s 0 n

let last_chars s n = String.sub s (String.length s - n) n

let regexp e = compile_regexp e false

let regexp_case_fold e = compile_regexp e true

let regexp_string s = compile_regexp (quote s) false

let regexp_string_case_fold s = compile_regexp (quote s) true

let group_beginning n =
  if not (valid_group n) then invalid_arg "Str.group_beginning";
  let pos = beginning_group n in
  if pos = -1 then
    raise Not_found
  else
    pos

let group_end n =
  if not (valid_group n) then invalid_arg "Str.group_end";
  let pos = end_group n in
  if pos = -1 then
    raise Not_found
  else
    pos

let matched_group n txt =
  let b = group_beginning n and e = group_end n in String.sub txt b (e-b)

let replace_matched repl matched = replacement_text repl matched

let match_beginning () = group_beginning 0
and match_end () = group_end 0
and matched_string txt = matched_group 0 txt

let substitute_first expr repl_fun text =
  try
    let pos = search_forward expr text 0 in
    String.concat "" [string_before text pos;
                      repl_fun text;
                      string_after text (match_end ())]
  with Not_found ->
    text

let global_substitute expr repl_fun text =
  let rec replace start last_was_empty =
    try
      let startpos = if last_was_empty then start + 1 else start in
      if startpos > String.length text then raise Not_found;
      let pos = search_forward expr text startpos in
      let end_pos = match_end () in
      let repl_text = repl_fun text in
      String.sub text start (pos-start) ::
      repl_text ::
      replace end_pos (end_pos = pos)
    with Not_found ->
      [string_after text start] in
  String.concat "" (replace 0 false)

let global_replace expr repl text =
  global_substitute expr (replacement_text repl) text
and replace_first expr repl text =
  substitute_first expr (replacement_text repl) text

let search_forward_progress re s p =
  let pos = search_forward re s p in
  if match_end () > p then
    pos
  else if p < String.length s then
    search_forward re s (p + 1)
  else
    raise Not_found

let bounded_split expr text num =
  let start =
    if string_match expr text 0 then match_end () else 0 in
  let rec split start n =
    if start >= String.length text then
      []
    else if n = 1 then
      [string_after text start]
    else
      try
        let pos = search_forward_progress expr text start in
        String.sub text start (pos-start) :: split (match_end ()) (n - 1)
      with Not_found ->
        [string_after text start] in
  split start num

let split expr text = bounded_split expr text 0

let bounded_split_delim expr text num =
  let rec split start n =
    if start > String.length text then
      []
    else if n = 1 then
      [string_after text start]
    else
      try
        let pos = search_forward_progress expr text start in
        String.sub text start (pos-start) :: split (match_end ()) (n - 1)
      with Not_found ->
        [string_after text start] in
  if text = "" then [] else split 0 num

let split_delim expr text = bounded_split_delim expr text 0

type split_result = Text of string | Delim of string

let bounded_full_split expr text num =
  let rec split start n =
    if start >= String.length text then
      []
    else if n = 1 then
      [Text (string_after text start)]
    else
      try
        let pos = search_forward_progress expr text start in
        let s = matched_string text in
        if pos > start then
          Text (String.sub text start (pos - start)) ::
          Delim (s) ::
          split (match_end ()) (n - 1)
        else
          Delim (s) ::
          split (match_end ()) (n - 1)
      with Not_found ->
        [Text (string_after text start)] in
  split 0 num

let full_split expr text = bounded_full_split expr text 0

end
module OcamlRe
= struct
#1 "ocamlRe.ml"
module Re = Re

module Re_pcre = Re_pcre

module Re_glob = Re_glob

module Re_perl = Re_perl

module Re_posix = Re_posix

module Re_str = Re_str

module Re_emacs = Re_emacs

module Re_cset = Re_cset

module Re_automata = Re_automata

end
module Helpers
= struct
#1 "helpers.ml"
open OcamlRe

(* Batteries library substitutes *)
let listDrop n lst =
  let lst = ref lst in
  for i = 1 to n do lst := List.tl !lst done;
  !lst

let listDropWhile f lst =
  let lst = ref lst in
  while f (List.hd !lst) do lst := List.tl !lst done;
  !lst

let listTake n lst =
  let result = ref [] in
  let lst = ref lst in
  for i = 1 to n do
    result := (List.hd !lst) :: !result;
    lst := (List.tl !lst)
  done;
  List.rev !result

let listTakeWhile f lst =
  let result = ref [] in
  let lst = ref lst in
  while f (List.hd !lst) do
    result := (List.hd !lst) :: !result;
    lst := List.tl !lst
  done;
  List.rev !result

let optionGet a =
  match a with
  | Some n -> n
  | None -> raise (Invalid_argument "optionGet")

let optionMap f a =
  match a with
  | Some a' -> Some (f a')
  | None -> None

let listFilterMap f lst =
  List.map f lst
  |> List.filter (function | Some a -> true | None -> false)
  |> List.map optionGet

let listFindMap f lst =
  lst
  |> List.find (fun a -> match f a with | Some x -> true | None -> false)
  |> f
  |> optionGet

let stringSlice ?(first=0) ?last str =
  let last = match last with
  | Some l -> min l (String.length str)
  | None -> String.length str
  in
  if last <= first then ""
  else String.sub str first (last - first)

let stringFind str part =
  let rec find' str part idx =
    if String.length str < String.length part then raise Not_found
    else if stringSlice str ~last:(String.length part) = part then idx
    else find' (stringSlice str ~first:1) part (idx + 1)
  in
    find' str part 0

let stringNsplit str ~by =
  if String.length str = 0 then
    raise (Invalid_argument "stringNSplit: empty str not allowed")
  else if str = "" then
    []
  else
    let rec split' str ~by accum curr =
      let lengthBy = String.length by in
      let lengthStr = String.length str in
      if lengthStr < lengthBy then
        (curr ^ str) :: accum
      else
        if (String.sub str 0 lengthBy) = by then
          split'
            (String.sub str lengthBy (lengthStr - lengthBy))
            ~by
            (curr :: accum)
            ""
        else
          split'
            (String.sub str 1 (lengthStr - 1))
            ~by
            accum
            (curr ^ (String.sub str 0 1))
    in (split' str ~by [] "") |> List.rev

let stringSplit str ~by =
  if by = "" then
    ("", str)
  else if str = "" then
    raise Not_found
  else
    match stringNsplit str ~by with
    | [] | [_] -> raise Not_found
    | x :: xs -> (x, String.concat by xs)

let linesOfChannelExn chan =
  let lines = ref [] in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let fileLinesOfExn filePath = linesOfChannelExn (open_in filePath)


(* ============ *)

let get_match_n n pat str =
  let rex = Re_pcre.regexp pat in
  Re_pcre.get_substring (Re_pcre.exec ~rex str) n
(* get the first (presumably only) match in a string *)
let get_match = get_match_n 1

let get_match_maybe pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.get_substring (Re_pcre.exec ~rex str) 1)
  with Not_found -> None

let get_match_n_maybe n pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.get_substring (Re_pcre.exec ~rex str) n)
  with _ -> None

let execMaybe pat str =
  let rex = Re_pcre.regexp pat in
  try Some (Re_pcre.exec ~rex str)
  with Not_found -> None

let getSubstringMaybe result n =
  try Some (Re_pcre.get_substring result n)
  with Not_found -> None

let split sep str =
  let rex = Re_pcre.regexp sep in
  Re_pcre.split ~rex str

let rec splitInto ~chunckSize (l: 'a list): 'a list list =
  if List.length l <= chunckSize || chunckSize = 0 then [l]
  else (listTake chunckSize l) :: (splitInto ~chunckSize (listDrop chunckSize l))

let resetANSI = "\027[0m"
let red s = "\027[31m" ^ s ^ resetANSI
let redUnderlined s = "\027[31;4m" ^ s ^ resetANSI
let yellow s = "\027[33m" ^ s ^ resetANSI
let yellowUnderlined s = "\027[33;4m" ^ s ^ resetANSI
let green s = "\027[32m" ^ s ^ resetANSI
let cyan s = "\027[36m" ^ s ^ resetANSI

let mapcat sep f l = String.concat sep (List.map f l)

let sp = Printf.sprintf

let highlight ?(color=red) ?(first=0) ?(last=99999) str =
  (stringSlice ~last:first str)
    ^ (color @@ stringSlice ~first ~last str)
    ^ (stringSlice ~first:last str)

end
module BetterErrorsParseError
= struct
#1 "betterErrorsParseError.ml"
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

end
module ParseWarning
= struct
#1 "parseWarning.ml"
open BetterErrorsTypes
open Helpers

(* agnostic extractors, turning err string into proper data structures *)
(* TODO: don't make these raise error *)

let warning_UnusedVariable code err cachedContent range = raise Not_found

(* need: what the variant is. If it's e.g. a list, instead of saying "doesn't
cover all the cases of the variant" we could say "doesn't cover all the possible
length of the list" *)
let warning_PatternNotExhaustive code err _ _ =
  let unmatchedR = {|this pattern-matching is not exhaustive.\sHere is an example of a value that is not matched:\s([\s\S]+)|} in
  let unmatchedRaw = get_match unmatchedR err in
  let unmatched = if (String.get unmatchedRaw 0) = '(' then
    (* format was (Variant1|Variant2|Variant3). We strip the surrounding parens *)
    unmatchedRaw
    |> Helpers.stringSlice ~first:1 ~last:(String.length unmatchedRaw - 1)
    |> split {|\|[\s]*|}
  else
    [unmatchedRaw]
  in
  Warning_PatternNotExhaustive {
    unmatched = unmatched;
  }

let warning_PatternUnused code err cachedContent range = raise Not_found

(* need: offending optional argument name from AST *)
(* need: offending function name *)
let warning_OptionalArgumentNotErased code err cachedContent range =
  let ((startRow, startColumn), (endRow, endColumn)) = range in
  (* Hardcoding 16 for now. We might one day switch to use the variant from
  https://github.com/ocaml/ocaml/blob/901c67559469acc58935e1cc0ced253469a8c77a/utils/warnings.ml#L20 *)
  let allR = {|this optional argument cannot be erased\.|} in
  let fileLine = List.nth cachedContent startRow in
  let _ = get_match_n 0 allR err in
  let argumentNameRaw = Helpers.stringSlice
    ~first:startColumn
    ~last: (if startRow = endRow then endColumn else 99999)
    fileLine
  in
  let argumentNameR = {|(:?\?\s*\()?([^=]+)|} in
  let argumentName = get_match_n 2 argumentNameR argumentNameRaw in
  Warning_OptionalArgumentNotErased {
    argumentName = String.trim argumentName;
  }

(* TODO: better logic using these codes *)
let parsers = [
  warning_UnusedVariable;
  warning_PatternNotExhaustive;
  warning_PatternUnused;
  warning_OptionalArgumentNotErased;
]

let parse code warningBody cachedContent range =
  try
    Helpers.listFindMap (fun parse ->
      try Some (parse code warningBody cachedContent range)
      with _ -> None)
    parsers
  with Not_found -> Warning_CatchAll warningBody

end
module ReportError
= struct
#1 "reportError.ml"
open BetterErrorsTypes
open Helpers

let listify suggestions =
  suggestions
  |> List.map (fun sug -> "- `" ^ sug ^ "`")
  |> String.concat "\n"

let highlightPart ~color ~part str =
  let indexOfPartInStr = Helpers.stringFind str part in
  highlight ~color ~first:indexOfPartInStr ~last:(indexOfPartInStr + (String.length part)) str

let report parsedContent =
  match parsedContent with
  | Error_CatchAll error -> error
  | Type_MismatchTypeArguments {typeConstructor; expectedCount; actualCount} ->
    sp "This needs to be applied to %d argument(s), we found %d." expectedCount actualCount
  | Type_IncompatibleType {actual; expected; differingPortion; actualEquivalentType; expectedEquivalentType; extra} ->
    let (diffA, diffB) = differingPortion in
    (sp "The types don't match.\n%s %s\n%s  %s"
      (redUnderlined "This is:")
      (highlightPart ~color:red ~part:diffA actual)
      (green "Wanted:")
      (highlightPart ~color:green ~part:diffB expected))
    ^ (match extra with
      | Some e -> "\nExtra info: " ^ e
      | None -> "")
  | Type_NotAFunction {actual} ->
    "This is " ^ actual ^ ". You seem to have called it as a function.\n"
      ^ "Careful with spaces, semicolons, parentheses, and whatever in-between!"
  | Type_AppliedTooMany {functionType; expectedArgCount} ->
    sp
      "This function has type %s\nIt accepts only %d arguments. You gave more. Maybe you forgot a `;` somewhere?"
      functionType
      expectedArgCount
  | File_SyntaxError {offendingString; hint} ->
    (match hint with
    | Some a -> "The syntax is wrong: " ^ a
    | None -> "The syntax is wrong.")
    ^ "\n" ^
    (match offendingString with
    | ";" -> "Semicolon is an infix symbol used *between* expressions that return `unit` (aka \"nothing\").\n"
    | "else" -> "Did you happen to have put a semicolon on the line before else?"
      ^ " Also, `then` accepts a single expression. If you've put many, wrap them in parentheses.\n"
    | _ -> ""
    ) ^ "Note: the location indicated might not be accurate."
  | File_IllegalCharacter {character} ->
    sp "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!" character
  | Type_UnboundTypeConstructor {namespacedConstructor; suggestion} ->
    (sp "The type constructor %s can't be found." namespacedConstructor)
    ^
    (match suggestion with
    | None -> ""
    | Some h -> sp "\nHint: did you mean `%s`?" h)
  | Type_UnboundValue {unboundValue; suggestions} ->
    (match suggestions with
    | None -> sp "`%s` can't be found. Could it be a typo?" unboundValue
    | Some [hint] -> sp "`%s` can't be found. Did you mean `%s`?" unboundValue hint
    | Some [hint1; hint2] -> sp "`%s` can't be found. Did you mean `%s` or `%s`?" unboundValue hint1 hint2
    | Some hints -> sp "`%s` can't be found. Did you mean one of these?\n%s" unboundValue (listify hints))
  | Type_UnboundRecordField {recordField; suggestion} ->
    (match suggestion with
    | None -> sp "Field `%s` can't be found in any record type." recordField
    | Some hint -> sp "Field `%s` can't be found in any record type. Did you mean `%s`?" recordField hint)
  | Type_UnboundModule {unboundModule; suggestion} ->
    (sp "Module `%s` not found in included libraries.\n" unboundModule)
    ^
    (match suggestion with
    | Some s -> sp "Hint: did you mean `%s`?" s
    | None ->
      let pckName = String.lowercase unboundModule in
      "Hint: your build rules might be missing a link. If you're using: \n" ^
      " - Oasis: make sure you have `"^ pckName ^"` under `BuildDepends` in your _oasis file.\n" ^
      " - ocamlbuild: make sure you have `-pkgs "^ pckName ^"` in your build command.\n" ^
      " - ocamlc | ocamlopt: make sure you have `-I +"^ pckName ^"` in your build command before the source files.\n" ^
      " - ocamlfind: make sure you have `-package "^ pckName ^" -linkpkg` in your build command.")
  |  _ -> "huh"

end
module ReportWarning
= struct
#1 "reportWarning.ml"
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

end
module TerminalReporter
= struct
#1 "terminalReporter.ml"
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

end
module BetterErrorsMain
= struct
#1 "betterErrorsMain.ml"
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

end
module BetterErrorsShell
= struct
#1 "betterErrorsShell.ml"
let () = BetterErrorsMain.parseFromStdin ~customErrorParsers:[]

end
