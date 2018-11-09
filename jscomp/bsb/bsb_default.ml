(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


(* for default warning flags, please see bsb_warning.ml *)
let bsc_flags =
  [
    "-color"; "always"
  ] 


let refmt_flags = ["--print"; "binary"]

let refmt_v3 = "refmt.exe"
let refmt_none = "refmt.exe"

let main_entries = [{ Bsb_config_types.kind = Library; main_module_name="Index"; output_name=None; backend = [JsTarget]}]

#if BS_NATIVE then
let ocaml_flags = ["-no-alias-deps"; "-bin-annot";]

let ocaml_linker_flags = []

let allowed_build_kinds = [Bsb_config_types.Js; Bsb_config_types.Bytecode; Bsb_config_types.Native]

let ocaml_dependencies = ["unix"; "bigarray"; "str"; "dynlink"; "nums"; "threads"; ]
#end
