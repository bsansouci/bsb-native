module Ext_sys : sig 
#1 "ext_sys.mli"
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


(* Not used yet *)
(* val is_directory_no_exn : string -> bool *)


val is_windows_or_cygwin : bool 
end = struct
#1 "ext_sys.ml"
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

(** TODO: not exported yet, wait for Windows Fix*)
let is_directory_no_exn f = 
  try Sys.is_directory f with _ -> false 


let is_windows_or_cygwin = Sys.win32 || Sys.cygwin
end
module Bsb_internals
= struct
#1 "bsb_internals.ml"
(* build_script.exe vendor_dir ocaml_lib cwd *)

let _ = if Array.length Sys.argv != 4 then begin
  print_endline "This binary is only for bsb's internal use";
  exit 1
end

let vendor_dir = Sys.argv.(1)
let ocaml_lib = Sys.argv.(2)
let cwd = Sys.argv.(3)

let ( // ) = Filename.concat

let mingw_dir = vendor_dir // "mingw32"

let _ = Sys.chdir cwd

let _ = if Ext_sys.is_windows_or_cygwin then
  let curdir = Sys.getcwd () in
  let newCurdir = curdir // mingw_dir in
  Unix.putenv "Path" (newCurdir ^ ";" ^ Unix.getenv("Path"));
 else ()

let gcc ?c:(c=true) ?include_ocaml:(include_ocaml=true) ?flags:(flags=[]) ?includes:(includes=[]) outfile srcs = 
  let dash_c = if c then "-c" else "" in
  let all_includes = if List.length includes > 0 then 
    "-I " ^ (String.concat " -I " (List.map (fun i -> cwd // i) includes)) 
  else "" in
  let include_ocaml_flag = if include_ocaml then "-I " ^ ocaml_lib else "" in
  let compiler = if Ext_sys.is_windows_or_cygwin then "gcc.exe" else "gcc" in
  if Ext_sys.is_windows_or_cygwin then
    Sys.chdir mingw_dir;
  let cmd = compiler
    :: dash_c
    :: ("-o " ^ cwd // outfile)
    :: all_includes
    :: include_ocaml_flag
    :: (flags @ (List.map (fun s -> cwd // s) srcs)) in
  let err = Sys.command (String.concat " " cmd) in
  if Ext_sys.is_windows_or_cygwin then
    Sys.chdir cwd;
  if err != 0 then
    failwith ("gcc compilation failed for: " ^ outfile);
  err

end
