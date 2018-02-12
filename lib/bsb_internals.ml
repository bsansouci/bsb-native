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
(* build_script.exe vendor_dir ocaml_lib cwd root_project_dir build_artifacts_dir -verbose *)

let _ = if Array.length Sys.argv != 6 && Array.length Sys.argv != 7 then begin
  print_endline "This binary is only for bsb's internal use";
  exit 1
end

let vendor_dir = Sys.argv.(1)
let ocaml_lib = Sys.argv.(2)
let cwd = Sys.argv.(3)
let root_project_dir = Sys.argv.(4)
let build_artifacts_dir = Sys.argv.(5)
let verbose = (7 = Array.length Sys.argv)

let ( // ) = Filename.concat

let mingw_dir = vendor_dir // "mingw32"
let node_modules = root_project_dir // "node_modules"

let _ = Sys.chdir cwd

let _ = if Ext_sys.is_windows_or_cygwin then
  Unix.putenv "Path" (mingw_dir ^ ";" ^ Unix.getenv("Path"))
 else ()

let checkInputTimestamps outfile_mtime includes srcs =
  let rec should_build = function 
    | [] -> false
    | file :: rest ->
      let path = if Filename.is_relative file then cwd // file else file in
      match (Unix.stat path) with
      | exception (Unix.Unix_error (Unix.ENOENT, _, _)) -> should_build rest
      | {Unix.st_mtime} -> 
        if st_mtime > outfile_mtime then 
          true
        else 
          should_build rest  
  in
  should_build srcs || should_build includes

(* @Hack We open a file here and let the OS clean it up when this program exists. *)
let oc = open_out (build_artifacts_dir // ".static_libraries")

let gcc ?c:(c=true) ?include_ocaml:(include_ocaml=true) ?flags:(flags=[]) ?includes:(includes=[]) ?should_link:(should_link=true) outfile srcs = 
  let output = if Filename.is_relative outfile then cwd // outfile else outfile in
  let should_build = match (Unix.(stat output)) with
    | exception (Unix.Unix_error (Unix.ENOENT, _, _)) -> true
    | {Unix.st_mtime} -> checkInputTimestamps st_mtime includes srcs
  in

  if should_link then begin
    output_string oc output; output_char oc '\n'
  end;

  if should_build then begin
    let dash_c = if c then "-c" else "" in
    let all_includes = if List.length includes > 0 then 
      "-I " ^ (String.concat " -I " (List.map (fun i -> 
        if Filename.is_relative i then cwd // i else i
      ) includes))
    else "" in
    let include_ocaml_flag = if include_ocaml then "-I " ^ ocaml_lib else "" in
    let compiler = if Ext_sys.is_windows_or_cygwin then "gcc.exe" else "gcc" in
    if Ext_sys.is_windows_or_cygwin then
      Sys.chdir mingw_dir;
    let cmd = compiler
      :: dash_c
      :: ("-o " ^ output)
      :: all_includes
      :: include_ocaml_flag
      :: (flags @ (List.map (fun s -> if Filename.is_relative s then cwd // s else s) srcs)) in
    
    if verbose then begin
      print_endline "Bsb_internal command:";
      print_endline ((String.concat " " cmd) ^ "\n");
    end;
    
    let err = Sys.command (String.concat " " cmd) in
    if Ext_sys.is_windows_or_cygwin then
      Sys.chdir cwd;
    if err != 0 then
      failwith ("gcc compilation failed for: " ^ outfile);
    err
  end else 0

end
