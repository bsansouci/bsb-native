(* Copyright (C) 2017 Authors of BuckleScript
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


let (//) = Ext_path.combine


#if BS_NATIVE then
let ninja_clean ~nested bsc_dir proj_dir =
#else
let ninja_clean bsc_dir proj_dir =
#end
  try
    let cmd = bsc_dir // "ninja.exe" in
#if BS_NATIVE then
    let cwd = proj_dir // nested // Bsb_config.lib_bs in
#else
    let cwd = proj_dir // Bsb_config.lib_bs in
#end
    if Sys.file_exists cwd then
      let eid =
        Bsb_unix.run_command_execv {cmd ; args = [|cmd; "-t"; "clean"|] ; cwd} in
      if eid <> 0 then
        Bsb_log.warn "@{<warning>ninja clean failed@}@."
  with  e ->
    Bsb_log.warn "@{<warning>ninja clean failed@} : %s @." (Printexc.to_string e)

#if BS_NATIVE then
let clean_bs_garbage ~nested bsc_dir proj_dir =
#else
let clean_bs_garbage bsc_dir proj_dir =
#end
  Bsb_log.info "@{<info>Cleaning:@} in %s@." proj_dir ;
  let try_remove x =
    let x = proj_dir // x in
    if Sys.file_exists x then
      Bsb_unix.remove_dir_recursive x  in
  try
    Bsb_parse_sources.clean_re_js proj_dir; (* clean re.js files*)
#if BS_NATIVE then
    ninja_clean ~nested bsc_dir proj_dir ;
#else
    ninja_clean bsc_dir proj_dir ;
#end
    List.iter try_remove Bsb_config.all_lib_artifacts;
  with
    e ->
    Bsb_log.warn "@{<warning>Failed@} to clean due to %s" (Printexc.to_string e)


#if BS_NATIVE then
let clean_bs_deps ~is_cmdline_build_kind_set ~nested bsc_dir proj_dir =
  if is_cmdline_build_kind_set then
    Bsb_build_util.walk_all_deps  proj_dir  (fun pkg_cxt ->
        (* whether top or not always do the cleaning *)
        clean_bs_garbage ~nested bsc_dir (Bsb_build_util.get_build_artifacts_location pkg_cxt.cwd)
      )
  else begin
   Bsb_build_util.walk_all_deps  proj_dir  (fun { cwd} ->
      let build_artifacts_cwd = Bsb_build_util.get_build_artifacts_location cwd in
      (* whether top or not always do the cleaning *)
      clean_bs_garbage ~nested:"js" bsc_dir build_artifacts_cwd;
      clean_bs_garbage ~nested:"bytecode" bsc_dir build_artifacts_cwd;
      clean_bs_garbage ~nested:"native" bsc_dir build_artifacts_cwd;
    )
end
#else
let clean_bs_deps bsc_dir proj_dir =
  Bsb_build_util.walk_all_deps  proj_dir  (fun pkg_cxt ->
      (* whether top or not always do the cleaning *)
      clean_bs_garbage bsc_dir pkg_cxt.cwd
    )
#end

#if BS_NATIVE then
let clean_self ~is_cmdline_build_kind_set ~nested bsc_dir proj_dir =
  let build_artifacts_cwd = Bsb_build_util.get_build_artifacts_location proj_dir in
  if is_cmdline_build_kind_set then
    clean_bs_garbage ~nested bsc_dir build_artifacts_cwd
  else begin
    clean_bs_garbage ~nested:"js" bsc_dir build_artifacts_cwd;
    clean_bs_garbage ~nested:"bytecode" bsc_dir build_artifacts_cwd;
    clean_bs_garbage ~nested:"native" bsc_dir build_artifacts_cwd;
  end
#else
let clean_self bsc_dir proj_dir = clean_bs_garbage bsc_dir proj_dir
#end
