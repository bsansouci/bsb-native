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

type dep_info = {
  dir_or_file : string ;
  st_mtime : float
}

type t =
  { file_stamps : dep_info array ;
    source_directory :  string ;
    bsb_version : string;
    bsc_version : string;
#if BS_NATIVE then
    cmdline_backend: Bsb_config_types.compilation_kind_t;
    cmdline_build_library: string option;
#end
  }


let magic_number = "BS_DEP_INFOS_20170822"
let bsb_version = "20170822+dev"
(* TODO: for such small data structure, maybe text format is better *)

let write (fname : string)  (x : t) =
  let oc = open_out_bin fname in
  output_string oc magic_number ;
  output_value oc x ;
  close_out oc





type check_result =
  | Good
  | Bsb_file_not_exist (** We assume that it is a clean repo *)
  | Bsb_source_directory_changed
  | Bsb_bsc_version_mismatch
  | Bsb_forced
#if BS_NATIVE then
  | Bsb_different_cmdline_arg
#end
  | Other of string

let pp_check_result fmt (check_resoult : check_result) =
  Format.pp_print_string fmt (match check_resoult with
      | Good -> "OK"
      | Bsb_file_not_exist -> "Dependencies information missing"
      | Bsb_source_directory_changed ->
        "Bsb source directory changed"
      | Bsb_bsc_version_mismatch ->
        "Bsc or bsb version mismatch"
      | Bsb_forced ->
        "Bsb forced rebuild"
#if BS_NATIVE then
      | Bsb_different_cmdline_arg ->
        "Bsb called with a different build argument"
#end
      | Other s -> s)

let rec check_aux cwd xs i finish =
  if i = finish then Good
  else
    let k = Array.unsafe_get  xs i  in
    let current_file = k.dir_or_file in
    let stat = Unix.stat  (Filename.concat cwd  current_file) in
    if stat.st_mtime <= k.st_mtime then
      check_aux cwd xs (i + 1 ) finish
    else Other current_file


let read (fname : string) cont =
  match open_in_bin fname with   (* Windows binary mode*)
  | ic ->
    let buffer = really_input_string ic (String.length magic_number) in
    if (buffer <> magic_number) then Bsb_bsc_version_mismatch
    else
      let res : t = input_value ic  in
      close_in ic ;
      cont res
  | exception _ -> Bsb_file_not_exist

#if BS_NATIVE then
let record ~cwd ~file cmdline_backend cmdline_build_library file_or_dirs =
#else
let record ~cwd ~file  file_or_dirs =
#end
  let file_stamps = 
    Ext_array.of_list_map file_or_dirs
      (fun  x -> 
         {dir_or_file = x ;
          st_mtime = (Unix.stat (Filename.concat cwd  x )).st_mtime
         })
  in 
  write file
    { file_stamps ;
      source_directory = cwd ;
      bsb_version ;
#if BS_NATIVE then
      cmdline_backend = cmdline_backend;
      cmdline_build_library = cmdline_build_library; 
#end
      bsc_version = Bs_version.version }

(** check time stamp for all files
    TODO: those checks system call can be saved later
    Return a reason
    Even forced, we still need walk through a little
    bit in case we found a different version of compiler
*)
#if BS_NATIVE then
let check ~cwd ~forced ~file cmdline_backend cmdline_build_library : check_result =
#else
let check ~cwd ~forced ~file : check_result =
#end
  read file  begin  function  {
    file_stamps = xs; source_directory; bsb_version = old_version;
#if BS_NATIVE then
    cmdline_backend = old_cmdline_backend; 
    cmdline_build_library = old_cmdline_build_library;
#end
    bsc_version
  } ->
    if old_version <> bsb_version then Bsb_bsc_version_mismatch else
    if cwd <> source_directory then Bsb_source_directory_changed else
    if bsc_version <> Bs_version.version then Bsb_bsc_version_mismatch else
    if forced then Bsb_forced (* No need walk through *)
#if BS_NATIVE then
    else if cmdline_backend <> old_cmdline_backend
      || cmdline_build_library <> old_cmdline_build_library then Bsb_different_cmdline_arg
#end
    else
      try
        check_aux cwd xs  0 (Array.length xs)
      with e ->
        begin
          Bsb_log.info
            "@{<info>Stat miss %s@}@."
            (Printexc.to_string e);
          Bsb_file_not_exist
        end
  end
