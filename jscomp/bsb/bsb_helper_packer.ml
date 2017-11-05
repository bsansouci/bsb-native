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

type pack_t = PackBytecode | PackNative

let (//) = Ext_path.combine

(* Ok this is a bit weird but the packer is called with object files (.cmo / .cmx) which will be namespaced
   and we're using those names to read-in the mlast files which are not namespaced. So we strip the 
   namespace before reading them in. *)
let module_of_filename filename = 
  let str = Ext_path.chop_extension filename in
  match (String.rindex str '-') with 
  | exception Not_found -> str
  | len -> String.sub str 0 len

let pack pack_byte_or_native ~batch_files ~includes ~ocamlfind_packages ~bs_super_errors ~namespace cwd =
  let suffix_object_files, suffix_library_files, compiler, custom_flag = begin match pack_byte_or_native with
  | PackBytecode -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc", true
  | PackNative   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt", false
  end in
  let module_to_filepath = List.fold_left
    (fun m v ->
      String_map.add
      (Ext_modulename.module_name_of_file_if_any (module_of_filename v))
      (Ext_path.chop_extension_if_any v)
      m)
    String_map.empty
    batch_files in
  let dependency_graph = List.fold_left
    (fun m file ->
      String_map.add
        (Ext_modulename.module_name_of_file_if_any (module_of_filename file))
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file ((module_of_filename file) ^ Literals.suffix_mlast))
        m)
    String_map.empty
    batch_files in
  let domain =
    String_map.fold
      (fun k _ acc -> String_set.add k acc)
      dependency_graph String_set.empty in
  let sorted_tasks = Bsb_helper_dep_graph.sort_files_by_dependencies ~domain dependency_graph in
  let all_object_files = List.rev (Queue.fold
    (fun acc v -> match String_map.find_opt v module_to_filepath with
      | Some file -> (file ^ suffix_object_files) :: acc
      | None -> failwith @@ "build.ninja is missing the file '" ^ v ^ "' that was used in the project. Try force-regenerating but this shouldn't happen."
      )
    []
    sorted_tasks) in
  let all_object_files = match namespace with
    | None -> all_object_files
    | Some namespace -> (namespace ^ suffix_object_files) :: all_object_files 
  in
  if all_object_files <> [] then
    let includes = List.fold_left (fun acc dir -> "-I" :: dir :: acc) [] includes in
    (* If there are no ocamlfind packages then let's not use ocamlfind, let's use the opt compiler instead.
       This is for mainly because we'd like to offer a "sandboxed" experience for those who want it.
       So if you don't care about opam dependencies you can solely rely on Bucklescript and npm, no need 
       to install ocamlfind. *)
    if ocamlfind_packages = [] then
      let ocaml_dir = Bsb_default_paths.ocaml_dir in
      let compiler = ocaml_dir // compiler ^ ".opt" in
      Unix.execvp
        compiler
          (Array.of_list ((compiler :: "-a" :: "-g" :: (if bs_super_errors then ["-bs-super-errors"] else []) )
            @ "-o" :: (Literals.library_file ^ suffix_library_files) :: includes @ all_object_files))
    else begin
      (* @CrossPlatform This might work on windows since we're using the Unix module which claims to
         have a windows implementation... We should double check this. *)
      (* @Hack we assume we're inside lib/bs/nested here, this might change in the future, breaking this 
            Ben - October 6th 2017
      *)
      let dir = Filename.dirname @@ Filename.dirname @@ Filename.dirname @@ cwd in
      let list_of_args = ("ocamlfind" :: compiler :: "-a" :: "-g" :: ocamlfind_packages) 
      @ ((if bs_super_errors then ["-passopt"; "-bs-super-errors"] else []))
      @  ("-o" :: (Literals.library_file ^ suffix_library_files) :: includes @ all_object_files) in
      Unix.execvp
        "ocamlfind"
          (Array.of_list list_of_args)
    end
  else
    failwith @@ "No " ^ suffix_object_files ^ " to pack into a lib."
