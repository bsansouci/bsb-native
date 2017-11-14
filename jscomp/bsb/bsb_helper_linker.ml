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

type link_t = LinkBytecode of string | LinkNative of string

let (//) = Ext_path.combine

let link link_byte_or_native 
  ~main_module
  ~batch_files
  ~clibs
  ~includes
  ~ocamlfind_packages
  ~bs_super_errors
  ~namespace
  ~ocaml_dependencies
  ~warnings 
  ~warn_error
  cwd =
  let suffix_object_files, suffix_library_files, compiler, add_custom, output_file = begin match link_byte_or_native with
  | LinkBytecode output_file -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc"  , true, output_file
  | LinkNative output_file   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt", false, output_file
  end in
  (* Map used to track the path to the files as the dependency_graph that we're going to read from the mlast file only contains module names *)
  let module_to_filepath = List.fold_left
    (fun m v ->
      String_map.add
      (Ext_modulename.module_name_of_file_if_any v)
      (Ext_path.chop_extension_if_any v)
      m)
    String_map.empty
    batch_files in
  let dependency_graph = List.fold_left
    (fun m file ->
      String_map.add
        (Ext_modulename.module_name_of_file_if_any file)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file ((Ext_path.chop_extension file) ^ Literals.suffix_mlast))
        m)
    String_map.empty
    batch_files in
  let tasks = Bsb_helper_dep_graph.simple_collect_from_main dependency_graph main_module in
  let namespace = match namespace with 
    | None -> ""
    | Some namespace -> "-" ^ namespace
  in
  let list_of_object_files = Queue.fold
    (fun acc v -> match String_map.find_opt v module_to_filepath with
      | Some file -> (file ^ namespace ^ suffix_object_files) :: acc
      | None -> Bsb_exception.missing_object_file v
      )
    []
    tasks in
  if list_of_object_files <> [] then begin
    let library_files = List.fold_left
      (fun acc dir ->
        (Ext_path.combine dir (Literals.library_file ^ suffix_library_files)) :: acc)
      [] includes in
    (* This list will be reversed so we append the otherlibs object files at the end, and they'll end at the beginning. *)
    let ocaml_dir = Bsb_default_paths.ocaml_dir in
    
    let suffix = begin match link_byte_or_native with
      | LinkBytecode _ -> Literals.suffix_cma
      | LinkNative _   -> Literals.suffix_cmxa
    end in

    let ocaml_dependencies = List.fold_left (fun acc v -> 
      match v with
      | "compiler-libs" -> 
        ((ocaml_dir // "lib" // "ocaml" // "compiler-libs" // "ocamlcommon") ^ suffix) :: acc
      | "threads" -> 
        "-thread" :: ("threads" ^ suffix) :: acc
      | v -> (v ^ suffix) :: acc
    ) [] ocaml_dependencies in 

    (* let (otherlibs, extra_flags) = 
      let compiler_libs_requested = List.filter (fun v -> v = "compiler-libs") ocaml_flags in
      if List.length compiler_libs_requested = 1 then begin
        print_endline " compiler_libs_requested = 1 ";
        if ocamlfind_packages = [] then
          let ocamlcommon =  in 
          (ocamlcommon :: (List.filter (fun v -> v != "compiler-libs") ocaml_flags), ["-I"; "+compiler-libs"])
        else ("compiler-libs.common" :: List.filter (fun v -> v != "compiler-libs") ocaml_flags, [])
      end else begin 
        (ocaml_flags, []) 
      end in *)
      (* Yup, the library is called nums.cma but the module is called Num, and when depending on it
         through ocamlfind you need to use the name Num. *)
    (* let otherlibs = if ocamlfind_packages = [] then 
      (List.map (fun v -> if v = "num" then v ^ "s" ^ suffix else v ^ suffix) otherlibs) 
    else 
      (List.fold_left (fun acc v -> "-package" :: v :: acc) [] otherlibs) in *)
    
    (* Bsb_helper_dep_graph.get_otherlibs_dependencies ~ocamlfind:(ocamlfind_packages <> []) dependency_graph suffix_library_files in *)
    let clibs = if add_custom && clibs <> [] then
      "-custom" :: clibs
    else
      clibs
    in
    let warning_command = if String.length warnings > 0 then
      "-w" :: warnings :: []
    else [] in 
    let warning_command = if String.length warn_error > 0 then
      "-warn-error" :: warn_error :: warning_command
    else warning_command in 
    
    let all_object_files = ocaml_dependencies @ clibs @ library_files @ List.rev (list_of_object_files) in
    (* If there are no ocamlfind packages then let's not use ocamlfind, let's use the opt compiler instead.
       This is for mainly because we'd like to offer a "sandboxed" experience for those who want it.
       So if you don't care about opam dependencies you can solely rely on Bucklescript and npm, no need 
       to install ocamlfind. 
     *)
    if ocamlfind_packages = [] then
      let compiler = ocaml_dir // compiler ^ ".opt" in
      let list_of_args = (compiler :: "-g"
        :: (if bs_super_errors then ["-bs-super-errors"] else [])) 
        @ warning_command
        @ "-o" :: output_file :: all_object_files in
        (* List.iter (fun a -> print_endline a) list_of_args; *)
      Unix.execvp
        compiler
        (Array.of_list (list_of_args))
    else begin
      (* @CrossPlatform This might work on windows since we're using the Unix module which claims to
         have a windows implementation... We should double check this. *)
      let list_of_args = ("ocamlfind" :: compiler :: []) 
        @ (if bs_super_errors then ["-passopt"; "-bs-super-errors"] else []) 
        @ ("-linkpkg" :: ocamlfind_packages)
        @ warning_command
        @ ("-g" :: "-o" :: output_file :: all_object_files) in
      (* List.iter (fun a -> print_endline a) list_of_args; *)
      Unix.execvp
        "ocamlfind"
        (Array.of_list (list_of_args))
    end
  end else
    Bsb_exception.no_files_to_link suffix_object_files main_module
