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


type dependency = 
  {
    package_name : string ; 
    package_install_path : string ; 
  }
type dependencies = dependency list 

type kind_t = Library | Ppx

type backend_t = 
    | JsTarget
    | NativeTarget
    | BytecodeTarget


type entries_t = {
    main_module_name: string;
    output_name: string option;
    kind: kind_t;
    backend: backend_t list
}

#if BS_NATIVE then
type compilation_kind_t = Js | Bytecode | Native
#end

type reason_react_jsx = string option 

type refmt = 
  | Refmt_none
  | Refmt_v3 
  | Refmt_custom of string 
type t = 
  {
    package_name : string ; 
    (* [captial-package] *)
    namespace : string option; 
    (* CapitalPackage *)
    external_includes : string list ; 
    bsc_flags : string list ;
    ppx_flags : string list ;
    bs_dependencies : dependencies;
    bs_dev_dependencies : dependencies;
    built_in_dependency : dependency option; 
    warning : Bsb_warning.t option;
    (*TODO: maybe we should always resolve bs-platform 
      so that we can calculate correct relative path in 
      [.merlin]
    *)
    refmt : refmt;
    refmt_flags : string list;
    js_post_build_cmd : string option;
    package_specs : Bsb_package_specs.t ; 
    globbed_dirs : string list;
    bs_file_groups : Bsb_file_groups.file_groups;
    files_to_install : String_hash_set.t ;
    generate_merlin : bool ; 
    reason_react_jsx : reason_react_jsx ; (* whether apply PPX transform or not*)
    entries : entries_t list ;
    generators : string String_map.t ; 
    cut_generators : bool; (* note when used as a dev mode, we will always ignore it *)
    bs_suffix : bool ; (* true means [.bs.js] we should pass [-bs-suffix] flag *)
    
#if BS_NATIVE then
    static_libraries: string list;
    c_linker_flags: string list;
    build_script: (string * bool) option;
    allowed_build_kinds: compilation_kind_t list;
    ocamlfind_dependencies: string list;
    ocaml_flags: string list;
    ocaml_linker_flags: string list;
    ocaml_dependencies: string list;
#end
  }
