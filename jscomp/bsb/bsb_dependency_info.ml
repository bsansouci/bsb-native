type path = string

type t = {
  mutable all_external_deps: path list;
  mutable all_ocamlfind_dependencies: string list;
  mutable all_ocaml_dependencies: Depend.StringSet.t;
  mutable all_clibs: path list;
  mutable all_c_linker_flags: string list;
  mutable all_toplevel_ppxes: (Bsb_config_types.entries_t list) String_map.t;
}

let (//) = Ext_path.combine

let check_if_dep ~root_project_dir ~backend (dependency_info : t) flag_exec =
  let components = Ext_string.split_by (fun c -> c = Ext_path.sep_char) flag_exec in
  match components with 
  | [dep_name; entry_name] -> begin 
    match String_map.find_opt dep_name dependency_info.all_toplevel_ppxes with
    | None -> 
      Ext_pervasives.failwithf ~loc:__LOC__ "No package named '%s' with ppx '%s'. Check the `entries` field in bsconfig.json." dep_name entry_name
    | Some l -> begin 
      match List.filter Bsb_config_types.(function 
        | JsTarget {main_module_name} 
        | NativeTarget {main_module_name} 
        | BytecodeTarget {main_module_name} -> main_module_name = entry_name) l with 
      | [] -> Ext_pervasives.failwithf ~loc:__LOC__ "Couldn't find ppx called '%s' under dep '%s'" entry_name dep_name
      | head :: _ -> 
        let nested = begin match backend with
          | Bsb_config_types.Js       -> "js"
          | Bsb_config_types.Native   -> "native"
          | Bsb_config_types.Bytecode -> "bytecode"
        end in 
        let extension = begin match head with
          | Bsb_config_types.JsTarget _       -> assert false
          | Bsb_config_types.NativeTarget _   -> ".native"
          | Bsb_config_types.BytecodeTarget _ -> ".byte"
        end in
        root_project_dir // Literals.node_modules // dep_name // Bsb_config.lib_bs // nested // entry_name ^ extension
      end
    end
  | _ -> flag_exec
