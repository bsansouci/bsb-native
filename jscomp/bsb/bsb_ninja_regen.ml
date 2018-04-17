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

let bsdeps = ".bsdeps"

let bsppx_exe = "bsppx.exe"

let (//) = Ext_path.combine

(** Regenerate ninja file by need based on [.bsdeps]
    return None if we dont need regenerate
    otherwise return Some info
*)
let regenerate_ninja
  ?dependency_info
  ~is_top_level
  ~not_dev
  ~forced
  ~root_project_dir
  ~build_library
  ~backend
  ~main_config:(main_config : Bsb_config_types.t)
  cwd bsc_dir ocaml_dir  =
  let build_artifacts_dir = Bsb_build_util.get_build_artifacts_location cwd in
  let output_deps = build_artifacts_dir // Bsb_config.lib_bs // bsdeps in
  let check_result  =
    Bsb_ninja_check.check 
      ~cwd:build_artifacts_dir
      ~forced ~file:output_deps backend build_library in
  let () = 
    Bsb_log.info
      "@{<info>BSB check@} build spec : %a @." Bsb_ninja_check.pp_check_result check_result in 
  begin match check_result  with 
    | Good -> false  (* Fast path, no need regenerate ninja *)
    | Bsb_forced 
    | Bsb_bsc_version_mismatch 
    | Bsb_file_not_exist 
    | Bsb_source_directory_changed  
    | Bsb_different_cmdline_arg
    | Other _ ->
      let nested = begin match backend with
        | Bsb_config_types.Js       -> "js"
        | Bsb_config_types.Native   -> "native"
        | Bsb_config_types.Bytecode -> "bytecode"
      end in
      if check_result = Bsb_bsc_version_mismatch then begin 
        Bsb_log.info "@{<info>Different compiler version@}: clean current repo";
        Bsb_clean.clean_self ~is_cmdline_build_kind_set:true ~nested bsc_dir build_artifacts_dir; 
      end ; 
      (* Generate the nested folder before anything else... *)
      Bsb_build_util.mkp (build_artifacts_dir // Bsb_config.lib_bs // nested);

      begin 
        Bsb_merlin_gen.merlin_file_gen ~cwd ~backend
          (bsc_dir // bsppx_exe) main_config;
        let dependency_info = match dependency_info with 
        | None -> 
          (* Either there's a `root_project_entry` (meaning we're currently building 
             an external dependency) or not, then we use the top level project's entry.
             
             If we're aiming at building JS, we do NOT walk the external dep graph.
             
             If we're aiming at building Native or Bytecode, we do walk the external 
             dep graph and build a topologically sorted list of all of them. *)
          begin match backend with
          | Bsb_config_types.Js -> 
            Bsb_dependency_info.{
              all_external_deps = [];
              all_ocamlfind_dependencies = [];
              all_ocaml_dependencies = Depend.StringSet.empty;
              all_clibs = [];
              all_c_linker_flags = [];
              all_toplevel_ppxes = String_map.empty;
            }
          | Bsb_config_types.Bytecode
          | Bsb_config_types.Native ->
            if not is_top_level then 
              Bsb_dependency_info.{
                all_external_deps = [];
                all_ocamlfind_dependencies = [];
                all_ocaml_dependencies = Depend.StringSet.empty;
                all_clibs = [];
                all_c_linker_flags = [];
                all_toplevel_ppxes = String_map.empty;
              }
            else begin
              (* @Speed Manually walk the external dep graph. Optimize this. 
                
                We can't really serialize that tree inside a file and avoid checking
                each dep one by one because that cache could go stale if one modifies
                something inside a dep directly. We could use such a cache for when
                the compiler's not invoked with `-make-world` but it's unclear if it's 
                worth doing.
                
                One way that might make this faster is to avoid parsing all of the bsconfig
                and just grab what we need. 
                
                Another way, which might be generally beneficial, is to generate
                a faster representation of bsconfig that's more liner. If we put the 
                most used information at the top and mashalled, it would be the "fastest"
                thing to do (that I can think of off the top of my head).
                
                   Ben - August 9th 2017
              *)
              let dependency_info : Bsb_dependency_info.t = {
                all_external_deps = [];
                all_ocamlfind_dependencies = [];
                all_ocaml_dependencies = Depend.StringSet.empty;
                all_clibs = [];
                all_c_linker_flags = [];
                all_toplevel_ppxes = String_map.empty;
              } in
              
              let all_ppxes = ref String_map.empty in
              
              Bsb_build_util.walk_all_deps cwd
                (fun {top; cwd} ->
                  if top then begin
                    dependency_info.all_toplevel_ppxes <- List.fold_left (fun all_toplevel_ppxes ({package_name} : Bsb_config_types.dependency) ->
                      match String_map.find_opt package_name !all_ppxes with
                      | None -> all_toplevel_ppxes
                      | Some v -> String_map.add package_name v all_toplevel_ppxes
                    ) dependency_info.all_toplevel_ppxes main_config.bs_dependencies;
                   end else begin 
                    let build_artifacts_dir = Bsb_build_util.get_build_artifacts_location cwd in
                    (* @Speed We don't need to read the full config, just the right fields.
                       Then again we also should cache this info so we don't have to crawl anything. *)
                    let inner_config = 
                      Bsb_config_parse.interpret_json 
                        ~override_package_specs:(Some main_config.package_specs)
                        ~bsc_dir
                        ~generate_watch_metadata:false
                        ~not_dev:true
                        ~backend
                        cwd in
                    all_ppxes := List.fold_left Bsb_config_types.(fun all_ppxes e -> 
                      match e with
                      | JsTarget {kind = Ppx}
                      | NativeTarget {kind = Ppx}
                      | BytecodeTarget {kind = Ppx} -> 
                          String_map.update inner_config.Bsb_config_types.package_name (function
                            | None -> Some [e]
                            | Some l -> Some (e :: l)
                          ) all_ppxes
                      | _ -> all_ppxes
                     ) !all_ppxes inner_config.Bsb_config_types.entries;
                    begin match backend with 
                    | Bsb_config_types.Js ->  assert false
                    | Bsb_config_types.Bytecode 
                      when List.mem Bsb_config_types.Bytecode Bsb_config_types.(inner_config.allowed_build_kinds) -> 
                        dependency_info.all_external_deps <- (build_artifacts_dir // Bsb_config.lib_ocaml // "bytecode") :: dependency_info.all_external_deps;
                        dependency_info.all_c_linker_flags <- (List.rev Bsb_config_types.(inner_config.c_linker_flags)) 
                          @ dependency_info.all_c_linker_flags;
                        dependency_info.all_clibs <- (List.rev Bsb_config_types.(inner_config.static_libraries)) 
                          @ dependency_info.all_clibs;
                        dependency_info.all_ocamlfind_dependencies <- Bsb_config_types.(inner_config.ocamlfind_dependencies) @ dependency_info.all_ocamlfind_dependencies;
                        dependency_info.all_ocaml_dependencies <- List.fold_left (fun acc v -> Depend.StringSet.add v acc) dependency_info.all_ocaml_dependencies Bsb_config_types.(inner_config.ocaml_dependencies);
                        
                    | Bsb_config_types.Native 
                      when List.mem Bsb_config_types.Native Bsb_config_types.(inner_config.allowed_build_kinds) -> 
                        dependency_info.all_external_deps <- (build_artifacts_dir // Bsb_config.lib_ocaml // "native") :: dependency_info.all_external_deps;
                        dependency_info.all_c_linker_flags <- (List.rev Bsb_config_types.(inner_config.c_linker_flags)) 
                          @ dependency_info.all_c_linker_flags;
                        dependency_info.all_clibs <- (List.rev Bsb_config_types.(inner_config.static_libraries)) 
                          @ dependency_info.all_clibs;
                        dependency_info.all_ocamlfind_dependencies <- Bsb_config_types.(inner_config.ocamlfind_dependencies) @ dependency_info.all_ocamlfind_dependencies;
                        dependency_info.all_ocaml_dependencies <- List.fold_left (fun acc v -> Depend.StringSet.add v acc) dependency_info.all_ocaml_dependencies Bsb_config_types.(inner_config.ocaml_dependencies);
                        
                    | _ -> ()
                    end;
                    if List.mem backend Bsb_config_types.(inner_config.allowed_build_kinds)
                       && Bsb_config_types.(inner_config.build_script) <> None then begin
                      let artifacts_installed = ref [] in
                      let filename = build_artifacts_dir // Literals.dot_static_libraries in
                      if not (Sys.file_exists filename) then 
                        ()
                        (* Bsb_exception.missing_static_libraries_file (Bsb_config_types.(inner_config.package_name))  *)
                      else begin
                        let ic = open_in_bin filename in
                        (try
                           while true do
                             artifacts_installed := (input_line ic) :: !artifacts_installed
                           done
                         with End_of_file -> ());
                        close_in ic;
                        dependency_info.all_clibs <- !artifacts_installed @ dependency_info.all_clibs;
                      end
                    end
                   end
                );
              dependency_info.all_external_deps <- List.rev dependency_info.all_external_deps;
              dependency_info.all_ocamlfind_dependencies <- List.rev dependency_info.all_ocamlfind_dependencies;
              dependency_info
            end
          end
        | Some all_deps -> all_deps in
        Bsb_ninja_gen.output_ninja_and_namespace_map 
          ~cwd 
          ~bsc_dir 
          ~not_dev
          ~dependency_info 
          ~ocaml_dir 
          ~root_project_dir 
          ~is_top_level 
          ~backend 
          ~main_bs_super_errors:main_config.bs_super_errors
          ~build_library
          main_config;
        (* PR2184: we still need record empty dir 
            since it may add files in the future *)  
        Bsb_ninja_check.record ~cwd ~file:output_deps 
          (Literals.bsconfig_json::main_config.globbed_dirs) 
          backend
          build_library;
        true
      end 
  end

