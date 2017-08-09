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

let (//) = Ext_filename.combine

(** Regenerate ninja file by need based on [.bsdeps]
    return None if we dont need regenerate
    otherwise return Some info
*)
let regenerate_ninja
  ?external_deps_for_linking_and_clibs
  ~is_top_level
  ~no_dev
  ~override_package_specs
  ~generate_watch_metadata
  ~forced
  ~root_project_dir
  ~cmdline_build_kind
  cwd bsc_dir ocaml_dir 
  : _ option =
  let output_deps = cwd // Bsb_config.lib_bs // bsdeps in
  let reason : Bsb_bsdeps.check_result =
    Bsb_bsdeps.check ~cwd  ~forced ~file:output_deps cmdline_build_kind in
  let () = 
    Format.fprintf Format.std_formatter  
      "@{<info>BSB check@} build spec : %a @." Bsb_bsdeps.pp_check_result reason in 
  begin match reason  with 
    | Good ->
      None  (* Fast path, no need regenerate ninja *)
    | Bsb_forced 
    | Bsb_bsc_version_mismatch 
    | Bsb_file_not_exist 
    | Bsb_source_directory_changed  
    | Bsb_different_cmdline_arg
    | Other _ ->
      if reason = Bsb_bsc_version_mismatch then begin 
        print_endline "Also clean current repo due to we have detected a different compiler";
        Bsb_clean.clean_self bsc_dir cwd; 
      end ; 
      Bsb_build_util.mkp (cwd // Bsb_config.lib_bs); 
      let config = 
        Bsb_config_parse.interpret_json 
          ~override_package_specs
          ~bsc_dir
          ~generate_watch_metadata
          ~no_dev
          ~compilation_kind:cmdline_build_kind
          cwd in 
      let nested = begin match cmdline_build_kind with
        | Bsb_config_types.Js -> "js"
        | Bsb_config_types.Bytecode -> "bytecode"
        | Bsb_config_types.Native -> "native"
      end in
      (* Generate the nested folder before anything else... *)
      Bsb_build_util.mkp (cwd // Bsb_config.lib_bs // nested);
      begin 
        Bsb_merlin_gen.merlin_file_gen ~cwd
          (bsc_dir // bsppx_exe) config;
        (match config.namespace with 
        | None -> ()
        | Some namespace -> 
          (* generate a map file
            TODO: adapt ninja rules
          *)
          Bsb_pkg_map_gen.output ~cwd namespace config.bs_file_groups
        );
        let external_deps_for_linking_and_clibs = match external_deps_for_linking_and_clibs with 
        | None -> 
          (* Either there's a `root_project_entry` (meaning we're currently building 
             an external dependency) or not, then we use the top level project's entry.
             
             If we're aiming at building JS, we do NOT walk the external dep graph.
             
             If we're aiming at building Native or Bytecode, we do walk the external 
             dep graph and build a topologically sorted list of all of them. *)
          begin match cmdline_build_kind with
          | Bsb_config_types.Js -> ([], []) (* No work for the JS flow! *)
          | Bsb_config_types.Bytecode
          | Bsb_config_types.Native ->
            if not is_top_level then 
              ([], [])
            else begin
              (* TODO(sansouci): Manually walk the external dep graph. Optimize this. *)
              let list_of_all_external_deps = ref [] in
              let all_clibs = ref [] in
              Bsb_build_util.walk_all_deps cwd
                (fun {top; cwd} ->
                  if not top then begin
                    (* TODO(sansouci): We don't need to read the full config, just the right fields.
                       Then again we also should cache this info so we don't have to crawl anything. *)
                    let innerConfig = 
                      Bsb_config_parse.interpret_json 
                        ~override_package_specs
                        ~bsc_dir
                        ~generate_watch_metadata:false
                        ~no_dev:true
                        ~compilation_kind:cmdline_build_kind
                        cwd in
                    begin match cmdline_build_kind with 
                    | Bsb_config_types.Js ->  assert false
                    | Bsb_config_types.Bytecode 
                      when List.mem Bsb_config_types.Bytecode Bsb_config_types.(innerConfig.allowed_build_kinds)-> 
                        list_of_all_external_deps := (cwd // Bsb_config.lib_ocaml // "bytecode") :: !list_of_all_external_deps;
                        all_clibs := (List.rev Bsb_config_types.(innerConfig.static_libraries)) @ !all_clibs;
                    | Bsb_config_types.Native 
                      when List.mem Bsb_config_types.Native Bsb_config_types.(innerConfig.allowed_build_kinds) -> 
                        list_of_all_external_deps := (cwd // Bsb_config.lib_ocaml // "native") :: !list_of_all_external_deps;
                        all_clibs := (List.rev Bsb_config_types.(innerConfig.static_libraries)) @ !all_clibs;
                    | _ -> ()
                    end;
                  end
                );
              (List.rev !list_of_all_external_deps, List.rev !all_clibs)
            end
          end
        | Some (external_deps, clibs) -> (external_deps, clibs) in
        Bsb_ninja_gen.output_ninja ~external_deps_for_linking_and_clibs ~cwd ~bsc_dir ~ocaml_dir ~root_project_dir ~is_top_level ~cmdline_build_kind:cmdline_build_kind config;
        Literals.bsconfig_json :: config.globbed_dirs
        |> List.map
          (fun x ->
             { Bsb_bsdeps.dir_or_file = x ;
               stamp = (Unix.stat (cwd // x)).st_mtime
             }
          )
        |> (fun x -> Bsb_bsdeps.store ~cwd ~file:output_deps (Array.of_list x) cmdline_build_kind);
        Some config 
      end 
  end

