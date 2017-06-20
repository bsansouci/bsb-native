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
  ?root_project_entry
  ~no_dev
  ~override_package_specs
  ~generate_watch_metadata
  ~root_project_dir
  cwd bsc_dir ocaml_dir ~forced 
  : _ option =
  let output_deps = cwd // Bsb_config.lib_bs // bsdeps in
  let reason : Bsb_dep_infos.check_result =
    Bsb_dep_infos.check ~cwd  forced output_deps in
  let () = 
    Format.fprintf Format.std_formatter  
      "@{<info>BSB check@} build spec : %a @." Bsb_dep_infos.pp_check_result reason in 
  begin match reason  with 
    | Good ->
      None  (* Fast path, no need regenerate ninja *)
    | Bsb_forced 
    | Bsb_bsc_version_mismatch 
    | Bsb_file_not_exist 
    | Bsb_source_directory_changed  
    | Other _ ->
      if reason = Bsb_bsc_version_mismatch then begin 
        print_endline "Also clean current repo due to we have detected a different compiler";
        clean_self (); 
      end ; 
      Bsb_build_util.mkp (cwd // Bsb_config.lib_bs);
      let config = 
        Bsb_config_parse.interpret_json 
          ~override_package_specs
          ~bsc_dir
          ~generate_watch_metadata
          ~no_dev
          cwd in 
      begin 
        Bsb_merlin_gen.merlin_file_gen ~cwd
          (bsc_dir // bsppx_exe) config;
        let external_deps_for_linking_and_clibs = match external_deps_for_linking_and_clibs with 
        | None -> 
          (* Either there's a `root_project_entry` (meaning we're currently building 
             an external dependency) or not, then we use the top level project's entry.
             
             If we're aiming at building JS, we do NOT walk the external dep graph.
             
             If we're aiming at building Native or Bytecode, we do walk the external 
             dep graph and build a topologically sorted list of all of them. *)
          let mainEntry = begin match root_project_entry with 
          (* `entries` should always contain at least on element. *)
          | None       -> List.hd config.Bsb_config_types.entries
          | Some entry -> entry
          end in
          begin match mainEntry with
          | Bsb_config_types.JsTarget _ -> ([], []) (* No work for the JS flow! *)
          | Bsb_config_types.BytecodeTarget _
          | Bsb_config_types.NativeTarget _ ->
            if root_project_entry <> None then 
              ([], [])
            else begin
              (* TODO(sansouci): Manually walk the external dep graph. Optimize this. *)
              let list_of_all_external_deps = ref [] in
              let all_clibs = ref [] in
              Bsb_build_util.walk_all_deps cwd
                (fun {top; cwd} ->
                  if not top then begin
                    let config = 
                      Bsb_config_parse.interpret_json 
                        ~override_package_specs
                        ~bsc_dir
                        ~generate_watch_metadata:false
                        ~no_dev:true
                        cwd in
                    begin match mainEntry with 
                    | Bsb_config_types.JsTarget _ ->  assert false
                    | Bsb_config_types.BytecodeTarget _ 
                      when List.mem Bsb_config_types.Bytecode Bsb_config_types.(config.allowed_build_kinds)-> 
                        list_of_all_external_deps := (cwd // Bsb_config.lib_ocaml) :: !list_of_all_external_deps;
                        all_clibs := (List.rev Bsb_config_types.(config.static_libraries)) @ !all_clibs;
                    | Bsb_config_types.NativeTarget _ 
                      when List.mem Bsb_config_types.Native Bsb_config_types.(config.allowed_build_kinds) -> 
                        list_of_all_external_deps := (cwd // Bsb_config.lib_ocaml) :: !list_of_all_external_deps;
                        all_clibs := (List.rev Bsb_config_types.(config.static_libraries)) @ !all_clibs;
                    | _ -> ()
                    end;
                  end
                );
              (List.rev !list_of_all_external_deps, List.rev !all_clibs)
            end
          end
        | Some (external_deps, clibs) -> (external_deps, clibs) in
        Bsb_gen.output_ninja ~external_deps_for_linking_and_clibs ~cwd ~bsc_dir ~ocaml_dir ~root_project_dir ?root_project_entry config;
        (* TODO(sansouci): output module alias file here *)
        Literals.bsconfig_json :: config.globbed_dirs
        |> List.map
          (fun x ->
             { Bsb_dep_infos.dir_or_file = x ;
               stamp = (Unix.stat (cwd // x)).st_mtime
             }
          )
        |> (fun x -> Bsb_dep_infos.store ~cwd output_deps (Array.of_list x));
        Some config 
      end 
  end

