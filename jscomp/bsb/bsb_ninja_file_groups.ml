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

type info =
  string list   
(* Figure out a list of files 
   to be built before building cm*
*)


let zero : info =
  [] 





let handle_generators oc 
    (group : Bsb_parse_sources.file_group) custom_rules =   
  let map_to_source_dir = 
    (fun x -> Bsb_config.proj_rel (group.dir //x )) in
  group.generators
  |> List.iter (fun  ({output; input; command}  : Bsb_parse_sources.build_generator)-> 
      begin match String_map.find_opt command custom_rules with 
        | None -> Ext_pervasives.failwithf ~loc:__LOC__ "custom rule %s used but  not defined" command
        | Some rule -> 
          begin match output, input with
            | output::outputs, input::inputs -> 
              Bsb_ninja_util.output_build oc 
                ~outputs:(Ext_list.map map_to_source_dir  outputs)
                ~inputs:(Ext_list.map map_to_source_dir inputs) 
                ~output:(map_to_source_dir output)
                ~input:(map_to_source_dir input)
                ~rule
            | [], _ 
            | _, []  -> Ext_pervasives.failwithf ~loc:__LOC__ "either output or input can not be empty in rule %s" command
          end
      end
    )


let make_common_shadows 
    is_re
    package_specs 
    dirname 
    dir_index 
  : Bsb_ninja_util.shadow list 
  =
  let shadows : Bsb_ninja_util.shadow list = 
    { key = Bsb_ninja_global_vars.bs_package_flags;
      op = 
        Append
          (Bsb_package_specs.package_flag_of_package_specs
             package_specs dirname
          )
    } ::
    (if Bsb_dir_index.is_lib_dir dir_index  then [] else
       [{
         key = Bsb_ninja_global_vars.bs_package_includes; 
         op = AppendVar Bsb_ninja_global_vars.bs_package_dev_includes 
       }
        ;
        { key = "bsc_extra_includes";
          op = OverwriteVar (Bsb_dir_index.string_of_bsb_dev_include dir_index)
        }
       ]
    )   
  in 
  if is_re then 
    { key = Bsb_ninja_global_vars.bsc_flags; 
      op = AppendList ["-bs-re-out"; "-bs-super-errors"]
    } :: shadows
  else shadows


let emit_impl_build
    (package_specs : Bsb_package_specs.t)
    (group_dir_index : Bsb_dir_index.t) 
    oc 
    ~bs_suffix
    ~no_intf_file:(no_intf_file : bool) 
    js_post_build_cmd
    ~is_re
    ~local_ppx_flags
    ~local_ppx_deps
    namespace
    filename_sans_extension
  : info =    
  let input = 
    Bsb_config.proj_rel 
      (if is_re then filename_sans_extension ^ Literals.suffix_re 
       else filename_sans_extension ^ Literals.suffix_ml  ) in
  let output_mlast = filename_sans_extension  ^ Literals.suffix_mlast in
  let output_mlastd = filename_sans_extension ^ Literals.suffix_mlastd in
  let output_filename_sans_extension = 
    match namespace with 
    | None -> 
      filename_sans_extension 
    | Some ns -> 
      Ext_namespace.make ~ns filename_sans_extension
  in 
  let file_cmi =  output_filename_sans_extension ^ Literals.suffix_cmi in
  let output_cmj =  output_filename_sans_extension ^ Literals.suffix_cmj in
  let output_js =
    Bsb_package_specs.get_list_of_output_js package_specs bs_suffix output_filename_sans_extension in 
  let common_shadows = 
    make_common_shadows is_re package_specs
      (Filename.dirname file_cmi)
      group_dir_index in
  let shadows = List.map (fun f -> 
    Bsb_ninja_util.{
     key = "ppx_flags"; 
     op = AppendList ["-ppx"; f]
   }) local_ppx_flags in
  begin
    Bsb_ninja_util.output_build oc
      ~output:output_mlast
      ~input
      ~rule:( if is_re then 
                Bsb_rule.build_ast_and_module_sets_from_re
              else
                Bsb_rule.build_ast_and_module_sets)
      ~implicit_deps:local_ppx_deps
      ~shadows;
    Bsb_ninja_util.output_build
      oc
      ~output:output_mlastd
      ~input:output_mlast
      ~rule:Bsb_rule.build_bin_deps
      ?shadows:(if Bsb_dir_index.is_lib_dir group_dir_index then None
                else Some [{Bsb_ninja_util.key = Bsb_build_schemas.bsb_dir_group ; 
                            op = 
                              Overwrite (string_of_int (group_dir_index :> int)) }])
    ;
    let shadows =
      match js_post_build_cmd with
      | None -> common_shadows
      | Some cmd ->
        {key = Bsb_ninja_global_vars.postbuild;
         op = Overwrite ("&& " ^ cmd ^ Ext_string.single_space ^ String.concat Ext_string.single_space output_js)} 
        :: common_shadows
    in
    let rule , cm_outputs, deps =
      if no_intf_file then 
        Bsb_rule.build_cmj_cmi_js, [file_cmi], []
      else  Bsb_rule.build_cmj_js, []  , [file_cmi]
    in
    Bsb_ninja_util.output_build oc
      ~output:output_cmj
      ~shadows
      ~implicit_outputs:  (output_js @ cm_outputs)
      ~input:output_mlast
      ~implicit_deps:deps
      ~rule;
    [output_mlastd] 
  end 


let emit_intf_build 
    (package_specs : Bsb_package_specs.t)
    (group_dir_index : Bsb_dir_index.t)
    oc
    ~is_re
    ~local_ppx_flags
    ~local_ppx_deps
    namespace
    filename_sans_extension
  : info =
  let output_mliast = filename_sans_extension ^ Literals.suffix_mliast in
  let output_mliastd = filename_sans_extension ^ Literals.suffix_mliastd in
  let output_filename_sans_extension = 
    match namespace with 
    | None -> 
      filename_sans_extension 
    | Some ns -> 
      Ext_namespace.make ~ns filename_sans_extension
  in 
  let output_cmi = output_filename_sans_extension ^ Literals.suffix_cmi in  
  let common_shadows = 
    make_common_shadows is_re package_specs
      (Filename.dirname output_cmi)
      group_dir_index in
  let shadows = List.map (fun f -> 
    Bsb_ninja_util.{
     key = "ppx_flags"; 
     op = AppendList ["-ppx"; f]
   }) local_ppx_flags in
  Bsb_ninja_util.output_build oc
    ~output:output_mliast
      (* TODO: we can get rid of absloute path if we fixed the location to be 
          [lib/bs], better for testing?
      *)
    ~input:(Bsb_config.proj_rel 
              (if is_re then filename_sans_extension ^ Literals.suffix_rei 
               else filename_sans_extension ^ Literals.suffix_mli))
    ~rule:(if is_re then Bsb_rule.build_ast_and_module_sets_from_rei
           else Bsb_rule.build_ast_and_module_sets)
    ~implicit_deps:local_ppx_deps
    ~shadows;
  Bsb_ninja_util.output_build oc
    ~output:output_mliastd
    ~input:output_mliast
    ~rule:Bsb_rule.build_bin_deps
    ?shadows:(if Bsb_dir_index.is_lib_dir group_dir_index  then None
              else Some [{
                  key = Bsb_build_schemas.bsb_dir_group; 
                  op = 
                    Overwrite (string_of_int (group_dir_index :> int )) }])
  ;
  Bsb_ninja_util.output_build oc
    ~shadows:common_shadows
    ~output:output_cmi
    ~input:output_mliast
    ~rule:Bsb_rule.build_cmi;
  [output_mliastd]



let handle_module_info 
    (group_dir_index : Bsb_dir_index.t)
    (package_specs : Bsb_package_specs.t) 
    js_post_build_cmd
    ~bs_suffix
    ~local_ppx_flags
    ~local_ppx_deps
    oc  module_name 
    ( module_info : Bsb_db.module_info)
    namespace
  : info =
  match module_info.ml, module_info.mli with
  | Ml_source (input_impl,impl_is_re,_), 
    Mli_source(input_intf, intf_is_re,_) ->
    emit_impl_build 
      package_specs
      group_dir_index
      oc 
      ~bs_suffix
      ~no_intf_file:false
      ~is_re:impl_is_re
      ~local_ppx_flags
      ~local_ppx_deps
      js_post_build_cmd      
      namespace
      input_impl  @ 
    emit_intf_build 
      package_specs
      group_dir_index
      oc         
      ~is_re:intf_is_re
      ~local_ppx_flags
      ~local_ppx_deps
      namespace
      input_intf 
  | Ml_source(input,is_re,_), Mli_empty ->
    emit_impl_build 
      package_specs
      group_dir_index
      oc 
      ~bs_suffix
      ~no_intf_file:true
      js_post_build_cmd      
      ~is_re
      ~local_ppx_flags
      ~local_ppx_deps
      namespace
      input 
  | Ml_empty, Mli_source(input,is_re,_) ->    
    emit_intf_build 
      package_specs
      group_dir_index
      oc         
      ~is_re
      ~local_ppx_flags
      ~local_ppx_deps
      namespace
      input 
  | Ml_empty, Mli_empty -> zero


let handle_file_group 
    oc 
    ~bs_suffix
    ~custom_rules 
    ~package_specs 
    ~js_post_build_cmd  
    ~local_ppx_flags 
    ~local_ppx_deps
    (files_to_install : String_hash_set.t) 
    (namespace  : string option)
    acc 
    (group: Bsb_parse_sources.file_group ) 
  : info =

  handle_generators oc group custom_rules ;
  String_map.fold (fun  module_name module_info  acc ->
      let installable =
        match group.public with
        | Export_all -> true
        | Export_none -> false
        | Export_set set ->  
          String_set.mem module_name set in
      if installable then 
        String_hash_set.add files_to_install (Bsb_db.filename_sans_suffix_of_module_info module_info);
      (handle_module_info 
        ~bs_suffix
        ~local_ppx_flags
        ~local_ppx_deps
         group.dir_index 
         package_specs js_post_build_cmd 
         oc 
         module_name 
         module_info
         namespace
      ) @  acc
    ) group.sources  acc 

(* Divide the entries into 3 groups:
  - entries: all the non-ppx entries that we'd like to pack or link
  - entries_that_have_ppxes: subset of `entries` which have dependencies on local ppxes
  - ppx_entries: entries that are ppxes, a disjoint set from `entries`
  
  Also returns a list of ppx module names and ppx executable paths.
 *)
let get_local_ppx_deps backend entries =
  List.fold_left Bsb_config_types.(fun (entries, entries_that_have_ppxes, local_ppx_deps, module_names, ppx_entries) project_entry ->
    let dry entry_backend ppx kind main_module_name output_name =
        let (entries, entries_that_have_ppxes) = if backend = entry_backend && kind <> Ppx then 
          if ppx <> [] then
            (project_entry :: entries, project_entry :: entries_that_have_ppxes)
          else
            (project_entry :: entries, entries_that_have_ppxes)
        else 
          (entries, entries_that_have_ppxes) in
        let (local_ppx_deps, module_names, ppx_entries) = if kind = Ppx then 
          let (output, module_name) =
          begin match entry_backend with
          | Js       -> assert false
          | Bytecode -> 
            (match output_name with 
              | None -> 
                let extension = if Ext_sys.is_windows_or_cygwin then ".exe" else "" in
                (String.lowercase main_module_name) ^ ".byte" ^ extension
              | Some name -> name
            ), main_module_name
          | Native   -> 
            (match output_name with 
              | None -> 
                let extension = if Ext_sys.is_windows_or_cygwin then ".exe" else "" in
                (String.lowercase main_module_name) ^ ".native" ^ extension
              | Some name -> name
            ), main_module_name
          end in
        let output = if Ext_sys.is_windows_or_cygwin then output else "./" ^ output in
        (output :: local_ppx_deps, module_name :: module_names, project_entry :: ppx_entries)
        else
          (local_ppx_deps, module_names, ppx_entries ) in
        (entries, entries_that_have_ppxes, local_ppx_deps, module_names, ppx_entries)
      in
      match project_entry with
      | JsTarget { ppx; kind; main_module_name; output_name } -> 
        dry Js ppx kind main_module_name output_name
        
      | NativeTarget { ppx; kind; main_module_name; output_name } -> 
        dry Native ppx kind main_module_name output_name
        
      | BytecodeTarget { ppx; kind; main_module_name; output_name } -> 
        dry Bytecode ppx kind main_module_name output_name
    
  ) ([], [], [], [], []) entries

let handle_file_groups
    oc ~package_specs 
    ~bs_suffix
    ~js_post_build_cmd
    ~files_to_install 
    ~custom_rules
    ~backend
    ~entries
    (file_groups  :  Bsb_parse_sources.file_group list)
    namespace (st : info) : info  =
  let file_groups = List.filter (fun (group : Bsb_parse_sources.file_group) ->
    match backend with 
    | Bsb_config_types.Js       -> 
      not group.is_ppx && List.mem Bsb_parse_sources.Js group.Bsb_parse_sources.backend
    | Bsb_config_types.Native   -> List.mem Bsb_parse_sources.Native group.Bsb_parse_sources.backend
    | Bsb_config_types.Bytecode -> List.mem Bsb_parse_sources.Bytecode group.Bsb_parse_sources.backend
  ) file_groups in 
  let (entries, entries_that_have_ppxes, local_ppx_deps, local_ppx_module_names, _) = get_local_ppx_deps backend entries in
  let (local_ppx_deps, local_ppx_module_names) = begin
    let rec loop_over_ppx (new_local_ppx_deps, new_local_ppx_module_names) local_ppx_deps local_ppx_module_names = 
      begin match (local_ppx_deps, local_ppx_module_names) with 
      | ([], []) -> (new_local_ppx_deps, new_local_ppx_module_names)
      | (ppx_dep :: ppx_dep_rest, ppx_name :: ppx_name_rest) ->
        let rec is_ppx_used entries = begin match entries with 
          | [] -> false
          | Bsb_config_types.JsTarget { ppx; } :: rest
          | Bsb_config_types.NativeTarget { ppx; } :: rest
          | Bsb_config_types.BytecodeTarget { ppx; } :: rest ->
            if List.mem ppx_name ppx then true
            else is_ppx_used rest 
        end in
        if is_ppx_used entries then 
          loop_over_ppx (ppx_dep :: new_local_ppx_deps, ppx_name :: new_local_ppx_module_names) ppx_dep_rest ppx_name_rest
        else 
          loop_over_ppx (new_local_ppx_deps, new_local_ppx_module_names) ppx_dep_rest ppx_name_rest
      | _ -> assert false
    end in
    loop_over_ppx ([], []) local_ppx_deps local_ppx_module_names
  end in
  List.fold_left 
    (fun comp_info (group : Bsb_parse_sources.file_group) -> 
      if group.is_ppx then
        comp_info
      else begin
        let local_ppx_flags = (String_map.fold (fun  module_name _  acc ->
          if acc = [] then begin
            List.fold_left Bsb_config_types.(fun acc e -> 
              match e with 
              | JsTarget { main_module_name; ppx = _ :: _ as ppx } when main_module_name = module_name -> ppx
              | NativeTarget { main_module_name; ppx = _ :: _  } when main_module_name = module_name -> assert false
              | BytecodeTarget { main_module_name; ppx = _ :: _  } when main_module_name = module_name -> assert false
              | _ -> acc
            ) acc entries_that_have_ppxes
          end else 
            acc
        ) group.sources []) in
        (*  map over the flags to find ppxes's full paths if they exist.  *)
        let (local_ppx_flags, local_ppx_deps) = List.fold_left (fun (new_local_ppx_flags, new_local_ppx_deps) flag_exec -> 
          (* @Hack You could potentially have a bug here where the exec_path name is the same as a module name
            inside main-module but eeeeh that'd be weird! *)
          let rec loop = fun local_ppx_deps local_ppx_module_names -> 
            match (local_ppx_deps, local_ppx_module_names) with
            | (exec_path :: local_ppx_deps, ppx_name :: local_ppx_module_names) -> 
              if flag_exec = ppx_name then (exec_path :: new_local_ppx_flags, exec_path :: new_local_ppx_deps)
              else loop local_ppx_deps local_ppx_module_names
            | _ -> (flag_exec :: new_local_ppx_flags, new_local_ppx_deps)
          in
          loop local_ppx_deps local_ppx_module_names
        ) ([], []) local_ppx_flags in
        handle_file_group 
         oc  ~bs_suffix ~package_specs ~custom_rules ~js_post_build_cmd ~local_ppx_flags ~local_ppx_deps
         files_to_install 
         namespace
         comp_info
        group
      end
    ) 
    st  file_groups
