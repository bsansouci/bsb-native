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


module Rules = Bsb_rule 

type compile_target_t = Native | Bytecode

type info =
  string list  

let zero : info =
  [] 


let output_build = Bsb_ninja_util.output_build

let (//) = Ext_path.combine

let install_file module_info files_to_install =
  String_hash_set.add  files_to_install (Bsb_db.filename_sans_suffix_of_module_info module_info)

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
  ~compile_target
  js_post_build_cmd
  ~is_re
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
  let output_cmi =  output_filename_sans_extension ^ Literals.suffix_cmi in
  let output_cmx_or_cmo =
    match compile_target with
    | Bytecode -> output_filename_sans_extension ^ Literals.suffix_cmo
    | Native   -> output_filename_sans_extension ^ Literals.suffix_cmx
  in
  let output_js =
    Bsb_package_specs.get_list_of_output_js package_specs bs_suffix output_filename_sans_extension in 
  let common_shadows = 
    make_common_shadows is_re package_specs
      (Filename.dirname output_cmi)
      group_dir_index in
  output_build oc
    ~output:output_mlast
    ~input
    ~rule:(if is_re then
            Rules.build_ast_and_module_sets_from_re_gen_simple
          else
            Rules.build_ast_and_module_sets_gen_simple);
  let bin_deps_rule = begin match compile_target with
  | Bytecode -> Rules.build_bin_deps_bytecode
  | Native   -> Rules.build_bin_deps_native
  end in
  output_build
    oc
    ~output:output_mlastd
    ~input:output_mlast
    ~rule:bin_deps_rule
    ?shadows:(if Bsb_dir_index.is_lib_dir group_dir_index then None
              else Some [{key = Bsb_build_schemas.bsb_dir_group; 
                          op = Bsb_ninja_util.Overwrite (string_of_int (group_dir_index :> int)) }])
  ;
  let rule_name = begin match compile_target with
  | Bytecode -> Rules.build_cmo_cmi_bytecode
  | Native   -> Rules.build_cmx_cmi_native
  end in
  let cm_outputs, deps =
    if no_intf_file then
      [output_cmi], []
    else    
      [], [output_cmi]
  in
  let deps = match namespace with 
    | None -> deps
    | Some ns -> (ns ^ Literals.suffix_cmi) :: deps
  in
  let shadows =
    match js_post_build_cmd with
    | None -> common_shadows
    | Some cmd ->
      {Bsb_ninja_util.key = Bsb_ninja_global_vars.postbuild;
       op = Bsb_ninja_util.Overwrite ("&& " ^ cmd ^ Ext_string.single_space ^ String.concat Ext_string.single_space output_js)}
        :: common_shadows
  in
  output_build oc
    ~output:output_cmx_or_cmo
    ~shadows
    ~implicit_outputs:cm_outputs
    ~input:output_mlast
    ~implicit_deps:deps
    ~rule:rule_name ;
  [output_mlastd]

let emit_intf_build 
    (package_specs : Bsb_package_specs.t)
    (group_dir_index : Bsb_dir_index.t)
    oc
    ~is_re
    ~compile_target
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
  Bsb_ninja_util.output_build oc
    ~output:output_mliast
      (* TODO: we can get rid of absloute path if we fixed the location to be 
          [lib/bs], better for testing?
      *)
    ~input:(Bsb_config.proj_rel 
              (if is_re then filename_sans_extension ^ Literals.suffix_rei 
               else filename_sans_extension ^ Literals.suffix_mli))
    ~rule:(if is_re then Bsb_rule.build_ast_and_module_sets_from_rei_gen_simple
           else Bsb_rule.build_ast_and_module_sets_gen_simple);
  Bsb_ninja_util.output_build oc
    ~output:output_mliastd
    ~input:output_mliast
    ~rule:Bsb_rule.build_bin_deps
    ?shadows:(if Bsb_dir_index.is_lib_dir group_dir_index  then None
              else Some [{
                  key = Bsb_build_schemas.bsb_dir_group; 
                  op = 
                    Overwrite (string_of_int (group_dir_index :> int )) }]);
    
  let rule = begin match compile_target with
  | Bytecode -> Rules.build_cmi_bytecode
  | Native   -> Rules.build_cmi_native
  end in
  (* TODO(sansouci): Do we need this? *)
  (* let deps = match namespace with 
    | None -> []
    | Some ns -> [ns ^ Literals.suffix_cmi]
  in *)
  Bsb_ninja_util.output_build oc
    ~shadows:common_shadows
    ~output:output_cmi
    ~input:output_mliast
    ~rule;
  [output_mliastd]

let handle_module_info  
  (group_dir_index : Bsb_dir_index.t)
  (package_specs : Bsb_package_specs.t) 
  js_post_build_cmd
  ~compile_target
  ~bs_suffix
  oc  module_name 
  ( module_info : Bsb_db.module_info)
  namespace  =
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
      ~compile_target
      js_post_build_cmd      
      namespace
      input_impl  @ 
    emit_intf_build 
      package_specs
      group_dir_index
      oc         
      ~is_re:intf_is_re
      ~compile_target
      namespace
      input_intf 
  | Ml_source(input,is_re,_), Mli_empty ->
    emit_impl_build 
      package_specs
      group_dir_index
      oc 
      ~bs_suffix
      ~no_intf_file:true
      ~compile_target
      js_post_build_cmd      
      ~is_re
      namespace
      input 
  | Ml_empty, Mli_source(input,is_re,_) ->
    emit_intf_build 
      package_specs
      group_dir_index
      oc         
      ~is_re
      ~compile_target
      namespace
      input 
  | Ml_empty, Mli_empty -> zero


let handle_file_group oc
  ~custom_rules
  ~compile_target
  ~package_specs
  ~js_post_build_cmd
  ~namespace
  ~bs_suffix
  (files_to_install : String_hash_set.t)
  acc
  (group: Bsb_parse_sources.file_group) : Bsb_ninja_file_groups.info =
  
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
        ~compile_target
        group.dir_index 
        package_specs 
        js_post_build_cmd 
        oc 
        module_name 
        module_info
        namespace
      ) @  acc
    ) group.sources  acc 

let link oc ret ~entries ~file_groups ~static_libraries ~c_linker_flags ~namespace ~external_deps_for_linking ~ocaml_dir =
  List.fold_left (fun acc project_entry ->
    let output, rule_name, library_file_name, suffix_cmo_or_cmx, main_module_name, shadows =
      begin match project_entry with
      | Bsb_config_types.JsTarget main_module_name       -> assert false
      | Bsb_config_types.BytecodeTarget main_module_name -> 
        (String.lowercase main_module_name) ^ ".byte" , 
        Rules.linking_bytecode, 
        "lib" ^ Literals.suffix_cma, 
        Literals.suffix_cmo, 
        main_module_name, 
        []
      | Bsb_config_types.NativeTarget main_module_name   -> 
        (String.lowercase main_module_name) ^ ".native", 
        Rules.linking_native  , 
        "lib" ^ Literals.suffix_cmxa, 
        Literals.suffix_cmx, 
        main_module_name,
        []
      end in
    let (all_mlast_files, all_cmo_or_cmx_files, all_cmi_files) =
      List.fold_left (fun acc group -> 
        String_map.fold (fun _ (v : Bsb_db.module_info) (all_mlast_files, all_cmo_or_cmx_files, all_cmi_files) -> 
          let mlname = match v.ml with
            | Ml_source (input, _, _) ->
            let input = (Ext_path.chop_extension_if_any input)  in
            begin match namespace with 
              | None    -> Some (input, input)
              | Some ns -> Some (input, (Ext_namespace.make ~ns input))
            end
            | Ml_empty -> None
          in
          let mliname = match v.mli with
            | Mli_source (input, _, _) ->
            let input = (Ext_path.chop_extension_if_any input)  in
            begin match namespace with 
              | None    -> Some (input, input)
              | Some ns -> Some (input, (Ext_namespace.make ~ns input))
            end
            | Mli_empty -> None 
          in
          begin match (mlname, mliname) with
          | None, None -> failwith "Got a source file without an ml or mli file. This should not happen."
          | Some (name, namespacedName), Some _ ->
            ((name ^ Literals.suffix_mlast) :: all_mlast_files,
             (namespacedName ^ suffix_cmo_or_cmx)     :: all_cmo_or_cmx_files,
             (namespacedName ^ Literals.suffix_cmi)   :: all_cmi_files)
          | Some (name, namespacedName), None ->
            ((name ^ Literals.suffix_mlast) :: all_mlast_files,
             (namespacedName ^ suffix_cmo_or_cmx)     :: all_cmo_or_cmx_files,
             (namespacedName ^ Literals.suffix_cmi)   :: all_cmi_files)
          | None, Some (_, namespacedName) ->
            (all_mlast_files,
             all_cmo_or_cmx_files,
             (namespacedName ^ Literals.suffix_cmi)   :: all_cmi_files)
          end    
        ) group.Bsb_parse_sources.sources acc) 
      ([], [], [])
      file_groups in
    let shadows = shadows @ [{
      Bsb_ninja_util.key = "main_module";
      op = Bsb_ninja_util.Overwrite main_module_name
    }; {
      key = "static_libraries";
      op = Bsb_ninja_util.Overwrite (Bsb_build_util.flag_concat "-add-clib" (c_linker_flags @ static_libraries))
    }] in
    output_build oc
      ~output
      ~input:""
      ~inputs:all_mlast_files
      ~implicit_deps:((List.map (fun dep -> (Ext_bytes.ninja_escaped dep) // library_file_name) external_deps_for_linking) @ all_cmi_files @ all_cmo_or_cmx_files @ static_libraries)
      ~shadows
      ~rule:rule_name;
    acc
  ) ret entries
    
let pack oc ret ~backend ~file_groups ~namespace =
  let output_cma_or_cmxa, rule_name, suffix_cmo_or_cmx =
    begin match backend with
    (* These cases could benefit from a better error message. *)
    | Bsb_config_types.Js       -> assert false
    | Bsb_config_types.Bytecode -> 
      Literals.library_file ^ Literals.suffix_cma , 
      Rules.build_cma_library , 
      Literals.suffix_cmo
    | Bsb_config_types.Native   -> 
      Literals.library_file ^ Literals.suffix_cmxa, 
      Rules.build_cmxa_library, 
      Literals.suffix_cmx
  end in
  (* TODO(sansouci): we pack all source files of the dependency, but we could just pack the
     files that are used by the main project. *)
  let all_cmo_or_cmx_files, all_cmi_files =
    List.fold_left (fun acc group -> 
      String_map.fold (fun _ (v : Bsb_db.module_info) (all_cmo_or_cmx_files, all_cmi_files) -> 
        let mlname = match v.ml with
            | Ml_source (input, _, _) ->
            let input = (Ext_path.chop_extension_if_any input)  in
            begin match namespace with 
              | None    -> Some (input)
              | Some ns -> Some ((Ext_namespace.make ~ns input))
            end
            | Ml_empty -> None
          in
          let mliname = match v.mli with
            | Mli_source (input, _, _) ->
            let input = (Ext_path.chop_extension_if_any input)  in
            begin match namespace with 
              | None    -> Some (input)
              | Some ns -> Some ((Ext_namespace.make ~ns input))
            end
            | Mli_empty -> None 
          in
          begin match (mlname, mliname) with
          | None, None -> failwith "Got a source file without an ml or mli file. This should not happen."
          | Some name, Some _ ->
            ((name ^ suffix_cmo_or_cmx)     :: all_cmo_or_cmx_files,
             (name ^ Literals.suffix_cmi)   :: all_cmi_files)
          | Some name, None ->
            ((name ^ suffix_cmo_or_cmx)     :: all_cmo_or_cmx_files,
             (name ^ Literals.suffix_cmi)   :: all_cmi_files)
          | None, Some name ->
            (all_cmo_or_cmx_files,
             (name ^ Literals.suffix_cmi)   :: all_cmi_files)
          end    
    ) group.Bsb_parse_sources.sources acc) 
    ([], [])
    file_groups in
  (* In the case that a library is just an interface file, we don't do anything *)
  if List.length all_cmo_or_cmx_files > 0 then begin
    output_build oc
      ~output:output_cma_or_cmxa
      ~input:""
      ~inputs:all_cmo_or_cmx_files
      ~implicit_deps:all_cmi_files
      ~rule:rule_name;
    ret @ []
  end else ret

let handle_file_groups oc
  ~custom_rules
  ~is_top_level
  ~entries
  ~compile_target
  ~backend
  ~package_specs
  ~js_post_build_cmd
  ~files_to_install
  ~static_libraries
  ~c_linker_flags
  ~external_deps_for_linking
  ~ocaml_dir
  ~bs_suffix
  (file_groups  :  Bsb_parse_sources.file_group list) namespace st =
  let file_groups = List.filter (fun (group : Bsb_parse_sources.file_group) ->
    match backend with 
    | Bsb_config_types.Js       -> List.mem Bsb_parse_sources.Js group.Bsb_parse_sources.backend
    | Bsb_config_types.Native   -> List.mem Bsb_parse_sources.Native group.Bsb_parse_sources.backend
    | Bsb_config_types.Bytecode -> List.mem Bsb_parse_sources.Bytecode group.Bsb_parse_sources.backend
  ) file_groups in 
  let ret = List.fold_left (
    handle_file_group oc
      ~custom_rules 
      ~compile_target
      ~package_specs
      ~js_post_build_cmd 
      ~namespace
      ~bs_suffix
      files_to_install
  ) st file_groups in
  if is_top_level then
    link oc ret ~entries ~file_groups ~static_libraries ~c_linker_flags ~namespace ~external_deps_for_linking ~ocaml_dir
  else
    pack oc ret ~backend ~file_groups ~namespace
