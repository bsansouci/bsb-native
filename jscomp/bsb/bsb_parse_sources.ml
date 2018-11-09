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



type build_generator = Bsb_file_groups.build_generator

type public = Bsb_file_groups.public 

type file_group = Bsb_file_groups.file_group

type t = Bsb_file_groups.t 

let is_input_or_output (xs : build_generator list) (x : string)  = 
  List.exists 
    (fun  ({input; output} : build_generator) -> 
       let it_is = (fun y -> y = x ) in
       List.exists it_is input ||
       List.exists it_is output
    ) xs   

let warning_unused_file : _ format = 
  "@{<warning>IGNORED@}: file %s under %s is ignored because it can't be turned into a valid module name. The build system transforms a file name into a module name by upper-casing the first letter@."

type cxt = {
  not_dev : bool ;
  dir_index : Bsb_dir_index.t ; 
  cwd : string ;
  root : string;
  cut_generators : bool;
  traverse : bool;
  namespace : string option;
  clean_staled_bs_js: bool;
#if BS_NATIVE then
  backend: Bsb_file_groups.compilation_kind_t list;
  is_ppx: bool;
  ppx: string list;
#end
}

(** [public] has a list of modules, we do a sanity check to see if all the listed 
  modules are indeed valid module components
*)
let collect_pub_modules 
    (xs : Ext_json_types.t array)
    (cache : Bsb_db.t) : String_set.t = 
  let set = ref String_set.empty in 
  for i = 0 to Array.length xs - 1 do 
    let v = Array.unsafe_get xs i in 
    match v with 
    | Str { str ; loc }
      -> 
      if String_map.mem str cache then 
        set := String_set.add str !set
      else 
        begin 
          Bsb_log.warn
            "@{<warning>IGNORED@} %S in public is ignored since it is not\
             an existing module@." str
        end  
    | _ -> 
      Bsb_exception.errorf 
        ~loc:(Ext_json.loc_of v)
        "public excpect a list of strings"
  done  ;
  !set

let extract_pub (input : Ext_json_types.t String_map.t) (cur_sources : Bsb_db.t) =   
  match String_map.find_opt Bsb_build_schemas.public input with 
  | Some (Str{str = s; loc}) ->  
    if s = Bsb_build_schemas.export_all then (Export_all : public) else 
    if s = Bsb_build_schemas.export_none then Export_none else 
      Bsb_exception.errorf ~loc "invalid str for %s "  s 
  | Some (Arr {content = s}) ->         
    Export_set (collect_pub_modules s cur_sources)
  | Some config -> 
    Bsb_exception.config_error config "expect array or string"
  | None ->
    Export_all 

let extract_resources (input : Ext_json_types.t String_map.t) =   
  match String_map.find_opt  Bsb_build_schemas.resources  input with 
  | Some (Arr {content = s}) ->
    Bsb_build_util.get_list_string s 
  | Some config -> 
    Bsb_exception.config_error config 
      "expect array "  
  | None -> [] 


let  handle_empty_sources 
    ( cur_sources : Bsb_db.t ref)
    dir 
    (file_array : string array Lazy.t)
    ({loc_start; loc_end} : Ext_json_types.json_array) 
    generators
  : Ext_file_pp.interval list  =    
  let files_array = Lazy.force file_array in 
  let dyn_file_array = String_vec.make (Array.length files_array) in 
  let files  =
    Array.fold_left (fun acc name -> 
        if is_input_or_output generators name then acc 
        else
          match Ext_string.is_valid_source_name name with 
          | Good ->   begin 
              let new_acc = Bsb_db.map_update ~dir acc name  in 
              String_vec.push name dyn_file_array ;
              new_acc 
            end 
          | Invalid_module_name ->
            Bsb_log.warn
              warning_unused_file name dir ;
            acc 
          | Suffix_mismatch -> acc 
      ) !cur_sources files_array in 
  cur_sources := files ;    
  [ Ext_file_pp.patch_action dyn_file_array 
      loc_start loc_end
  ]
  (* ,
  files *)


let extract_input_output 
    (loc_start : Ext_position.t) 
    (content : Ext_json_types.t array) : string list * string list = 
  let error () = 
    Bsb_exception.errorf ~loc:loc_start {| invalid edge format, expect  ["output" , ":", "input" ]|}
  in  
  match Ext_array.find_and_split content 
          (fun x () -> match x with Str { str =":"} -> true | _ -> false )
          () with 
  | `No_split -> error ()
  | `Split (  output, input) -> 
    (Ext_array.to_list_map (fun (x : Ext_json_types.t) -> 
        match x with
        | Str {str = ":"} -> 
          error ()
        | Str {str } ->           
          Some str 
        | _ -> None) output
    ,
    Ext_array.to_list_map (fun (x : Ext_json_types.t) -> 
        match x with
        | Str {str = ":"} -> 
          error () 
        | Str {str} -> 
          Some str (* More rigirous error checking: It would trigger a ninja syntax error *)
        | _ -> None) input)

let extract_generators 
    (input : Ext_json_types.t String_map.t) 
    (cut_generators_or_not_dev : bool) 
    (dir : string) 
    (cur_sources : Bsb_db.t ref)
     : build_generator list  =
  let generators : build_generator list ref  = ref [] in
  begin match String_map.find_opt Bsb_build_schemas.generators input with
    | Some (Arr { content ; loc_start}) ->
      (* Need check is dev build or not *)
      Ext_array.iter content (fun x ->
        match x with
        | Obj { map = generator; loc} ->
          begin match String_map.find_opt Bsb_build_schemas.name generator,
                      String_map.find_opt Bsb_build_schemas.edge generator
            with
            | Some (Str{str = command}), Some (Arr {content })->

              let output, input = extract_input_output loc_start content in 
              if not cut_generators_or_not_dev then begin 
                generators := {input ; output ; command } :: !generators
              end;
              (* ATTENTION: Now adding output as source files, 
                 it may be re-added again later when scanning files (not explicit files input)
              *)
              output |> List.iter begin fun  output -> 
                  match Ext_string.is_valid_source_name output with
                  | Good ->
                    cur_sources := Bsb_db.map_update ~dir !cur_sources output
                  | Invalid_module_name ->                  
                    Bsb_log.warn warning_unused_file output dir 
                  | Suffix_mismatch -> ()                
              end
            | _ ->
              Bsb_exception.errorf ~loc "Invalid generator format"
          end
        | _ -> Bsb_exception.errorf ~loc:(Ext_json.loc_of x) "Invalid generator format"
      )  
    | Some x  -> Bsb_exception.errorf ~loc:(Ext_json.loc_of x ) "Invalid generator format"
    | None -> ()
  end ;
  !generators 

(** [parsing_source_dir_map cxt input]
    Major work done in this function, 
    assume [not_dev && not (Bsb_dir_index.is_lib_dir dir_index)]      
    is already checked, so we don't need check it again    
*)
let try_unlink s = 
  try Unix.unlink s  
  with _ -> 
    Bsb_log.info "@{<info>Failed to remove %s}@." s 


(** This is the only place where we do some removal during scanning,
  configurabl
*)    
let clean_staled_bs_js_files 
    (context : cxt) 
    (cur_sources : _ String_map.t ) 
    (files : string array)  =     
  Ext_array.iter files (fun current_file -> 
    match Ext_namespace.ends_with_bs_suffix_then_chop current_file  with
    | None -> ()
    | Some basename -> (* Found [.bs.js] files *)
         let parent = Filename.concat context.root context.cwd in 
         let lib_parent = 
#if BS_NATIVE then
           Filename.concat (Filename.concat context.root Bsb_config.lib_bs) 
             (Bsb_build_util.get_build_artifacts_location context.cwd) in 
#else
           Filename.concat (Filename.concat context.root Bsb_config.lib_bs) 
             context.cwd in 
#end
         if not (String_map.mem (Ext_string.capitalize_ascii basename) cur_sources) then 
           begin 
             Unix.unlink (Filename.concat parent current_file);
             let basename = 
               match context.namespace with  
               | None -> basename
               | Some ns -> Ext_namespace.make ~ns basename in 
             (
              (* \O/ we use env vars!?!?
                    Bne - August 10th 2018 *)
               match Sys.getenv "BS_CMT_POST_PROCESS_CMD" with 
               | exception _ -> ()
               | cmd -> 
                 Ext_pervasives.try_it (fun _ -> 
                   Sys.command (
                     cmd ^ 
                     " -cmt-rm " ^
                     Filename.concat lib_parent (basename ^ Literals.suffix_cmt))                   
                 )
             );
             Ext_list.iter [
                (* What about cmi, cmt, mlast and mliast ?
                       Ben - August 10th 2018 *)
                Literals.suffix_cmi; Literals.suffix_cmj ; 
                Literals.suffix_cmt; Literals.suffix_cmti ; 
                Literals.suffix_mlast; Literals.suffix_mlastd;
                Literals.suffix_mliast; Literals.suffix_mliastd
                (*TODO: GenType*)
             ] (fun suffix -> 
              try_unlink (Filename.concat lib_parent (basename ^ suffix))
             )
           end           
  )

#if BS_NATIVE then
let parse_backend parent_backend input =
  let maybeBackend : Ext_json_types.t option = begin match String_map.find_opt Bsb_build_schemas.kind input with
    | None -> String_map.find_opt Bsb_build_schemas.backend input
    | x ->
      Bsb_log.warn "@{<warn>Warning@} 'kind' field in 'sources' is deprecated and will be removed in the next release. Please use 'backend'.@.";
      x
  end in
  begin match maybeBackend with 
    | Some (Arr {loc_start; content = s }) -> (* [ a,b ] *)
      List.map (fun (s : string) ->
        match s with 
        | "js"       -> Bsb_file_groups.Js
        | "native"   -> Bsb_file_groups.Native
        | "bytecode" -> Bsb_file_groups.Bytecode
        | str -> Bsb_exception.errorf ~loc:loc_start "'backend' field expects one of: 'js', 'bytecode' or 'native'. Found '%s'" str
      ) (Bsb_build_util.get_list_string s);
    | Some (Str {str = "js"} )       -> [Bsb_file_groups.Js]
    | Some (Str {str = "native"} )   -> [Bsb_file_groups.Native]
    | Some (Str {str = "bytecode"} ) -> [Bsb_file_groups.Bytecode]
    | Some x -> Bsb_exception.config_error x "'backend' field expects one of: 'js', 'bytecode' or 'native'"
    | None -> parent_backend
  end
#end

let rec 
  parsing_source_dir_map 
    ({ cwd =  dir; not_dev; cut_generators ; 
       traverse = cxt_traverse ;
     } as cxt )
    (input : Ext_json_types.t String_map.t) : t     
  = 
  let cur_update_queue = ref [] in 
  let cur_globbed_dirs = ref [] in 
  let cur_sources = ref String_map.empty in 
  let generators = 
      extract_generators input (cut_generators || not_dev) dir 
      cur_sources
  in 
  let sub_dirs_field = String_map.find_opt Bsb_build_schemas.subdirs input in 
  let file_array = lazy (Sys.readdir (Filename.concat cxt.root dir)) in 
  begin 
    match String_map.find_opt Bsb_build_schemas.files input with 
    | None ->  (* No setting on [!files]*)
      (** We should avoid temporary files *)
      cur_sources := 
        Array.fold_left (fun acc name -> 
            if is_input_or_output generators name then 
              acc 
            else 
              match Ext_string.is_valid_source_name name with 
              | Good -> 
                Bsb_db.map_update  ~dir acc name 
              | Invalid_module_name ->
                Bsb_log.warn
                  warning_unused_file
                  name dir 
                ; 
                acc 
              | Suffix_mismatch ->  acc
          ) !cur_sources (Lazy.force file_array);
      cur_globbed_dirs :=  [dir]  
    | Some (Arr ({content = [||] }as empty_json_array)) -> 
      (* [ ] populatd by scanning the dir (just once) *)         
      cur_update_queue := 
          handle_empty_sources cur_sources cxt.cwd 
          file_array 
          empty_json_array
          generators
    | Some (Arr {loc_start;loc_end; content = sx }) -> 
      (* [ a,b ] populated by users themselves 
         TODO: still need check?
      *)      
      cur_sources := 
        Array.fold_left (fun acc (s : Ext_json_types.t) ->
            match s with 
            | Str {str = s} -> 
              Bsb_db.map_update ~dir acc s
            | _ -> acc
          ) !cur_sources sx    
    | Some (Obj {map = m; loc} ) -> (* { excludes : [], slow_re : "" }*)
      cur_globbed_dirs := [dir];  
      let excludes = 
        match String_map.find_opt Bsb_build_schemas.excludes m with 
        | None -> []   
        | Some (Arr {content = arr}) -> Bsb_build_util.get_list_string arr 
        | Some x -> Bsb_exception.config_error x  "excludes expect array "in 
      let slow_re = String_map.find_opt Bsb_build_schemas.slow_re m in 
      let predicate = 
        match slow_re, excludes with 
        | Some (Str {str = s}), [] -> 
          let re = Str.regexp s  in 
          fun name -> Str.string_match re name 0 
        | Some (Str {str = s}) , _::_ -> 
          let re = Str.regexp s in   
          fun name -> Str.string_match re name 0 && not (List.mem name excludes)
        | Some x, _ -> Bsb_exception.errorf ~loc "slow-re expect a string literal"
        | None , _ -> Bsb_exception.errorf ~loc  "missing field: slow-re"  in 
      cur_sources := Array.fold_left (fun acc name -> 
          if is_input_or_output generators name || not (predicate name) then acc 
          else 
            Bsb_db.map_update  ~dir acc name 
        ) !cur_sources (Lazy.force file_array)      
    | Some x -> Bsb_exception.config_error x "files field expect array or object "
  end;
  let cur_sources = !cur_sources in 
  let resources = extract_resources input in
  let public = extract_pub input cur_sources in 
  (** Doing recursive stuff *)  
  let children =     
    match sub_dirs_field, 
          cxt_traverse with 
    | None , true
    | Some (True _), _ -> 
      let root = cxt.root in 
      let parent = Filename.concat root dir in
      Array.fold_left (fun origin x -> 
            if Sys.is_directory (Filename.concat parent x) then 
              Bsb_file_groups.merge
              (
                parsing_source_dir_map
                  {cxt with 
                   cwd = Ext_path.concat cxt.cwd 
                        (Ext_filename.simple_convert_node_path_to_os_path x);
                   traverse = true
                  } String_map.empty)  origin               
            else origin  
          ) Bsb_file_groups.empty (Lazy.force file_array) 
        (* readdir parent avoiding scanning twice *)        
    | None, false  
    | Some (False _), _  -> Bsb_file_groups.empty
    | Some s, _  -> parse_sources cxt s 
  in 
  (** Do some clean up *)  
  if cxt.clean_staled_bs_js then 
  begin
    clean_staled_bs_js_files cxt cur_sources (Lazy.force file_array )
  end;
  Bsb_file_groups.merge {
    files =  [ { dir ; 
                 sources = cur_sources; 
                 resources ;
                 public ;
                 dir_index = cxt.dir_index ;
#if BS_NATIVE then
                 backend = cxt.backend;
                 is_ppx = cxt.is_ppx;
                 ppx = cxt.ppx;
#end
                generators  } ] ;
    intervals = !cur_update_queue ;
    globbed_dirs = !cur_globbed_dirs ;
  }  children


and parsing_single_source ({not_dev; dir_index ; cwd} as cxt ) (x : Ext_json_types.t )
  : t  =
  match x with 
  | Str  { str = dir }  -> 
    if not_dev && not (Bsb_dir_index.is_lib_dir dir_index) then 
      Bsb_file_groups.empty
    else 
      parsing_source_dir_map 
        {cxt with 
         cwd = Ext_path.concat cwd (Ext_filename.simple_convert_node_path_to_os_path dir)}
        String_map.empty  
  | Obj {map} ->
#if BS_NATIVE then
    let backend = parse_backend cxt.backend map in
    let ppx = match String_map.find_opt Bsb_build_schemas.ppx map with 
        | Some (Arr {loc_start; content = s }) -> Bsb_build_util.get_list_string s
        | Some (Str {str} )                    -> [ str ]
        | None                                 -> cxt.ppx
        | _ -> Bsb_exception.config_error x "Field 'ppx' not recognized. Should be a string or an array of strings." 
      in
    let (current_dir_index, is_ppx) = 
      match String_map.find_opt Bsb_build_schemas.type_ map with 
      | Some (Str {str="dev"}) -> (Bsb_dir_index.get_dev_index (), false)
      | Some (Str {str="ppx"}) -> (dir_index, true)
      | Some _ -> Bsb_exception.config_error x {|type field expect "dev" or "ppx" literal |}
      | None -> (dir_index, cxt.is_ppx) in 
#else
    let current_dir_index = 
      match String_map.find_opt Bsb_build_schemas.type_ map with 
      | Some (Str {str="dev"}) -> 
        Bsb_dir_index.get_dev_index ()
      | Some _ -> Bsb_exception.config_error x {|type field expect "dev" literal |}
      | None -> dir_index in 
#end
    if not_dev && not (Bsb_dir_index.is_lib_dir current_dir_index) then 
      Bsb_file_groups.empty 
    else 
      let dir = 
        match String_map.find_opt Bsb_build_schemas.dir map with 
        | Some (Str{str}) -> 
          Ext_filename.simple_convert_node_path_to_os_path str 
        | Some x -> Bsb_exception.config_error x "dir expected to be a string"
        | None -> 
          Bsb_exception.config_error x
            (
              "required field :" ^ Bsb_build_schemas.dir ^ " missing" )

      in
      parsing_source_dir_map 
#if BS_NATIVE then
        {cxt with 
          dir_index = current_dir_index; 
          cwd= Ext_path.concat cwd dir;
          backend = backend;
          is_ppx=is_ppx;
          ppx = ppx;
        } map
#else
        {cxt with dir_index = current_dir_index; 
                  cwd= Ext_path.concat cwd dir} map
#end
  | _ -> Bsb_file_groups.empty
and  parsing_arr_sources cxt (file_groups : Ext_json_types.t array)  = 
  Array.fold_left (fun  origin x ->
      Bsb_file_groups.merge (parsing_single_source cxt x) origin 
    ) Bsb_file_groups.empty  file_groups 

and  parse_sources ( cxt : cxt) (sources : Ext_json_types.t )  = 
  match sources with   
  | Arr file_groups -> 
    parsing_arr_sources cxt file_groups.content
  | _ -> parsing_single_source cxt sources



let scan ~not_dev ~root ~cut_generators ~namespace ~clean_staled_bs_js x = 
  parse_sources {
    not_dev;
    dir_index = Bsb_dir_index.lib_dir_index;
    cwd = Filename.current_dir_name;
    root ;
    cut_generators;
    namespace;
    clean_staled_bs_js;
#if BS_NATIVE then
    backend = [Bsb_file_groups.Js; Bsb_file_groups.Native; Bsb_file_groups.Bytecode];
    is_ppx = false;
    ppx = [];
#end
    traverse = false
  } x



type walk_cxt = {
    cwd : string ;
    root : string;
    traverse : bool;
  }
  
let rec walk_sources (cxt : walk_cxt) (sources : Ext_json_types.t) = 
  match sources with 
  | Arr {content =  file_groups} -> 
    Ext_array.iter file_groups (fun x -> walk_single_source cxt x) 
  | x -> walk_single_source  cxt x    
and walk_single_source cxt (x : Ext_json_types.t) =      
  match x with 
  | Str {str = dir} 
    -> 
    walk_source_dir_map 
      {cxt with 
        cwd = 
        Ext_path.concat cxt.cwd 
        (Ext_filename.simple_convert_node_path_to_os_path dir)
      }
      String_map.empty   
  | Obj {map} ->       
    begin match String_map.find_opt Bsb_build_schemas.dir map with 
    | Some (Str{str}) -> 
      let dir = Ext_filename.simple_convert_node_path_to_os_path str  in 
      walk_source_dir_map 
      {cxt with cwd = Ext_path.concat cxt.cwd dir} map
    | _ -> ()
    end
  | _ -> ()  
and walk_source_dir_map (cxt : walk_cxt) (input : Ext_json_types.t String_map.t) =   
    let working_dir = Filename.concat cxt.root cxt.cwd in 
    let file_array = Sys.readdir working_dir in 
    (* Remove .re.js when clean up *)
    Ext_array.iter file_array begin fun file -> 
        if Ext_string.ends_with file Literals.suffix_re_js then 
          Sys.remove (Filename.concat working_dir file)
    end; 
    let sub_dirs_field = 
        String_map.find_opt Bsb_build_schemas.subdirs input in 
    let cxt_traverse = cxt.traverse in     
    match sub_dirs_field, cxt_traverse with     
    | None, true 
    | Some(True _), _ -> 
      Ext_array.iter file_array begin fun f -> 
      if Sys.is_directory (Filename.concat working_dir f ) then 
        walk_source_dir_map 
        {cxt with 
          cwd = 
            Ext_path.concat cxt.cwd
            (Ext_filename.simple_convert_node_path_to_os_path f);
          traverse = true
          } String_map.empty 
      end   
    | None, _ 
    | Some (False _), _ -> ()      
    | Some s, _ -> walk_sources cxt s 

(* It makes use of the side effect when [walk_sources], removing suffix_re_js,
   TODO: make it configurable
 *)
let clean_re_js root =     
  match Ext_json_parse.parse_json_from_file 
      (Filename.concat root Literals.bsconfig_json) with 
  | Obj {map ; loc} -> 
    (match String_map.find_opt Bsb_build_schemas.sources map with 
    | Some config -> 
      Ext_pervasives.try_it (fun () -> 
           walk_sources { root ; traverse = true; cwd = Filename.current_dir_name} config
        )      
    | None  -> ())
  | _  -> () 
  | exception _ -> ()    
  
