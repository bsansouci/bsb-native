(*
  For Windows, we distribute a prebuilt bsc.exe
  To build on windows, we still need figure out constructing config.ml
  from existing compiler

  For other OSes, we detect
  if there is other compiler installed and the version matches,
  we get the config.ml from existing OCaml compiler and build `whole_compiler`

  Otherwise, we build the compiler shipped with Buckle and use the
  old compiler.ml
*)

let ( // ) = Filename.concat 

let get_matching_name = function
  | "LIBDIR" -> "standard_library_default"
  | "BYTERUN" -> "standard_runtime"
  | "CCOMPTYPE" -> "ccomp_type"
  | "BYTECC" -> "bytecomp_c_compiler"
  | "BYTECCLIBS" -> "bytecomp_c_libraries"
  | "NATIVECC" -> "native_c_compiler"
  | "NATIVECCLIBS" -> "native_c_libraries"
  | "PACKLD" -> "native_pack_linker"
  | "RANLIBCMD" -> "ranlib"
  | "ARCMD" -> "ar"
  | "CC_PROFILE" -> "cc_profile"

  | "MKDLL" -> "mkdll" (* undefined *)
  | "MKEXE" -> "mkexe" (* undefined *)
  | "MKMAINDLL" -> "mkmaindll" (* undefined TODO= upstream to print it too *)

  | "ARCH" -> "architecture"
  | "MODEL" -> "model"
  | "SYSTEM" -> "system"
  | "ASM" -> "asm"
  | "ASM_CFI_SUPPORTED" -> "asm_cfi_supported" (* boolean *)
  | "WITH_FRAME_POINTERS" -> "with_frame_pointers" (* boolean *)
  | "EXT_OBJ" -> "ext_obj"
  | "EXT_ASM" -> "ext_asm"
  | "EXT_LIB" -> "ext_lib"
  | "EXT_DLL" -> "ext_lib"
  | "HOST" -> "host"
  | "TARGET" -> "target"
  | "SYSTHREAD_SUPPORT" -> "systhread_supported" (* boolean *)
  | _ -> ""

let sp = Printf.sprintf

let print = Printf.printf

let gen_bsb_default_paths ~jscomp_dir ~bin_dir ~ocaml_dir =
  let bsb_default_paths = jscomp_dir // "bsb" // "bsb_default_paths.mlp" in
  let ic = open_in bsb_default_paths in
  let buf = Buffer.create 64 in
  (try
     while true do
      let line = input_line ic in
      begin match (Ext_string.find ~sub:"%%" line) with
      | -1 -> 
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
      | start -> 
      begin match (Ext_string.find ~sub:"%%" (String.sub line (start + 2) (String.length line - (start + 2)))) with
        | -1 -> assert false
        | endd -> 
          let wordToReplace = String.sub line (start + 2) endd in
          Buffer.add_string buf (String.sub line 0 start);
          begin match wordToReplace with
            | "BIN_DIR"   ->  Buffer.add_string buf bin_dir;
            | "OCAML_DIR" ->  Buffer.add_string buf ocaml_dir;
            | _ -> failwith @@ sp "Don't know how to replace Keyword '%s'." wordToReplace
          end;
          let len = String.length line - start - 2 - endd - 2 in 
          if len > 0 then
            Buffer.add_string buf (String.sub line (start + endd + 2 + 2) len);
          Buffer.add_char buf '\n';
        end
      end
     done
   with End_of_file -> ());
  let bsb_default_paths_output = jscomp_dir // "bsb" // "bsb_default_paths.ml" in
  let oc = open_out bsb_default_paths_output in
  Buffer.output_buffer oc buf;
  close_out oc;
  let bsb_default_paths_output = jscomp_dir // "bin" // "bsb_default_paths.ml" in
  let oc = open_out bsb_default_paths_output in
  Buffer.output_buffer oc buf;
  close_out oc


let patch_config jscomp_dir config_map =
  let whole_compiler_config = jscomp_dir // "bin" // "config_whole_compiler.mlp" in
  let whole_compiler_config_output = jscomp_dir // "bin" // "config_whole_compiler.ml" in
  let ic = open_in whole_compiler_config in
  let buf = Buffer.create 64 in
  (try
     while true do
      let line = input_line ic in
      begin match (Ext_string.find ~sub:"%%" line) with
      | -1 -> 
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
      | start -> 
      begin match (Ext_string.find ~sub:"%%" (String.sub line (start + 2) (String.length line - (start + 2)))) with
        | -1 -> assert false
        | endd -> 
          let wordToReplace = get_matching_name @@ String.sub line (start + 2) endd in
          Buffer.add_string buf (String.sub line 0 start);
          if wordToReplace = "standard_library_default" then begin
            let origin_path = (Filename.dirname jscomp_dir) // "lib" // "ocaml" in
            Buffer.add_string buf ("\"" ^ origin_path ^ "\"");
          end else begin 
            begin match (List.filter (fun (key, v) -> key = wordToReplace) config_map) with
            | [ (_, value) ] -> Buffer.add_string buf value;
            | _ -> print_endline @@ "No value found for " ^ wordToReplace
            end;
          end;
          let len = String.length line - start - 2 - endd - 2 in 
          if len > 0 then
            Buffer.add_string buf (String.sub line (start + endd + 2 + 2) len);
          Buffer.add_char buf '\n';
        end
      end
     done
   with End_of_file -> ());
  let oc = open_out whole_compiler_config_output in
  Buffer.output_buffer oc buf;
  close_out oc
  
let run_command_capture_stdout cmd env =
  let ic, oc, err = Unix.open_process_full cmd env in
  let buf = Buffer.create 64 in
  let bufError = Buffer.create 64 in
  (try
     while true do
       Buffer.add_channel buf ic 1;
     done
   with End_of_file -> ());
  (try
     while true do
       Buffer.add_channel bufError err 1;
     done
   with End_of_file -> ());
  let code = Unix.close_process_full (ic, oc, err) in
  (code, Buffer.contents buf, Buffer.contents bufError)

let () =
  let working_dir = Filename.dirname Sys.argv.(0) in
  let working_dir = if working_dir.[0] = Filename.dir_sep.[0] then working_dir
  else begin
    let allComponents = Ext_string.split_by (fun c -> c = Filename.dir_sep.[0]) working_dir in
    List.fold_left (fun working_dir dir -> 
      if dir = ".." then Filename.dirname working_dir 
      else if dir = "." then working_dir  
      else working_dir // dir) (Sys.getcwd ()) allComponents
  end in
  let main_bucklescript_dir = Filename.dirname working_dir in
  print "main_bucklescript_dir: %s\n" main_bucklescript_dir;
  
  let env = Unix.environment () |> Array.to_list |> List.filter (fun v -> (if String.length v >= 11 then String.sub v 0 11 <> "OCAMLPARAM=" else true)) in
  let env = Array.of_list @@ "OCAMLRUNPARAM=b" :: env in

  (* This will not work on Windows
     Windows diestribution relies on env variable OCAMLLIB and CAMLLIB
     delete process.env.OCAMLLIB
     delete process.env.CAMLLIB
  *)
  let ocamlc_path = main_bucklescript_dir // "vendor" // "ocaml" // "ocamlc.opt" in
  let cmdToRun = sp "%s -config" ocamlc_path in
  let (code, config_output, err) = run_command_capture_stdout cmdToRun env in
  match code with
  | Unix.WEXITED code when code <> 0 -> failwith err
  | Unix.WSIGNALED _  
  | Unix.WSTOPPED _ -> failwith @@ sp "Running %s was interrupted by someone" cmdToRun
  | _ -> ();
  (* print_endline ("err: " ^ err); *)
  (* print_endline ("config_output:\n" ^ config_output); *)
  
  let keyvalues = Ext_string.split_by (fun c -> c = '\n') config_output
                  |> List.filter (fun x -> String.length x > 0)
                  |> List.map (fun x ->
                    match (Ext_string.split_by (fun c -> c = ':') x) with
                    | [key; value] -> (Ext_string.trim key, Ext_string.trim value)
                    | _ -> assert false
                  ) in
  List.iter (fun (key, value) -> print_endline (key ^ ": " ^ value)) keyvalues;
  
  let should_patch = List.fold_left (fun acc (key, v) -> acc || (key = "version" && Ext_string.contain_substring v "4.02.3")) false keyvalues in
  if should_patch then begin
    print_endline @@ "should_patch: " ^ string_of_bool should_patch;
    patch_config
        (main_bucklescript_dir // "jscomp")
        keyvalues
  end;
  ignore @@ Array.map (fun i -> print "ARGGG: %s\n" i ) Sys.argv;
  let (bin_dir, ocaml_dir) = match Sys.argv with
  | [| _; share |] -> print "commandline argument: %s\n" share; (share, share)
  | _ -> (main_bucklescript_dir // "bin", main_bucklescript_dir // "vendor" // "ocaml") in
  gen_bsb_default_paths ~jscomp_dir:(main_bucklescript_dir // "jscomp") ~bin_dir ~ocaml_dir
