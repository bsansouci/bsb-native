(* build_script.exe vendor_dir ocaml_lib cwd *)

let _ = if Array.length Sys.argv != 4 then begin
  print_endline "This binary is only for bsb's internal use";
  exit 1
end

let vendor_dir = Sys.argv.(1)
let ocaml_lib = Sys.argv.(2)
let cwd = Sys.argv.(3)

let ( // ) = Filename.concat

let mingw_dir = vendor_dir // "mingw32"

let _ = Sys.chdir cwd

let _ = if Ext_sys.is_windows_or_cygwin then
  let curdir = Sys.getcwd () in
  let newCurdir = curdir // mingw_dir in
  Unix.putenv "Path" (newCurdir ^ ";" ^ Unix.getenv("Path"));
 else ()

let gcc ?c:(c=true) ?include_ocaml:(include_ocaml=true) ?flags:(flags=[]) ?includes:(includes=[]) outfile srcs = 
  let dash_c = if c then "-c" else "" in
  let all_includes = if List.length includes > 0 then 
    "-I " ^ (String.concat " -I " (List.map (fun i -> cwd // i) includes)) 
  else "" in
  let include_ocaml_flag = if include_ocaml then "-I " ^ ocaml_lib else "" in
  let compiler = if Ext_sys.is_windows_or_cygwin then "gcc.exe" else "gcc" in
  if Ext_sys.is_windows_or_cygwin then
    Sys.chdir mingw_dir;
  let cmd = compiler
    :: dash_c
    :: ("-o " ^ cwd // outfile)
    :: all_includes
    :: include_ocaml_flag
    :: (flags @ (List.map (fun s -> cwd // s) srcs)) in
  let err = Sys.command (String.concat " " cmd) in
  if Ext_sys.is_windows_or_cygwin then
    Sys.chdir cwd;
  err
