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

let main_module = ref None

let set_main_module modulename =
  main_module := Some modulename

let includes :  _ list ref = ref []

let add_include =
  let normalize cwd s =
    Ext_path.normalize_absolute_path (Ext_path.combine cwd s) in
  fun dir ->
    includes := (normalize (Sys.getcwd ()) dir) :: !includes

let batch_files = ref []
let collect_file name =
  batch_files := name :: !batch_files

(* let output_prefix = ref None *)
let dev_group = ref 0
let namespace = ref None

#if BS_NATIVE then
let verbose = ref false

let warnings = ref ""

let warn_error = ref ""

let ocaml_dependencies = ref []

let add_ocaml_dependencies s = 
  ocaml_dependencies := s :: !ocaml_dependencies

let clibs = ref []
let add_clib file = clibs := file :: !clibs 

let flags = ref []
let add_flag flag = flags := flag :: !flags 

let ocamlfind_packages = ref []

let bs_super_errors = ref false

let build_library = ref None
#end

let anonymous filename =
  collect_file filename
let usage = "Usage: bsb_helper.exe [options] \nOptions are:"
#if BS_NATIVE then
let link link_byte_or_native = 
  begin match !main_module with
  | None -> Bsb_exception.missing_main()
  | Some main_module ->
    Bsb_helper_linker.link 
      link_byte_or_native
      ~main_module:main_module
      (* `includes` is not reversed here because it gets reversed inside when we fold_list and 
          prepend a new list. *)
      ~includes:!includes
      ~batch_files:!batch_files
      ~clibs:(List.rev !clibs)
      ~flags:(List.rev !flags)
      ~ocamlfind_packages:!ocamlfind_packages
      ~bs_super_errors:!bs_super_errors
      ~namespace:!namespace
      ~ocaml_dependencies:(List.rev !ocaml_dependencies)
      ~warnings:!warnings
      ~warn_error:!warn_error
      ~verbose:!verbose
      (Sys.getcwd ())
  end
let pack link_byte_or_native =
  Bsb_helper_packer.pack
    link_byte_or_native
    ~main_module:!main_module
    ~includes:!includes
    ~batch_files:!batch_files
    ~flags:(List.rev !flags)
    ~ocamlfind_packages:!ocamlfind_packages
    ~bs_super_errors:!bs_super_errors
    ~namespace:!namespace
    ~warnings:!warnings
    ~warn_error:!warn_error
    ~verbose:!verbose
    ~build_library:!build_library
    (Sys.getcwd ())
#end

let () =
  Arg.parse [
    "-g", Arg.Int (fun i -> dev_group := i ),
    " Set the dev group (default to be 0)"
    ;
    "-MD", Arg.String (
      fun x -> 
        Bsb_helper_depfile_gen.emit_dep_file
          Js 
          x (Bsb_dir_index.of_int !dev_group )
          !namespace
          ),
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";
    "-ns", Arg.String (fun s -> namespace := Some s),
    " Set namespace";
#if BS_NATIVE then    
    "-MD-bytecode", Arg.String (
      fun x -> 
        Bsb_helper_depfile_gen.emit_dep_file
          Bytecode 
          x (Bsb_dir_index.of_int !dev_group ) !namespace),          
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";
    "-MD-native", Arg.String (fun x -> 
        Bsb_helper_depfile_gen.emit_dep_file
          Native 
           x (Bsb_dir_index.of_int !dev_group )
           !namespace
           ),
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";

    (**
       The args below are used for packing/linking.

       This makes bsb_helper act as an ocaml linker where we automatically figure
       out the dependencies graph to do a topological sort before calling 
       ocamlc/ocamlopt.
    *)
    "-bs-main", (Arg.String set_main_module),
    " set the main entry module. Only used in conjunction with -link-bytecode and -link-native";

    (* This is a way to add a directory to the search path. This is used for the 
       compiler to look for cmi files. It's also used to look for a file called `lib.cma` to 
       link with the current executable.

       For example if called like so

          bsb_helper -I theExtLib myMainFile.cmo -link-bytecode

       Then we'll go look for `theExtLib/lib.cma` to link with the final exec.
    *)
    "-I",  (Arg.String add_include),
    " add dir to search path for the linker and packer";

    (* Both linking and packing arguments must come _after_ all of the other args and files have been listed.
       For example:

          bsb_helper -main-module MyModule myFile.cmo myOtherFile.cmo -link-bytecode 

       In the following example, the file called `myIgnoredFile.cmo` is not linked nor is `myLibFolder/lib.cma`

          bsb_helper -main-module MyModule myFile.cmo myOtherFile.cmo -link-bytecode -I myLibFolder myIgnoredFile.cmo

    *)
    "-link-bytecode", (Arg.String (fun x -> link (Bsb_helper_linker.LinkBytecode x))),
    " link bytecode files into an executable";

    "-link-native", (Arg.String (fun x -> link (Bsb_helper_linker.LinkNative x))),
    " link native files into an executable";

    "-package", (Arg.String (fun x -> ocamlfind_packages := "-package" :: x :: !ocamlfind_packages)),
    " add an ocamlfind pacakge to be packed/linked";
    
    "-pack-native-library", (Arg.Unit (fun () -> 
      pack Bsb_helper_packer.PackNative
        
    )),
    " pack native files (cmx) into a library file (cmxa)";

    "-pack-bytecode-library", (Arg.Unit (fun () -> 
      pack Bsb_helper_packer.PackBytecode
    )),
    " pack bytecode files (cmo) into a library file (cma)";

    "-bs-super-errors", (Arg.Unit (fun () -> bs_super_errors := true)),
    " Better error message combined with other tools ";
    
    "-add-clib", (Arg.String add_clib),
    " adds a .a library file to be linked into the final executable";
    
    "-add-flag", (Arg.String add_flag),
    " passes flag to underlaying ocaml compiler for packing/linking phase.";
    
    "-add-ocaml-dependency", (Arg.String add_ocaml_dependencies),
    " Add a dependency on otherlibs or compiler-libs.";
    
    "-w", (Arg.String (fun w -> warnings := w )),
    " Use warnings for packer/linker.";
    
    "-warn-error", (Arg.String (fun w -> warn_error := w )),
    " Turn warnings into errors for packer/linker.";
    
    "-verbose", (Arg.Unit (fun v -> verbose := true)),
    " Turn on verbose Maude.";
    
    "-build-library", (Arg.String (fun v -> build_library := Some v)),
    " Create a library file with all the object files from the given entry point."
#end
  ] anonymous usage
