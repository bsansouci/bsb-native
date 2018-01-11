module Ocaml_parse = struct 
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

let parse_interface ppf sourcefile = 
  let ast = Pparse.parse_interface ~tool_name:Js_config.tool_name ppf sourcefile in
  ast

let lazy_parse_interface ppf sourcefile =
  lazy (parse_interface ppf sourcefile)

let parse_implementation ppf sourcefile = 
  let ast = 
    Pparse.parse_implementation ~tool_name:Js_config.tool_name ppf sourcefile in 
  ast

let parse_implementation_from_string  str = 
  let lb = Lexing.from_string str in
  Location.init lb "//toplevel//";
  let ast = Parse.implementation lb  in 
  ast


let lazy_parse_implementation ppf sourcefile =
  lazy (parse_implementation ppf sourcefile)

type valid_input = 
  | Ml 
  | Mli
  | Mlast    
  | Mliast 
  | Mlmap
  | Cmi
  
let check_suffix  name  = 
  if Ext_path.check_suffix_case name ".ml"
  || Ext_path.check_suffix_case name ".mlt" then 
    Ml,
    (** This is per-file based, 
        when [ocamlc] [-c -o another_dir/xx.cmi] 
        it will return (another_dir/xx)
    *)    
    Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name !Config.interface_suffix then 
    Mli,  Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name ".mlast" then 
    Mlast, Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name ".mliast" then 
    Mliast, Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name ".mlmap"  then 
    Mlmap, Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name ".cmi" then 
    Cmi, Compenv.output_prefix name
  else 
    raise(Arg.Bad("don't know what to do with " ^ name))

end
module Js_implementation = struct
  open Format
  open Typedtree
  open Compenv

  let fprintf = Format.fprintf



  let print_if ppf flag printer arg =
    if !flag then fprintf ppf "%a@." printer arg;
    arg



  let after_parsing_sig ppf sourcefile outputprefix ast  =
      let oc = open_out_bin (outputprefix ^ Literals.suffix_mliast_simple) in 
      Ml_binary.write_ast Mli sourcefile ast oc;
      close_out oc;
      Binary_ast.write_ast
        Mli
        ~fname:sourcefile
        ~output:(outputprefix ^ Literals.suffix_mliast)
        (* to support relocate to another directory *)
        ast 
    
  let interface ppf sourcefile outputprefix =
    Compmisc.init_path false;
    Ocaml_parse.parse_interface ppf sourcefile
    |> print_if ppf Clflags.dump_parsetree Printast.interface
    |> print_if ppf Clflags.dump_source Pprintast.signature 
    |> after_parsing_sig ppf sourcefile outputprefix 

  let interface_mliast ppf sourcefile outputprefix  = 
    Compmisc.init_path false;
    Binary_ast.read_ast Mli sourcefile 
    |> print_if ppf Clflags.dump_parsetree Printast.interface
    |> print_if ppf Clflags.dump_source Pprintast.signature 
    |> after_parsing_sig ppf sourcefile outputprefix 

  let after_parsing_impl ppf sourcefile outputprefix ast =
    let oc = open_out_bin (outputprefix ^ Literals.suffix_mlast_simple) in 
    Ml_binary.write_ast Ml sourcefile ast oc;
    close_out oc ;
    Binary_ast.write_ast ~fname:sourcefile 
      Ml ~output:(outputprefix ^ Literals.suffix_mlast)
      ast

  let implementation ppf sourcefile outputprefix =
    Compmisc.init_path false;
    Ocaml_parse.parse_implementation ppf sourcefile
    |> print_if ppf Clflags.dump_parsetree Printast.implementation
    |> print_if ppf Clflags.dump_source Pprintast.structure
    |> after_parsing_impl ppf sourcefile outputprefix 

  let implementation_mlast ppf sourcefile outputprefix = 
    Compmisc.init_path false;
    Binary_ast.read_ast Ml sourcefile
    |> print_if ppf Clflags.dump_parsetree Printast.implementation
    |> print_if ppf Clflags.dump_source Pprintast.structure
    |> after_parsing_impl ppf sourcefile outputprefix 







  let make_structure_item ~ns cunit : Parsetree.structure_item =
    let open Ast_helper in 
    let loc = Location.none in 
    Str.module_ 
      (Mb.mk {txt = cunit; loc  }
         (Mod.ident 
            {txt = Lident 
                 ( Ext_namespace.make ~ns cunit)
            ; loc}))



  let implementation_map ppf sourcefile outputprefix = 
    let list_of_modules = Ext_io.rev_lines_of_file sourcefile 
    in 
    let ns = 
      Ext_string.capitalize_ascii
        (Filename.chop_extension (Filename.basename sourcefile)) in
    let ml_ast = List.fold_left (fun acc module_name -> 
        if Ext_string.is_empty module_name then acc 
        else make_structure_item ~ns module_name :: acc 
      ) [] list_of_modules in 
    Compmisc.init_path false;
    ml_ast
    |> print_if ppf Clflags.dump_parsetree Printast.implementation
    |> print_if ppf Clflags.dump_source Pprintast.structure
    |> after_parsing_impl ppf sourcefile outputprefix 
end


let process_interface_file ppf name =
  Js_implementation.interface ppf name (Compenv.output_prefix name)
let process_implementation_file ppf name =
  Js_implementation.implementation ppf name (Compenv.output_prefix name)


let process_file ppf name = 
  match Ocaml_parse.check_suffix  name with 
  | Ml, opref ->
    Js_implementation.implementation ppf name opref 
  | Mli, opref -> 
    Js_implementation.interface ppf name opref 
  | Mliast, opref 
    -> Js_implementation.interface_mliast ppf name opref 
  | Mlast, opref 
    -> Js_implementation.implementation_mlast ppf name opref
  | Mlmap, opref 
    -> Js_implementation.implementation_map ppf name opref
  | Cmi, _ 
    ->
      failwith "Cannot call this helper on cmi files."
      
let usage = "Usage: bsc <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous filename =
  Compenv.readenv ppf (Before_compile filename); process_file ppf filename;;
let impl filename =
  Compenv.readenv ppf (Before_compile filename); process_implementation_file ppf filename;;
let intf filename =
  Compenv.readenv ppf (Before_compile filename); process_interface_file ppf filename;;


let buckle_script_flags : (string * Arg.spec * string) list = 
  Ocaml_options.mk_impl impl
  :: Ocaml_options.mk_intf intf 
  :: Ocaml_options.mk__ anonymous
  :: Ocaml_options.ocaml_options

let _ = 
  (* Default configuration: sync up with 
    {!Jsoo_main}  *)
  (* Clflags.bs_only := true;   *)
  Clflags.unsafe_string := false;
  Clflags.debug := true;
  (* Clflags.record_event_when_debug := false; *)
  Clflags.binary_annotations := true; 
  (* Oprint.out_ident := Outcome_printer_ns.out_ident; *)
  (* Bs_conditional_initial.setup_env (); *)
  try
    Compenv.readenv ppf Before_args;
    Arg.parse buckle_script_flags anonymous usage;
    (* let main_file = !main_file in
    let eval_string = !eval_string in
    let task : Ocaml_batch_compile.task = 
      if main_file <> "" then 
        Bsc_task_main main_file
      else if eval_string <> "" then 
        Bsc_task_eval eval_string
      else Bsc_task_none in
    exit (Ocaml_batch_compile.batch_compile ppf 
            (if !Clflags.no_implicit_current_dir then !script_dirs else 
               Filename.current_dir_name::!script_dirs) !batch_files task)  *)
  with x -> 
    begin
      Location.report_exception ppf x;
      exit 2
    end
