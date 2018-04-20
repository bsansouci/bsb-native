module Config_whole_compiler : sig 
#1 "config_whole_compiler.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* System configuration *)

val version: string
        (* The current version number of the system *)

val standard_library: string
        (* The directory containing the standard libraries *)
val standard_runtime: string
        (* The full path to the standard bytecode interpreter ocamlrun *)
val ccomp_type: string
        (* The "kind" of the C compiler, assembler and linker used: one of
               "cc" (for Unix-style C compilers)
               "msvc" (for Microsoft Visual C++ and MASM) *)
val bytecomp_c_compiler: string
        (* The C compiler to use for compiling C files
           with the bytecode compiler *)
val bytecomp_c_libraries: string
        (* The C libraries to link with custom runtimes *)
val native_c_compiler: string
        (* The C compiler to use for compiling C files
           with the native-code compiler *)
val native_c_libraries: string
        (* The C libraries to link with native-code programs *)
val native_pack_linker: string
        (* The linker to use for packaging (ocamlopt -pack) and for partial
           links (ocamlopt -output-obj). *)
val mkdll: string
        (* The linker command line to build dynamic libraries. *)
val mkexe: string
        (* The linker command line to build executables. *)
val mkmaindll: string
        (* The linker command line to build main programs as dlls. *)
val ranlib: string
        (* Command to randomize a library, or "" if not needed *)
val ar: string
        (* Name of the ar command, or "" if not needed  (MSVC) *)
val cc_profile : string
        (* The command line option to the C compiler to enable profiling. *)

val load_path: string list ref
        (* Directories in the search path for .cmi and .cmo files *)

val interface_suffix: string ref
        (* Suffix for interface file names *)

val exec_magic_number: string
        (* Magic number for bytecode executable files *)
val cmi_magic_number: string
        (* Magic number for compiled interface files *)
val cmo_magic_number: string
        (* Magic number for object bytecode files *)
val cma_magic_number: string
        (* Magic number for archive files *)
val cmx_magic_number: string
        (* Magic number for compilation unit descriptions *)
val cmxa_magic_number: string
        (* Magic number for libraries of compilation unit descriptions *)
val ast_intf_magic_number: string
        (* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number: string
        (* Magic number for file holding an implementation syntax tree *)
val cmxs_magic_number: string
        (* Magic number for dynamically-loadable plugins *)
val cmt_magic_number: string
        (* Magic number for compiled interface files *)

val max_tag: int
        (* Biggest tag that can be stored in the header of a regular block. *)
val lazy_tag : int
        (* Normally the same as Obj.lazy_tag.  Separate definition because
           of technical reasons for bootstrapping. *)
val max_young_wosize: int
        (* Maximal size of arrays that are directly allocated in the
           minor heap *)
val stack_threshold: int
        (* Size in words of safe area at bottom of VM stack,
           see byterun/config.h *)

val architecture: string
        (* Name of processor type for the native-code compiler *)
val model: string
        (* Name of processor submodel for the native-code compiler *)
(* val system: string *)
        (* Name of operating system for the native-code compiler *)

val asm: string
        (* The assembler (and flags) to use for assembling
           ocamlopt-generated code. *)

val asm_cfi_supported: bool
        (* Whether assembler understands CFI directives *)
val with_frame_pointers : bool
        (* Whether assembler should maintain frame pointers *)

val ext_obj: string
        (* Extension for object files, e.g. [.o] under Unix. *)
val ext_asm: string
        (* Extension for assembler files, e.g. [.s] under Unix. *)
val ext_lib: string
        (* Extension for library files, e.g. [.a] under Unix. *)
val ext_dll: string
        (* Extension for dynamically-loaded libraries, e.g. [.so] under Unix.*)

val default_executable_name: string
        (* Name of executable produced by linking if none is given with -o,
           e.g. [a.out] under Unix. *)

val systhread_supported : bool
        (* Whether the system thread library is implemented *)

(* val host : string *)
        (* Whether the compiler is a cross-compiler *)

(* val target : string *)
        (* Whether the compiler is a cross-compiler *)

val print_config : out_channel -> unit;;


end = struct
#1 "config_whole_compiler.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)


(* The main OCaml version string has moved to ../VERSION *)
let version = "4.02.3+BS"
let standard_library =
  Filename.concat (Filename.dirname Sys.executable_name)  "ocaml"
let standard_library_default = standard_library

let standard_runtime = "ocamlrun" (*dont care:path to ocamlrun*)
let ccomp_type = "cc"(*dont care: cc or msvc*)
let bytecomp_c_compiler = "gcc -O  -Wall -D_FILE_OFFSET_BITS=64 -D_REENTRANT -O" (*dont care*)
let bytecomp_c_libraries = "-lcurses -lpthread" (*dont care*)
let native_c_compiler = "gcc -O  -D_FILE_OFFSET_BITS=64 -D_REENTRANT" (*dont care*)
let native_c_libraries = ""(*dont care*)
let native_pack_linker = "ld -r -arch x86_64  -o"(*dont care*) 
let ranlib = "ranlib"(*dont care*)
let ar = ""(*dont care*)
let cc_profile = "-pg"(*dont care*)
let mkdll = ""(*dont care*)
let mkexe = ""(*dont care*)
let mkmaindll = ""(*dont care*)

let exec_magic_number = "Caml1999X011"
and cmi_magic_number = "Caml1999I017"
and cmo_magic_number = "Caml1999O010"
and cma_magic_number = "Caml1999A011"
and cmx_magic_number = "Caml1999Y014"
and cmxa_magic_number = "Caml1999Z013"
and ast_impl_magic_number = "Caml1999M016"
and ast_intf_magic_number = "Caml1999N015"
and cmxs_magic_number = "Caml2007D002"
and cmt_magic_number = "Caml2012T004"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 245
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 256 (* see byterun/config.h *)

let architecture = "amd64" (*dont care*)
let model = "default"(*dont care*)
let system = "macosx"

let asm = "clang -arch x86_64 -c"
let asm_cfi_supported = false (*dont care*)
let with_frame_pointers = false (*dontcare*)

let ext_obj = ".o" (*dont care*)
let ext_asm = ".s" (*dont care*)
let ext_lib = ".a" (*dont caer*)
let ext_dll = ".a" (*dont care*)

let host = "%%HOST%%"
let target = "%%TARGET%%"

let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"

let systhread_supported = false (*dontcare*);;

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  let p_bool name valu = Printf.fprintf oc "%s: %B\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  p "standard_runtime" standard_runtime;
  p "ccomp_type" ccomp_type;
  p "bytecomp_c_compiler" bytecomp_c_compiler;
  p "bytecomp_c_libraries" bytecomp_c_libraries;
  p "native_c_compiler" native_c_compiler;
  p "native_c_libraries" native_c_libraries;
  p "native_pack_linker" native_pack_linker;
  p "ranlib" ranlib;
  p "cc_profile" cc_profile;
  p "architecture" architecture;
  p "model" model;
  p "system" system;
  p "asm" asm;
  p_bool "asm_cfi_supported" asm_cfi_supported;
  p_bool "with_frame_pointers" with_frame_pointers;
  p "ext_obj" ext_obj;
  p "ext_asm" ext_asm;
  p "ext_lib" ext_lib;
  p "ext_dll" ext_dll;
  p "os_type" Sys.os_type;
  p "default_executable_name" default_executable_name;
  p_bool "systhread_supported" systhread_supported;
  p "host" host;
  p "target" target;

  (* print the magic number *)
  p "exec_magic_number" exec_magic_number;
  p "cmi_magic_number" cmi_magic_number;
  p "cmo_magic_number" cmo_magic_number;
  p "cma_magic_number" cma_magic_number;
  p "cmx_magic_number" cmx_magic_number;
  p "cmxa_magic_number" cmxa_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmxs_magic_number" cmxs_magic_number;
  p "cmt_magic_number" cmt_magic_number;

  flush oc;
;;


end
module Config = Config_whole_compiler 
module Clflags : sig 
#1 "clflags.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val compile_only : bool ref
val output_name : string option ref
val include_dirs : string list ref
val no_std_include : bool ref
val print_types : bool ref
val make_archive : bool ref
val debug : bool ref
val fast : bool ref
val link_everything : bool ref
val custom_runtime : bool ref
val no_check_prims : bool ref
val bytecode_compatible_32 : bool ref
val output_c_object : bool ref
val output_complete_object : bool ref
val all_ccopts : string list ref
val classic : bool ref
val nopervasives : bool ref
val open_modules : string list ref
val preprocessor : string option ref
val all_ppx : string list ref
val annotations : bool ref
val binary_annotations : bool ref
val use_threads : bool ref
val use_vmthreads : bool ref
val noassert : bool ref
val verbose : bool ref
val noprompt : bool ref
val nopromptcont : bool ref
val init_file : string option ref
val noinit : bool ref
val use_prims : string ref
val use_runtime : string ref
val principal : bool ref
val real_paths : bool ref
val recursive_types : bool ref
val strict_sequence : bool ref
val strict_formats : bool ref
val applicative_functors : bool ref
val make_runtime : bool ref
val gprofile : bool ref
val c_compiler : string option ref
val no_auto_link : bool ref
val dllpaths : string list ref
val make_package : bool ref
val for_package : string option ref
val error_size : int ref
val float_const_prop : bool ref
val transparent_modules : bool ref
val dump_source : bool ref
val dump_parsetree : bool ref
val dump_typedtree : bool ref
val dump_rawlambda : bool ref
val dump_lambda : bool ref
val dump_clambda : bool ref
val dump_instr : bool ref
val keep_asm_file : bool ref
val optimize_for_speed : bool ref
val dump_cmm : bool ref
val dump_selection : bool ref
val dump_cse : bool ref
val dump_live : bool ref
val dump_spill : bool ref
val dump_split : bool ref
val dump_interf : bool ref
val dump_prefer : bool ref
val dump_regalloc : bool ref
val dump_reload : bool ref
val dump_scheduling : bool ref
val dump_linear : bool ref
val keep_startup_file : bool ref
val dump_combine : bool ref
val native_code : bool ref
val inline_threshold : int ref
val dont_write_files : bool ref
val std_include_flag : string -> string
val std_include_dir : unit -> string list
val shared : bool ref
val dlcode : bool ref
val runtime_variant : string ref
val force_slash : bool ref
val keep_docs : bool ref
val keep_locs : bool ref
val unsafe_string : bool ref
val opaque : bool ref


 
type mli_status = Mli_na | Mli_exists | Mli_non_exists
val no_implicit_current_dir : bool ref
val assume_no_mli : mli_status ref 
val record_event_when_debug : bool ref 
val bs_vscode : bool
val dont_record_crc_unit : string option ref
val bs_only : bool ref (* set true on bs top*)
val no_assert_false : bool ref


type color_setting = Auto | Always | Never
val parse_color_setting : string -> color_setting option
val color : color_setting ref


end = struct
#1 "clflags.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Command-line parameters *)

let objfiles = ref ([] : string list)   (* .cmo and .cma files *)
and ccobjs = ref ([] : string list)     (* .o, .a, .so and -cclib -lxxx *)
and dllibs = ref ([] : string list)     (* .so and -dllib -lxxx *)

let compile_only = ref false            (* -c *)
and output_name = ref (None : string option) (* -o *)
and include_dirs = ref ([] : string list)(* -I *)
and no_std_include = ref false          (* -nostdlib *)
and print_types = ref false             (* -i *)
and make_archive = ref false            (* -a *)
and debug = ref false                   (* -g *)
and fast = ref false                    (* -unsafe *)
and link_everything = ref false         (* -linkall *)
and custom_runtime = ref false          (* -custom *)
and no_check_prims = ref false          (* -no-check-prims *)
and bytecode_compatible_32 = ref false  (* -compat-32 *)
and output_c_object = ref false         (* -output-obj *)
and output_complete_object = ref false  (* -output-complete-obj *)
and all_ccopts = ref ([] : string list)     (* -ccopt *)
and classic = ref false                 (* -nolabels *)
and nopervasives = ref false            (* -nopervasives *)
and preprocessor = ref(None : string option) (* -pp *)
and all_ppx = ref ([] : string list)        (* -ppx *)
let annotations = ref false             (* -annot *)
let binary_annotations = ref false      (* -annot *)
and use_threads = ref false             (* -thread *)
and use_vmthreads = ref false           (* -vmthread *)
and noassert = ref false                (* -noassert *)
and verbose = ref false                 (* -verbose *)
and noprompt = ref false                (* -noprompt *)
and nopromptcont = ref false            (* -nopromptcont *)
and init_file = ref (None : string option)   (* -init *)
and noinit = ref false                  (* -noinit *)
and open_modules = ref []               (* -open *)
and use_prims = ref ""                  (* -use-prims ... *)
and use_runtime = ref ""                (* -use-runtime ... *)
and principal = ref false               (* -principal *)
and real_paths = ref true               (* -short-paths *)
and recursive_types = ref false         (* -rectypes *)
and strict_sequence = ref false         (* -strict-sequence *)
and strict_formats = ref false          (* -strict-formats *)
and applicative_functors = ref true     (* -no-app-funct *)
and make_runtime = ref false            (* -make-runtime *)
and gprofile = ref false                (* -p *)
and c_compiler = ref (None: string option) (* -cc *)
and no_auto_link = ref false            (* -noautolink *)
and dllpaths = ref ([] : string list)   (* -dllpath *)
and make_package = ref false            (* -pack *)
and for_package = ref (None: string option) (* -for-pack *)
and error_size = ref 500                (* -error-size *)
and float_const_prop = ref true         (* -no-float-const-prop *)
and transparent_modules = ref false     (* -trans-mod *)
let dump_source = ref false             (* -dsource *)
let dump_parsetree = ref false          (* -dparsetree *)
and dump_typedtree = ref false          (* -dtypedtree *)
and dump_rawlambda = ref false          (* -drawlambda *)
and dump_lambda = ref false             (* -dlambda *)
and dump_clambda = ref false            (* -dclambda *)
and dump_instr = ref false              (* -dinstr *)

let keep_asm_file = ref false           (* -S *)
let optimize_for_speed = ref true       (* -compact *)
and opaque = ref false                  (* -opaque *)

and dump_cmm = ref false                (* -dcmm *)
let dump_selection = ref false          (* -dsel *)
let dump_cse = ref false                (* -dcse *)
let dump_live = ref false               (* -dlive *)
let dump_spill = ref false              (* -dspill *)
let dump_split = ref false              (* -dsplit *)
let dump_interf = ref false             (* -dinterf *)
let dump_prefer = ref false             (* -dprefer *)
let dump_regalloc = ref false           (* -dalloc *)
let dump_reload = ref false             (* -dreload *)
let dump_scheduling = ref false         (* -dscheduling *)
let dump_linear = ref false             (* -dlinear *)
let keep_startup_file = ref false       (* -dstartup *)
let dump_combine = ref false            (* -dcombine *)
let native_code = ref false             (* set to true under ocamlopt *)
let inline_threshold = ref 10
let force_slash = ref false             (* for ocamldep *)

let dont_write_files = ref false        (* set to true under ocamldoc *)

let std_include_flag prefix =
  if !no_std_include then ""
  else (prefix ^ (Filename.quote Config.standard_library))
;;

let std_include_dir () =
  if !no_std_include then [] else [Config.standard_library]
;;

let shared = ref false (* -shared *)
let dlcode = ref true (* not -nodynlink *)

let runtime_variant = ref "";;      (* -runtime-variant *)

let keep_docs = ref false              (* -keep-docs *)
let keep_locs = ref false              (* -keep-locs *)
let unsafe_string = ref true;;         (* -safe-string / -unsafe-string *)


 
type mli_status = Mli_na | Mli_exists | Mli_non_exists
let no_implicit_current_dir = ref false
let assume_no_mli = ref Mli_na
let record_event_when_debug = ref true (* turned off in BuckleScript*)
let bs_vscode = 
    try ignore @@ Sys.getenv "BS_VSCODE" ; true with _ -> false
    (* We get it from environment variable mostly due to 
       we don't want to rebuild when flip on or off
    *)
let dont_record_crc_unit : string option ref = ref None
let bs_only = ref false
let no_assert_false = ref false


type color_setting = Auto | Always | Never
let parse_color_setting = function
  | "auto" -> Some Auto
  | "always" -> Some Always
  | "never" -> Some Never
  | _ -> None
let color = ref Auto ;; (* -color *)


end
module Misc : sig 
#1 "misc.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Miscellaneous useful types and functions *)

val fatal_error: string -> 'a
exception Fatal_error

val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a;;

val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
        (* [map_end f l t] is [map f l @ t], just more efficient. *)
val map_left_right: ('a -> 'b) -> 'a list -> 'b list
        (* Like [List.map], with guaranteed left-to-right evaluation order *)
val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [List.for_all] but for a binary predicate.
           In addition, this [for_all2] never fails: given two lists
           with different lengths, it returns false. *)
val replicate_list: 'a -> int -> 'a list
        (* [replicate_list elem n] is the list with [n] elements
           all identical to [elem]. *)
val list_remove: 'a -> 'a list -> 'a list
        (* [list_remove x l] returns a copy of [l] with the first
           element equal to [x] removed. *)
val split_last: 'a list -> 'a list * 'a
        (* Return the last element and the other elements of the given list. *)
val samelist: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
        (* Like [List.for_all2] but returns [false] if the two
           lists have different length. *)

val may: ('a -> unit) -> 'a option -> unit
val may_map: ('a -> 'b) -> 'a option -> 'b option

val find_in_path: string list -> string -> string
        (* Search a file in a list of directories. *)
val find_in_path_rel: string list -> string -> string
        (* Search a relative file in a list of directories. *)
val find_in_path_uncap: string list -> string -> string
        (* Same, but search also for uncapitalized name, i.e.
           if name is Foo.ml, allow /path/Foo.ml and /path/foo.ml
           to match. *)
val remove_file: string -> unit
        (* Delete the given file if it exists. Never raise an error. *)
val expand_directory: string -> string -> string
        (* [expand_directory alt file] eventually expands a [+] at the
           beginning of file into [alt] (an alternate root directory) *)

val create_hashtable: int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
        (* Create a hashtable of the given size and fills it with the
           given bindings. *)

val copy_file: in_channel -> out_channel -> unit
        (* [copy_file ic oc] reads the contents of file [ic] and copies
           them to [oc]. It stops when encountering EOF on [ic]. *)
val copy_file_chunk: in_channel -> out_channel -> int -> unit
        (* [copy_file_chunk ic oc n] reads [n] bytes from [ic] and copies
           them to [oc]. It raises [End_of_file] when encountering
           EOF on [ic]. *)
val string_of_file: in_channel -> string
        (* [string_of_file ic] reads the contents of file [ic] and copies
           them to a string. It stops when encountering EOF on [ic]. *)
val log2: int -> int
        (* [log2 n] returns [s] such that [n = 1 lsl s]
           if [n] is a power of 2*)
val align: int -> int -> int
        (* [align n a] rounds [n] upwards to a multiple of [a]
           (a power of 2). *)
val no_overflow_add: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 + n2] does not overflow. *)
val no_overflow_sub: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
val no_overflow_lsl: int -> bool
        (* [no_overflow_add n] returns [true] if the computation of
           [n lsl 1] does not overflow. *)

val chop_extension_if_any: string -> string
        (* Like Filename.chop_extension but returns the initial file
           name if it has no extension *)

val chop_extensions: string -> string
        (* Return the given file name without its extensions. The extensions
           is the longest suffix starting with a period and not including
           a directory separator, [.xyz.uvw] for instance.

           Return the given name if it does not contain an extension. *)

val search_substring: string -> string -> int -> int
        (* [search_substring pat str start] returns the position of the first
           occurrence of string [pat] in string [str].  Search starts
           at offset [start] in [str].  Raise [Not_found] if [pat]
           does not occur. *)

val replace_substring: before:string -> after:string -> string -> string
        (* [search_substring ~before ~after str] replaces all occurences
           of [before] with [after] in [str] and returns the resulting string. *)

val rev_split_words: string -> string list
        (* [rev_split_words s] splits [s] in blank-separated words, and return
           the list of words in reverse order. *)

val get_ref: 'a list ref -> 'a list
        (* [get_ref lr] returns the content of the list reference [lr] and reset
           its content to the empty list. *)


val fst3: 'a * 'b * 'c -> 'a
val snd3: 'a * 'b * 'c -> 'b
val thd3: 'a * 'b * 'c -> 'c

val fst4: 'a * 'b * 'c * 'd -> 'a
val snd4: 'a * 'b * 'c * 'd -> 'b
val thd4: 'a * 'b * 'c * 'd -> 'c
val for4: 'a * 'b * 'c * 'd -> 'd

module LongString :
  sig
    type t = bytes array
    val create : int -> t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val blit : t -> int -> t -> int -> int -> unit
    val output : out_channel -> t -> int -> int -> unit
    val unsafe_blit_to_bytes : t -> int -> bytes -> int -> int -> unit
    val input_bytes : in_channel -> int -> t
  end

val edit_distance : string -> string -> int -> int option
(** [edit_distance a b cutoff] computes the edit distance between
    strings [a] and [b]. To help efficiency, it uses a cutoff: if the
    distance [d] is smaller than [cutoff], it returns [Some d], else
    [None].

    The distance algorithm currently used is Damerau-Levenshtein: it
    computes the number of insertion, deletion, substitution of
    letters, or swapping of adjacent letters to go from one word to the
    other. The particular algorithm may change in the future.
*)

val split : string -> char -> string list
(** [String.split string char] splits the string [string] at every char
    [char], and returns the list of sub-strings between the chars.
    [String.concat (String.make 1 c) (String.split s c)] is the identity.
    @since 4.01
 *)

val cut_at : string -> char -> string * string
(** [String.cut_at s c] returns a pair containing the sub-string before
   the first occurrence of [c] in [s], and the sub-string after the
   first occurrence of [c] in [s].
   [let (before, after) = String.cut_at s c in
    before ^ String.make 1 c ^ after] is the identity if [s] contains [c].

   Raise [Not_found] if the character does not appear in the string
   @since 4.01
*)





(* Color handling *)
module Color : sig
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
  ;;

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

    | Dim


  val ansi_of_style_l : style list -> string
  (* ANSI escape sequence for the given style *)

  type styles = {
    error: style list;
    warning: style list;
    loc: style list;
  }

  val default_styles: styles
  val get_styles: unit -> styles
  val set_styles: styles -> unit

  val setup : Clflags.color_setting -> unit
  (* [setup opt] will enable or disable color handling on standard formatters
     according to the value of color setting [opt].
     Only the first call to this function has an effect. *)

  val set_color_tag_handling : Format.formatter -> unit
  (* adds functions to support color tags to the given formatter. *)
end


end = struct
#1 "misc.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

(* Exceptions *)

let try_finally work cleanup =
  let result = (try work () with e -> cleanup (); raise e) in
  cleanup ();
  result
;;

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

let rec list_remove x = function
    [] -> []
  | hd :: tl ->
      if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

let rec samelist pred l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (hd1 :: tl1, hd2 :: tl2) -> pred hd1 hd2 && samelist pred tl1 tl2
  | (_, _) -> false

(* Options *)

let may f = function
    Some x -> f x
  | None -> ()

let may_map f = function
    Some x -> Some (f x)
  | None -> None

(* File functions *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = simplify (Filename.concat dir name) in
      if Sys.file_exists fullname then fullname else try_dir rem
  in try_dir path

let find_in_path_uncap path name =
  let uname = String.uncapitalize name in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

let no_overflow_lsl a = min_int asr 1 <= a && a <= max_int asr 1

(* String operations *)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str curr (next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str curr (String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat after (search [] 0)

let rev_split_words s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      match s.[i] with
        ' ' | '\t' | '\r' | '\n' -> split1 res (i+1)
      | _ -> split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      match s.[j] with
        ' ' | '\t' | '\r' | '\n' -> split1 (String.sub s i (j-i) :: res) (j+1)
      | _ -> split2 res i (j+1)
    end
  in split1 [] 0

let get_ref r =
  let v = !r in
  r := []; v

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x


module LongString = struct
  type t = bytes array

  let create str_size =
    let tbl_size = str_size / Sys.max_string_length + 1 in
    let tbl = Array.make tbl_size Bytes.empty in
    for i = 0 to tbl_size - 2 do
      tbl.(i) <- Bytes.create Sys.max_string_length;
    done;
    tbl.(tbl_size - 1) <- Bytes.create (str_size mod Sys.max_string_length);
    tbl

  let length tbl =
    let tbl_size = Array.length tbl in
    Sys.max_string_length * (tbl_size - 1) + Bytes.length tbl.(tbl_size - 1)

  let get tbl ind =
    Bytes.get tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)

  let set tbl ind c =
    Bytes.set tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)
              c

  let blit src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      set dst (dstoff + i) (get src (srcoff + i))
    done

  let output oc tbl pos len =
    for i = pos to pos + len - 1 do
      output_char oc (get tbl i)
    done

  let unsafe_blit_to_bytes src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      Bytes.unsafe_set dst (dstoff + i) (get src (srcoff + i))
    done

  let input_bytes ic len =
    let tbl = create len in
    Array.iter (fun str -> really_input ic str 0 (Bytes.length str)) tbl;
    tbl
end


let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    min (max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = max 1 (i - cutoff - 1) to min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          min (1 + min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end


(* split a string [s] at every char [c], and return the list of sub-strings *)
let split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)





(* Color handling *)
module Color = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
  ;;

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

    | Dim


  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"

    | Dim -> "2"


  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"

  type styles = {
    error: style list;
    warning: style list;
    loc: style list;
  }

  let default_styles = {
    warning = [Bold; FG Magenta];
    error = [Bold; FG Red];
    loc = [Bold];
  }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | "error" -> (!cur_styles).error
    | "warning" -> (!cur_styles).warning
    | "loc" -> (!cur_styles).loc

    | "info" -> [Bold; FG Yellow]
    | "dim" -> [Dim]
    | "filename" -> [FG Cyan]

    | _ -> raise Not_found

  let color_enabled = ref true

  (* either prints the tag of [s] or delegate to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !color_enabled then ansi_of_style_l style else ""
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let _ = style_of_tag s in
      if !color_enabled then ansi_of_style_l [Reset] else ""
    with Not_found -> or_else s

  (* add color handling to formatter [ppf] *)
  let set_color_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_tag_functions ppf () in
    let functions' = {functions with
      mark_open_tag=(mark_open_tag ~or_else:functions.mark_open_tag);
      mark_close_tag=(mark_close_tag ~or_else:functions.mark_close_tag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_tag_functions ppf functions'

  (* external isatty : out_channel -> bool = "caml_sys_isatty" *)

  (* reasonable heuristic on whether colors should be enabled *)
   let should_enable_color () = false  
(*    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> "" *)
(*    && isatty stderr *)

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l = [Format.std_formatter; Format.err_formatter; Format.str_formatter] in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_color_tag_handling formatter_l;
        color_enabled := (match o with
          | Clflags.Always -> true
          | Clflags.Auto -> should_enable_color ()
          | Clflags.Never -> false
        )
      );
      ()
end


end
module Terminfo : sig 
#1 "terminfo.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int  (* number of lines of the terminal *)
;;
external setup : out_channel -> status = "caml_terminfo_setup";;
external backup : int -> unit = "caml_terminfo_backup";;
external standout : bool -> unit = "caml_terminfo_standout";;
external resume : int -> unit = "caml_terminfo_resume";;

end = struct
#1 "terminfo.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int
;;
external setup : out_channel -> status = "caml_terminfo_setup";;
external backup : int -> unit = "caml_terminfo_backup";;
external standout : bool -> unit = "caml_terminfo_standout";;
external resume : int -> unit = "caml_terminfo_resume";;

end
module Warnings : sig 
#1 "warnings.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
  | Deprecated of string                    (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Partial_application                     (*  5 *)
  | Labels_omitted                          (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Non_closed_record_pattern of string     (*  9 *)
  | Statement_type                          (* 10 *)
  | Unused_match                            (* 11 *)
  | Unused_pat                              (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of string                 (* 18 *)
  | Without_principality of string          (* 19 *)
  | Unused_argument                         (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (* 30 *)
  | Multiple_definition of string * string * string (* 31 *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * bool * bool (* 37 *)
  | Unused_extension of string * bool * bool   (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool   (* 40 *)
  | Ambiguous_name of string list * string list * bool (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string                   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)

  | Bs_unused_attribute of string           (* 101 *)
  | Bs_polymorphic_comparison               (* 102 *)
  | Bs_ffi_warning of string                (* 103 *)
  | Bs_derive_warning of string             (* 104 *)
;;

val parse_options : bool -> string -> unit;;

val is_active : t -> bool;;
val is_error : t -> bool;;

val defaults_w : string;;
val defaults_warn_error : string;;

val print : formatter -> t -> unit;;

exception Errors of int;;

val check_fatal : unit -> unit;;

val help_warnings: unit -> unit

type state
val backup: unit -> state
val restore: state -> unit


val message : t -> string 
val number: t -> int
val super_print : (t -> string) -> formatter -> t -> unit;;


end = struct
#1 "warnings.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* When you change this, you need to update the documentation:
   - man/ocamlc.m   in ocaml
   - man/ocamlopt.m in ocaml
   - manual/cmds/comp.etex   in the doc sources
   - manual/cmds/native.etex in the doc sources
*)

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
  | Deprecated of string                    (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Partial_application                     (*  5 *)
  | Labels_omitted                          (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Non_closed_record_pattern of string     (*  9 *)
  | Statement_type                          (* 10 *)
  | Unused_match                            (* 11 *)
  | Unused_pat                              (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of string                 (* 18 *)
  | Without_principality of string          (* 19 *)
  | Unused_argument                         (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (*30 *)
  | Multiple_definition of string * string * string (* 31 *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * bool * bool  (* 37 *)
  | Unused_extension of string * bool * bool    (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool (* 40 *)
  | Ambiguous_name of string list * string list *  bool    (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string                   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)

  | Bs_unused_attribute of string           (* 101 *)
  | Bs_polymorphic_comparison               (* 102 *)
  | Bs_ffi_warning of string                (* 103 *)
  | Bs_derive_warning of string             (* 104 *)
;;

(* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*)

let number = function
  | Comment_start -> 1
  | Comment_not_end -> 2
  | Deprecated _ -> 3
  | Fragile_match _ -> 4
  | Partial_application -> 5
  | Labels_omitted -> 6
  | Method_override _ -> 7
  | Partial_match _ -> 8
  | Non_closed_record_pattern _ -> 9
  | Statement_type -> 10
  | Unused_match -> 11
  | Unused_pat -> 12
  | Instance_variable_override _ -> 13
  | Illegal_backslash -> 14
  | Implicit_public_methods _ -> 15
  | Unerasable_optional_argument -> 16
  | Undeclared_virtual_method _ -> 17
  | Not_principal _ -> 18
  | Without_principality _ -> 19
  | Unused_argument -> 20
  | Nonreturning_statement -> 21
  | Preprocessor _ -> 22
  | Useless_record_with -> 23
  | Bad_module_name _ -> 24
  | All_clauses_guarded -> 25
  | Unused_var _ -> 26
  | Unused_var_strict _ -> 27
  | Wildcard_arg_to_constant_constr -> 28
  | Eol_in_string -> 29
  | Duplicate_definitions _ -> 30
  | Multiple_definition _ -> 31
  | Unused_value_declaration _ -> 32
  | Unused_open _ -> 33
  | Unused_type_declaration _ -> 34
  | Unused_for_index _ -> 35
  | Unused_ancestor _ -> 36
  | Unused_constructor _ -> 37
  | Unused_extension _ -> 38
  | Unused_rec_flag -> 39
  | Name_out_of_scope _ -> 40
  | Ambiguous_name _ -> 41
  | Disambiguated_name _ -> 42
  | Nonoptional_label _ -> 43
  | Open_shadow_identifier _ -> 44
  | Open_shadow_label_constructor _ -> 45
  | Bad_env_variable _ -> 46
  | Attribute_payload _ -> 47
  | Eliminated_optional_arguments _ -> 48
  | No_cmi_file _ -> 49
  | Bad_docstring _ -> 50

  | Bs_unused_attribute _ -> 101
  | Bs_polymorphic_comparison -> 102
  | Bs_ffi_warning _ -> 103
  | Bs_derive_warning _ -> 104
;;

let last_warning_number = 104
(* Must be the max number returned by the [number] function. *)
let letter_all = 
  let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
  loop last_warning_number

let letter = function
  | 'a' ->
    letter_all
  | 'b' -> []
  | 'c' -> [1; 2]
  | 'd' -> [3]
  | 'e' -> [4]
  | 'f' -> [5]
  | 'g' -> []
  | 'h' -> []
  | 'i' -> []
  | 'j' -> []
  | 'k' -> [32; 33; 34; 35; 36; 37; 38; 39]
  | 'l' -> [6]
  | 'm' -> [7]
  | 'n' -> []
  | 'o' -> []
  | 'p' -> [8]
  | 'q' -> []
  | 'r' -> [9]
  | 's' -> [10]
  | 't' -> []
  | 'u' -> [11; 12]
  | 'v' -> [13]
  | 'w' -> []
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 30]
  | 'y' -> [26]
  | 'z' -> [27]
  | _ -> assert false
;;

type state =
  {
    active: bool array;
    error: bool array;
  }

let current =
  ref
    {
      active = Array.make (last_warning_number + 1) true;
      error = Array.make (last_warning_number + 1) false;
    }

let backup () = !current

let restore x = current := x

let is_active x = (!current).active.(number x);;
let is_error x = (!current).error.(number x);;

let parse_opt error active flags s =
  let set i = flags.(i) <- true in
  let clear i = flags.(i) <- false in
  let set_all i = active.(i) <- true; error.(i) <- true in
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
    | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
    | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop i =
    if i >= String.length s then () else
    match s.[i] with
    | 'A' .. 'Z' ->
       List.iter set (letter (Char.lowercase s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter clear (letter s.[i]);
       loop (i+1)
    | '+' -> loop_letter_num set (i+1)
    | '-' -> loop_letter_num clear (i+1)
    | '@' -> loop_letter_num set_all (i+1)
    | c -> error ()
  and loop_letter_num myset i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        for n = n1 to min n2 last_warning_number do myset n done;
        loop i
    | 'A' .. 'Z' ->
       List.iter myset (letter (Char.lowercase s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter myset (letter s.[i]);
       loop (i+1)
    | _ -> error ()
  in
  loop 0
;;

let parse_options errflag s =
  let error = Array.copy (!current).error in
  let active = Array.copy (!current).active in
  parse_opt error active (if errflag then error else active) s;
  current := {error; active}

(* If you change these, don't forget to change them in man/ocamlc.m *)
let defaults_w = "+a-4-6-7-9-27-29-32..39-41..42-44-45-48-50-102";;
let defaults_warn_error = "-a";;

let () = parse_options false defaults_w;;
let () = parse_options true defaults_warn_error;;

let message = function
  | Comment_start -> "this is the start of a comment."
  | Comment_not_end -> "this is not the end of a comment."
  | Deprecated s -> "deprecated: " ^ s
  | Fragile_match "" ->
      "this pattern-matching is fragile."
  | Fragile_match s ->
      "this pattern-matching is fragile.\n\
       It will remain exhaustive when constructors are added to type " ^ s ^ "."
  | Partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted ->
      "labels were omitted in the application of this function."
  | Method_override [lab] ->
      "the method " ^ lab ^ " is overridden."
  | Method_override (cname :: slist) ->
      String.concat " "
        ("the following methods are overridden by the class"
         :: cname  :: ":\n " :: slist)
  | Method_override [] -> assert false
  | Partial_match "" -> "this pattern-matching is not exhaustive."
  | Partial_match s ->
      "this pattern-matching is not exhaustive.\n\
       Here is an example of a value that is not matched:\n" ^ s
  | Non_closed_record_pattern s ->
      "the following labels are not bound in this record pattern:\n" ^ s ^
      "\nEither bind these labels explicitly or add '; _' to the pattern."
  | Statement_type ->
      "this expression should have type unit."
  | Unused_match -> "this match case is unused."
  | Unused_pat   -> "this sub-pattern is unused."
  | Instance_variable_override [lab] ->
      "the instance variable " ^ lab ^ " is overridden.\n" ^
      "The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override (cname :: slist) ->
      String.concat " "
        ("the following instance variables are overridden by the class"
         :: cname  :: ":\n " :: slist) ^
      "\nThe behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override [] -> assert false
  | Illegal_backslash -> "illegal backslash escape in string."
  | Implicit_public_methods l ->
      "the following private methods were made public implicitly:\n "
      ^ String.concat " " l ^ "."
  | Unerasable_optional_argument -> "this optional argument cannot be erased."
  | Undeclared_virtual_method m -> "the virtual method "^m^" is not declared."
  | Not_principal s -> s^" is not principal."
  | Without_principality s -> s^" without principality."
  | Unused_argument -> "this argument will not be used by the function."
  | Nonreturning_statement ->
      "this statement never returns (or has an unsound type.)"
  | Preprocessor s -> s
  | Useless_record_with ->
      "all the fields are explicitly listed in this record:\n\
       the 'with' clause is useless."
  | Bad_module_name (modname) ->
      "bad source file name: \"" ^ modname ^ "\" is not a valid module name."
  | All_clauses_guarded ->
      "bad style, all clauses in this pattern-matching are guarded."
  | Unused_var v | Unused_var_strict v -> "unused variable " ^ v ^ "."
  | Wildcard_arg_to_constant_constr ->
     "wildcard pattern given as argument to a constant constructor"
  | Eol_in_string ->
     "unescaped end-of-line in a string constant (non-portable code)"
  | Duplicate_definitions (kind, cname, tc1, tc2) ->
      Printf.sprintf "the %s %s is defined in both types %s and %s."
        kind cname tc1 tc2
  | Multiple_definition(modname, file1, file2) ->
      Printf.sprintf
        "files %s and %s both define a module named %s"
        file1 file2 modname
  | Unused_value_declaration v -> "unused value " ^ v ^ "."
  | Unused_open s -> "unused open " ^ s ^ "."
  | Unused_type_declaration s -> "unused type " ^ s ^ "."
  | Unused_for_index s -> "unused for-loop index " ^ s ^ "."
  | Unused_ancestor s -> "unused ancestor variable " ^ s ^ "."
  | Unused_constructor (s, false, false) -> "unused constructor " ^ s ^ "."
  | Unused_constructor (s, true, _) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        (However, this constructor appears in patterns.)"
  | Unused_constructor (s, false, true) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        Its type is exported as a private type."
  | Unused_extension (s, false, false) ->
      "unused extension constructor " ^ s ^ "."
  | Unused_extension (s, true, _) ->
      "extension constructor " ^ s ^
      " is never used to build values.\n\
        (However, this constructor appears in patterns.)"
  | Unused_extension (s, false, true) ->
      "extension constructor " ^ s ^
      " is never used to build values.\n\
        It is exported or rebound as a private extension."
  | Unused_rec_flag ->
      "unused rec flag."
  | Name_out_of_scope (ty, [nm], false) ->
      nm ^ " was selected from type " ^ ty ^
      ".\nIt is not visible in the current scope, and will not \n\
       be selected if the type becomes unknown."
  | Name_out_of_scope (_, _, false) -> assert false
  | Name_out_of_scope (ty, slist, true) ->
      "this record of type "^ ty ^" contains fields that are \n\
       not visible in the current scope: "
      ^ String.concat " " slist ^ ".\n\
       They will not be selected if the type becomes unknown."
  | Ambiguous_name ([s], tl, false) ->
      s ^ " belongs to several types: " ^ String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
  | Ambiguous_name (_, _, false) -> assert false
  | Ambiguous_name (slist, tl, true) ->
      "these field labels belong to several types: " ^
      String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
  | Disambiguated_name s ->
      "this use of " ^ s ^ " required disambiguation."
  | Nonoptional_label s ->
      "the label " ^ s ^ " is not optional."
  | Open_shadow_identifier (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s identifier %s (which is later used)"
        kind s
  | Open_shadow_label_constructor (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s %s (which is later used)"
        kind s
  | Bad_env_variable (var, s) ->
      Printf.sprintf "illegal environment variable %s : %s" var s
  | Attribute_payload (a, s) ->
      Printf.sprintf "illegal payload for attribute '%s'.\n%s" a s
  | Eliminated_optional_arguments sl ->
      Printf.sprintf "implicit elimination of optional argument%s %s"
        (if List.length sl = 1 then "" else "s")
        (String.concat ", " sl)
  | No_cmi_file s ->
      "no cmi file was found in path for module " ^ s
  | Bad_docstring unattached ->
      if unattached then "unattached documentation comment (ignored)"
      else "ambiguous documentation comment"
  | Bs_unused_attribute s ->
      "Unused BuckleScript attribute: " ^ s
  | Bs_polymorphic_comparison ->
      "polymorphic comparison introduced (maybe unsafe)"
  | Bs_ffi_warning s ->
      "BuckleScript FFI warning: " ^ s
  | Bs_derive_warning s ->
      "BuckleScript bs.deriving warning: " ^ s 
;;

let nerrors = ref 0;;

let print ppf w =
  let msg = message w in
  let num = number w in
  Format.fprintf ppf "%d: %s" num msg;
  Format.pp_print_flush ppf ();
  if (!current).error.(num) then incr nerrors
;;


(* used by super-errors. Copied from the `print` above *)
let super_print message ppf w =
  let msg = message w in
  let num = number w in
  Format.fprintf ppf "%s" msg;
  Format.pp_print_flush ppf ();
  if (!current).error.(num) then incr nerrors
;;


exception Errors of int;;

let check_fatal () =
  if !nerrors > 0 then begin
    let e = Errors !nerrors in
    nerrors := 0;
    raise e;
  end;
;;

let descriptions =
  [
    1, "Suspicious-looking start-of-comment mark.";
    2, "Suspicious-looking end-of-comment mark.";
    3, "Deprecated feature.";
    4, "Fragile pattern matching: matching that will remain complete even\n\
   \    if additional constructors are added to one of the variant types\n\
   \    matched.";
    5, "Partially applied function: expression whose result has function\n\
   \    type and is ignored.";
    6, "Label omitted in function application.";
    7, "Method overridden.";
    8, "Partial match: missing cases in pattern-matching.";
    9, "Missing fields in a record pattern.";
   10, "Expression on the left-hand side of a sequence that doesn't have type\n\
   \    \"unit\" (and that is not a function, see warning number 5).";
   11, "Redundant case in a pattern matching (unused match case).";
   12, "Redundant sub-pattern in a pattern-matching.";
   13, "Instance variable overridden.";
   14, "Illegal backslash escape in a string constant.";
   15, "Private method made public implicitly.";
   16, "Unerasable optional argument.";
   17, "Undeclared virtual method.";
   18, "Non-principal type.";
   19, "Type without principality.";
   20, "Unused function argument.";
   21, "Non-returning statement.";
   22, "Proprocessor warning.";
   23, "Useless record \"with\" clause.";
   24, "Bad module name: the source file name is not a valid OCaml module \
        name.";
   25, "Pattern-matching with all clauses guarded.  Exhaustiveness cannot be\n\
   \    checked.";
   26, "Suspicious unused variable: unused variable that is bound\n\
   \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   27, "Innocuous unused variable: unused variable that is not bound with\n\
   \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   28, "Wildcard pattern given as argument to a constant constructor.";
   29, "Unescaped end-of-line in a string constant (non-portable code).";
   30, "Two labels or constructors of the same name are defined in two\n\
   \    mutually recursive types.";
   31, "A module is linked twice in the same executable.";
   32, "Unused value declaration.";
   33, "Unused open statement.";
   34, "Unused type declaration.";
   35, "Unused for-loop index.";
   36, "Unused ancestor variable.";
   37, "Unused constructor.";
   38, "Unused extension constructor.";
   39, "Unused rec flag.";
   40, "Constructor or label name used out of scope.";
   41, "Ambiguous constructor or label name.";
   42, "Disambiguated constructor or label name.";
   43, "Nonoptional label applied as optional.";
   44, "Open statement shadows an already defined identifier.";
   45, "Open statement shadows an already defined label or constructor.";
   46, "Error in environment variable.";
   47, "Illegal attribute payload.";
   48, "Implicit elimination of optional arguments.";
   49, "Missing cmi file when looking up module alias.";
   50, "Unexpected documentation comment.";
   101,"Unused bs attributes";
  ]
;;

let help_warnings () =
  List.iter (fun (i, s) -> Printf.printf "%3i %s\n" i s) descriptions;
  print_endline "  A all warnings";
  for i = Char.code 'b' to Char.code 'z' do
    let c = Char.chr i in
    match letter c with
    | [] -> ()
    | [n] ->
        Printf.printf "  %c warning %i\n" (Char.uppercase c) n
    | l ->
        Printf.printf "  %c warnings %s.\n"
          (Char.uppercase c)
          (String.concat ", " (List.map string_of_int l))
  done;
  exit 0
;;

end
module Location : sig 
#1 "location.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Source code locations (ranges of positions), used in parsetree. *)

open Format

type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

(* Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
     re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)

val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. *)
val rhs_loc: int -> t

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

val get_pos_info: Lexing.position -> string * int * int (* file, line, char *)
val print_loc: formatter -> t -> unit
val print_error: formatter -> t -> unit
val print_error_cur_file: formatter -> unit -> unit
val print_warning: t -> formatter -> Warnings.t -> unit
val formatter_for_warnings : formatter ref
val prerr_warning: t -> Warnings.t -> unit
val echo_eof: unit -> unit
val reset: unit -> unit

val warning_printer : (t -> formatter -> Warnings.t -> unit) ref
(** Hook for intercepting warnings. *)

val default_warning_printer : t -> formatter -> Warnings.t -> unit
(** Original warning printer for use in hooks. *)

val highlight_locations: formatter -> t list -> bool

type 'a loc = {
  txt : 'a;
  loc : t;
}

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc

val print: formatter -> t -> unit
val print_filename: formatter -> string -> unit

val absolute_path: string -> string

val show_filename: string -> string
    (** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. *)


val absname: bool ref

(* Support for located errors *)

type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

exception Error of error

val print_error_prefix: formatter -> unit -> unit
  (* print the prefix "Error:" possibly with style *)

val error: ?loc:t -> ?sub:error list -> ?if_highlight:string -> string -> error

 
val pp_ksprintf : ?before:(formatter -> unit) -> (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b


val errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, error) format4 -> 'a

val raise_errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val error_of_printer: t -> (formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file: (formatter -> 'a -> unit) -> 'a -> error

val error_of_exn: exn -> error option

val register_error_of_exn: (exn -> error option) -> unit
  (* Each compiler module which defines a custom type of exception
     which can surface as a user-visible error should register
     a "printer" for this exception using [register_error_of_exn].
     The result of the printer is an [error] value containing
     a location, a message, and optionally sub-messages (each of them
     being located as well). *)

val report_error: formatter -> error -> unit

val error_reporter : (formatter -> error -> unit) ref
(** Hook for intercepting error reports. *)

val default_error_reporter : formatter -> error -> unit
(** Original error reporter for use in hooks. *)

val report_exception: formatter -> exn -> unit
  (* Reraise the exception if it is unknown. *)

end = struct
#1 "location.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lexing

let absname = ref false
    (* This reference should be in Clflags, but it would create an additional
       dependency and make bootstrapping Camlp4 more difficult. *)

type t = { loc_start: position; loc_end: position; loc_ghost: bool };;

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
};;

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
};;

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
};;

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

(* Terminal info *)

let status = ref Terminfo.Uninitialised

let num_loc_lines = ref 0 (* number of lines already printed after input *)

let print_updating_num_loc_lines ppf f arg =
  let open Format in
  let out_functions = pp_get_formatter_out_functions ppf () in
  let out_string str start len =
    let rec count i c =
      if i = start + len then c
      else if String.get str i = '\n' then count (succ i) (succ c)
      else count (succ i) c in
    num_loc_lines := !num_loc_lines + count start 0 ;
    out_functions.out_string str start len in
  pp_set_formatter_out_functions ppf
    { out_functions with out_string } ;
  f ppf arg ;
  pp_print_flush ppf ();
  pp_set_formatter_out_functions ppf out_functions

(* Highlight the locations using standout mode. *)

let highlight_terminfo ppf num_lines lb locs =
  Format.pp_print_flush ppf ();  (* avoid mixing Format and normal output *)
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  (* Count number of lines in phrase *)
  let lines = ref !num_loc_lines in
  for i = pos0 to lb.lex_buffer_len - 1 do
    if Bytes.get lb.lex_buffer i = '\n' then incr lines
  done;
  (* If too many lines, give up *)
  if !lines >= num_lines - 2 then raise Exit;
  (* Move cursor up that number of lines *)
  flush stdout; Terminfo.backup !lines;
  (* Print the input, switching to standout for the location *)
  let bol = ref false in
  print_string "# ";
  for pos = 0 to lb.lex_buffer_len - pos0 - 1 do
    if !bol then (print_string "  "; bol := false);
    if List.exists (fun loc -> pos = loc.loc_start.pos_cnum) locs then
      Terminfo.standout true;
    if List.exists (fun loc -> pos = loc.loc_end.pos_cnum) locs then
      Terminfo.standout false;
    let c = Bytes.get lb.lex_buffer (pos + pos0) in
    print_char c;
    bol := (c = '\n')
  done;
  (* Make sure standout mode is over *)
  Terminfo.standout false;
  (* Position cursor back to original location *)
  Terminfo.resume !num_loc_lines;
  flush stdout

(* Highlight the location by printing it again. *)

let highlight_dumb ppf lb loc =
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  let end_pos = lb.lex_buffer_len - pos0 - 1 in
  (* Determine line numbers for the start and end points *)
  let line_start = ref 0 and line_end = ref 0 in
  for pos = 0 to end_pos do
    if Bytes.get lb.lex_buffer (pos + pos0) = '\n' then begin
      if loc.loc_start.pos_cnum > pos then incr line_start;
      if loc.loc_end.pos_cnum   > pos then incr line_end;
    end
  done;
  (* Print character location (useful for Emacs) *)
  Format.fprintf ppf "Characters %i-%i:@."
                 loc.loc_start.pos_cnum loc.loc_end.pos_cnum;
  (* Print the input, underlining the location *)
  Format.pp_print_string ppf "  ";
  let line = ref 0 in
  let pos_at_bol = ref 0 in
  for pos = 0 to end_pos do
    match Bytes.get lb.lex_buffer (pos + pos0) with
    | '\n' ->
      if !line = !line_start && !line = !line_end then begin
        (* loc is on one line: underline location *)
        Format.fprintf ppf "@.  ";
        for _i = !pos_at_bol to loc.loc_start.pos_cnum - 1 do
          Format.pp_print_char ppf ' '
        done;
        for _i = loc.loc_start.pos_cnum to loc.loc_end.pos_cnum - 1 do
          Format.pp_print_char ppf '^'
        done
      end;
      if !line >= !line_start && !line <= !line_end then begin
        Format.fprintf ppf "@.";
        if pos < loc.loc_end.pos_cnum then Format.pp_print_string ppf "  "
      end;
      incr line;
      pos_at_bol := pos + 1
    | '\r' -> () (* discard *)
    | c ->
      if !line = !line_start && !line = !line_end then
        (* loc is on one line: print whole line *)
        Format.pp_print_char ppf c
      else if !line = !line_start then
        (* first line of multiline loc:
           print a dot for each char before loc_start *)
        if pos < loc.loc_start.pos_cnum then
          Format.pp_print_char ppf '.'
        else
          Format.pp_print_char ppf c
      else if !line = !line_end then
        (* last line of multiline loc: print a dot for each char
           after loc_end, even whitespaces *)
        if pos < loc.loc_end.pos_cnum then
          Format.pp_print_char ppf c
        else
          Format.pp_print_char ppf '.'
      else if !line > !line_start && !line < !line_end then
        (* intermediate line of multiline loc: print whole line *)
        Format.pp_print_char ppf c
  done

(* Highlight the location using one of the supported modes. *)

let rec highlight_locations ppf locs =
  match !status with
    Terminfo.Uninitialised ->
      status := Terminfo.setup stdout; highlight_locations ppf locs
  | Terminfo.Bad_term ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          let norepeat =
            try Sys.getenv "TERM" = "norepeat" with Not_found -> false in
          if norepeat then false else
            let loc1 = List.hd locs in
            try highlight_dumb ppf lb loc1; true
            with Exit -> false
      end
  | Terminfo.Good_term num_lines ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          try highlight_terminfo ppf num_lines lb locs; true
          with Exit -> false
      end

(* Print the location in some way or another *)

open Format

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if is_relative s then concat (Sys.getcwd ()) s else s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  if !absname then absolute_path file else file

let print_filename ppf file =
  Format.fprintf ppf "%s" (show_filename file)

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) =
  ("File \"", "\", line ", ", characters ", "-", ":")

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
;;

let setup_colors () =
  Misc.Color.setup !Clflags.color

let print_loc ppf loc =
  setup_colors ();
  let (file, line, startchar) = get_pos_info loc.loc_start in
 
  let startchar = 
    if Clflags.bs_vscode then startchar + 1 else startchar in 
    
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  if file = "//toplevel//" then begin
    if highlight_locations ppf [loc] then () else
      fprintf ppf "Characters %i-%i"
              loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else begin
    fprintf ppf "%s@{<loc>%a%s%i" msg_file print_filename file msg_line line;
    if startchar >= 0 then
      fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar;
    fprintf ppf "@}"
  end
;;

let print ppf loc =
  setup_colors ();
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else fprintf ppf "@{<loc>%a@}%s@." print_loc loc msg_colon
;;

let error_prefix = "Error"
let warning_prefix = "Warning"

let print_error_prefix ppf () =
  setup_colors ();
  fprintf ppf "@{<error>%s@}:" error_prefix;
  ()
;;

let print_error ppf loc =
  print ppf loc;
  print_error_prefix ppf ()
;;

let print_error_cur_file ppf () = print_error ppf (in_file !input_name);;

let default_warning_printer loc ppf w =
  if Warnings.is_active w then begin
    setup_colors ();
    print ppf loc;
    fprintf ppf "@{<warning>%s@} %a@." warning_prefix Warnings.print w
  end
;;

let warning_printer = ref default_warning_printer ;;

let print_warning loc ppf w =
  print_updating_num_loc_lines ppf (!warning_printer loc) w
;;

let formatter_for_warnings = ref err_formatter;;
let prerr_warning loc w = print_warning loc !formatter_for_warnings w;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none


type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

let pp_ksprintf ?before k fmt =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Misc.Color.set_color_tag_handling ppf;
  begin match before with
    | None -> ()
    | Some f -> f ppf
  end;
  kfprintf
    (fun _ ->
      pp_print_flush ppf ();
      let msg = Buffer.contents buf in
      k msg)
    ppf fmt

(* Shift the formatter's offset by the length of the error prefix, which
   is always added by the compiler after the message has been formatted *)
let print_phanton_error_prefix ppf =
  Format.pp_print_as ppf (String.length error_prefix + 2 (* ": " *)) ""

let errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") fmt =
  pp_ksprintf
    ~before:print_phanton_error_prefix
    (fun msg -> {loc; msg; sub; if_highlight})
    fmt

let error ?(loc = none) ?(sub = []) ?(if_highlight = "") msg =
  {loc; msg; sub; if_highlight}

let error_of_exn : (exn -> error option) list ref = ref []

let register_error_of_exn f = error_of_exn := f :: !error_of_exn

let error_of_exn exn =
  let rec loop = function
    | [] -> None
    | f :: rest ->
        match f exn with
        | Some _ as r -> r
        | None -> loop rest
  in
  loop !error_of_exn

let rec default_error_reporter ppf ({loc; msg; sub; if_highlight} as err) =
  let highlighted =
    if if_highlight <> "" then
      let rec collect_locs locs {loc; sub; if_highlight; _} =
        List.fold_left collect_locs (loc :: locs) sub
      in
      let locs = collect_locs [] err in
      highlight_locations ppf locs
    else
      false
  in
  if highlighted then
    Format.pp_print_string ppf if_highlight
  else begin
    fprintf ppf "%a%a %s" print loc print_error_prefix () msg;
    List.iter (Format.fprintf ppf "@\n@[<2>%a@]" default_error_reporter) sub
  end

let error_reporter = ref default_error_reporter

let report_error ppf err =
  print_updating_num_loc_lines ppf !error_reporter err
;;

let error_of_printer loc print x =
  errorf ~loc "%a@?" print x

let error_of_printer_file print x =
  error_of_printer (in_file !input_name) print x

let () =
  register_error_of_exn
    (function
      | Sys_error msg ->
          Some (errorf ~loc:(in_file !input_name)
                "I/O error: %s" msg)
      | Warnings.Errors n ->
          Some
            (errorf ~loc:(in_file !input_name)
             "Some fatal warnings were triggered (%d occurrences)" n)
      | _ ->
          None
    )


let rec report_exception_rec n ppf exn =
  try match error_of_exn exn with
  | Some err ->
      fprintf ppf "@[%a@]@." report_error err
  | None -> raise exn
  with exn when n > 0 ->
    report_exception_rec (n-1) ppf exn

let report_exception ppf exn = report_exception_rec 5 ppf exn


exception Error of error

let () =
  register_error_of_exn
    (function
      | Error e -> Some e
      | _ -> None
    )

let raise_errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") =
  pp_ksprintf
    ~before:print_phanton_error_prefix
    (fun msg -> raise (Error ({loc; msg; sub; if_highlight})))

end
(** Interface as module  *)
module Asttypes
= struct
#1 "asttypes.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Auxiliary a.s.t. types used by parsetree and typedtree. *)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}


type variance =
  | Covariant
  | Contravariant
  | Invariant

end
module Longident : sig 
#1 "longident.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten: t -> string list
val last: t -> string
val parse: string -> t

end = struct
#1 "longident.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

let rec flat accu = function
    Lident s -> s :: accu
  | Ldot(lid, s) -> flat (s :: accu) lid
  | Lapply(_, _) -> Misc.fatal_error "Longident.flat"

let flatten lid = flat [] lid

let last = function
    Lident s -> s
  | Ldot(_, s) -> s
  | Lapply(_, _) -> Misc.fatal_error "Longident.last"

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let parse s =
  match split_at_dots s 0 with
    [] -> Lident ""  (* should not happen, but don't put assert false
                        so as not to crash the toplevel (see Genprintval) *)
  | hd :: tl -> List.fold_left (fun p s -> Ldot(p, s)) (Lident hd) tl

end
(** Interface as module  *)
module Parsetree
= struct
#1 "parsetree.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Abstract syntax tree produced by parsing *)

open Asttypes

(** {2 Extension points} *)

type attribute = string loc * payload
       (* [@id ARG]
          [@@id ARG]

          Metadata containers passed around within the AST.
          The compiler ignores unknown attributes.
       *)

and extension = string loc * payload
      (* [%id ARG]
         [%%id ARG]

         Sub-language placeholder -- rejected by the typechecker.
      *)

and attributes = attribute list

and payload =
  | PStr of structure
  | PTyp of core_type  (* : T *)
  | PPat of pattern * expression option  (* ? P  or  ? P when E *)

(** {2 Core language} *)

(* Type expressions *)

and core_type =
    {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and core_type_desc =
  | Ptyp_any
        (*  _ *)
  | Ptyp_var of string
        (* 'a *)
  | Ptyp_arrow of label * core_type * core_type
        (* T1 -> T2       (label = "")
           ~l:T1 -> T2    (label = "l")
           ?l:T1 -> T2    (label = "?l")
         *)
  | Ptyp_tuple of core_type list
        (* T1 * ... * Tn

           Invariant: n >= 2
        *)
  | Ptyp_constr of Longident.t loc * core_type list
        (* tconstr
           T tconstr
           (T1, ..., Tn) tconstr
         *)
  | Ptyp_object of (string * attributes * core_type) list * closed_flag
        (* < l1:T1; ...; ln:Tn >     (flag = Closed)
           < l1:T1; ...; ln:Tn; .. > (flag = Open)
         *)
  | Ptyp_class of Longident.t loc * core_type list
        (* #tconstr
           T #tconstr
           (T1, ..., Tn) #tconstr
         *)
  | Ptyp_alias of core_type * string
        (* T as 'a *)
  | Ptyp_variant of row_field list * closed_flag * label list option
        (* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
         *)
  | Ptyp_poly of string list * core_type
        (* 'a1 ... 'an. T

           Can only appear in the following context:

           - As the core_type of a Ppat_constraint node corresponding
             to a constraint on a let-binding: let x : 'a1 ... 'an. T
             = e ...

           - Under Cfk_virtual for methods (not values).

           - As the core_type of a Pctf_method node.

           - As the core_type of a Pexp_poly node.

           - As the pld_type field of a label_declaration.

           - As a core_type of a Ptyp_object node.
         *)

  | Ptyp_package of package_type
        (* (module S) *)
  | Ptyp_extension of extension
        (* [%id] *)

and package_type = Longident.t loc * (Longident.t loc * core_type) list
      (*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
       *)

and row_field =
  | Rtag of label * attributes * bool * core_type list
        (* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

          - The 2nd field is true if the tag contains a
            constant (empty) constructor.
          - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)

          - TODO: switch to a record representation, and keep location
        *)
  | Rinherit of core_type
        (* [ T ] *)

(* Patterns *)

and pattern =
    {
     ppat_desc: pattern_desc;
     ppat_loc: Location.t;
     ppat_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and pattern_desc =
  | Ppat_any
        (* _ *)
  | Ppat_var of string loc
        (* x *)
  | Ppat_alias of pattern * string loc
        (* P as 'a *)
  | Ppat_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval of constant * constant
        (* 'a'..'z'

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple of pattern list
        (* (P1, ..., Pn)

           Invariant: n >= 2
        *)
  | Ppat_construct of Longident.t loc * pattern option
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
         *)
  | Ppat_variant of label * pattern option
        (* `A             (None)
           `A P           (Some P)
         *)
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
         *)
  | Ppat_array of pattern list
        (* [| P1; ...; Pn |] *)
  | Ppat_or of pattern * pattern
        (* P1 | P2 *)
  | Ppat_constraint of pattern * core_type
        (* (P : T) *)
  | Ppat_type of Longident.t loc
        (* #tconst *)
  | Ppat_lazy of pattern
        (* lazy P *)
  | Ppat_unpack of string loc
        (* (module P)
           Note: (module P : S) is represented as
           Ppat_constraint(Ppat_unpack, Ptyp_package)
         *)
  | Ppat_exception of pattern
        (* exception P *)
  | Ppat_extension of extension
        (* [%id] *)

(* Value expressions *)

and expression =
    {
     pexp_desc: expression_desc;
     pexp_loc: Location.t;
     pexp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and expression_desc =
  | Pexp_ident of Longident.t loc
        (* x
           M.x
         *)
  | Pexp_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Pexp_let of rec_flag * value_binding list * expression
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Pexp_function of case list
        (* function P1 -> E1 | ... | Pn -> En *)
  | Pexp_fun of label * expression option * pattern * expression
        (* fun P -> E1                          (lab = "", None)
           fun ~l:P -> E1                       (lab = "l", None)
           fun ?l:P -> E1                       (lab = "?l", None)
           fun ?l:(P = E0) -> E1                (lab = "?l", Some E0)

           Notes:
           - If E0 is provided, lab must start with '?'.
           - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
           - "let f P = E" is represented using Pexp_fun.
         *)
  | Pexp_apply of expression * (label * expression) list
        (* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
         *)
  | Pexp_match of expression * case list
        (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_try of expression * case list
        (* try E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_tuple of expression list
        (* (E1, ..., En)

           Invariant: n >= 2
        *)
  | Pexp_construct of Longident.t loc * expression option
        (* C                None
           C E              Some E
           C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
        *)
  | Pexp_variant of label * expression option
        (* `A             (None)
           `A E           (Some E)
         *)
  | Pexp_record of (Longident.t loc * expression) list * expression option
        (* { l1=P1; ...; ln=Pn }     (None)
           { E0 with l1=P1; ...; ln=Pn }   (Some E0)

           Invariant: n > 0
         *)
  | Pexp_field of expression * Longident.t loc
        (* E.l *)
  | Pexp_setfield of expression * Longident.t loc * expression
        (* E1.l <- E2 *)
  | Pexp_array of expression list
        (* [| E1; ...; En |] *)
  | Pexp_ifthenelse of expression * expression * expression option
        (* if E1 then E2 else E3 *)
  | Pexp_sequence of expression * expression
        (* E1; E2 *)
  | Pexp_while of expression * expression
        (* while E1 do E2 done *)
  | Pexp_for of
      pattern *  expression * expression * direction_flag * expression
        (* for i = E1 to E2 do E3 done      (flag = Upto)
           for i = E1 downto E2 do E3 done  (flag = Downto)
         *)
  | Pexp_constraint of expression * core_type
        (* (E : T) *)
  | Pexp_coerce of expression * core_type option * core_type
        (* (E :> T)        (None, T)
           (E : T0 :> T)   (Some T0, T)
         *)
  | Pexp_send of expression * string
        (*  E # m *)
  | Pexp_new of Longident.t loc
        (* new M.c *)
  | Pexp_setinstvar of string loc * expression
        (* x <- 2 *)
  | Pexp_override of (string loc * expression) list
        (* {< x1 = E1; ...; Xn = En >} *)
  | Pexp_letmodule of string loc * module_expr * expression
        (* let module M = ME in E *)
  | Pexp_assert of expression
        (* assert E
           Note: "assert false" is treated in a special way by the
           type-checker. *)
  | Pexp_lazy of expression
        (* lazy E *)
  | Pexp_poly of expression * core_type option
        (* Used for method bodies.

           Can only be used as the expression under Cfk_concrete
           for methods (not values). *)
  | Pexp_object of class_structure
        (* object ... end *)
  | Pexp_newtype of string * expression
        (* fun (type t) -> E *)
  | Pexp_pack of module_expr
        (* (module ME)

           (module ME : S) is represented as
           Pexp_constraint(Pexp_pack, Ptyp_package S) *)
  | Pexp_open of override_flag * Longident.t loc * expression
        (* let open M in E
           let! open M in E
        *)
  | Pexp_extension of extension
        (* [%id] *)

and case =   (* (P -> E) or (P when E0 -> E) *)
    {
     pc_lhs: pattern;
     pc_guard: expression option;
     pc_rhs: expression;
    }

(* Value descriptions *)

and value_description =
    {
     pval_name: string loc;
     pval_type: core_type;
     pval_prim: string list;
     pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
     pval_loc: Location.t;
    }

(*
  val x: T                            (prim = [])
  external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])

  Note: when used under Pstr_primitive, prim cannot be empty
*)

(* Type declarations *)

and type_declaration =
    {
     ptype_name: string loc;
     ptype_params: (core_type * variance) list;
           (* ('a1,...'an) t; None represents  _*)
     ptype_cstrs: (core_type * core_type * Location.t) list;
           (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
     ptype_kind: type_kind;
     ptype_private: private_flag;   (* = private ... *)
     ptype_manifest: core_type option;  (* = T *)
     ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
     ptype_loc: Location.t;
    }

(*
  type t                     (abstract, no manifest)
  type t = T0                (abstract, manifest=T0)
  type t = C of T | ...      (variant,  no manifest)
  type t = T0 = C of T | ... (variant,  manifest=T0)
  type t = {l: T; ...}       (record,   no manifest)
  type t = T0 = {l : T; ...} (record,   manifest=T0)
  type t = ..                (open,     no manifest)
*)

and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
        (* Invariant: non-empty list *)
  | Ptype_record of label_declaration list
        (* Invariant: non-empty list *)
  | Ptype_open

and label_declaration =
    {
     pld_name: string loc;
     pld_mutable: mutable_flag;
     pld_type: core_type;
     pld_loc: Location.t;
     pld_attributes: attributes; (* l [@id1] [@id2] : T *)
    }

(*  { ...; l: T; ... }            (mutable=Immutable)
    { ...; mutable l: T; ... }    (mutable=Mutable)

    Note: T can be a Ptyp_poly.
*)

and constructor_declaration =
    {
     pcd_name: string loc;
     pcd_args: core_type list;
     pcd_res: core_type option;
     pcd_loc: Location.t;
     pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }
(*
  | C of T1 * ... * Tn     (res = None)
  | C: T0                  (args = [], res = Some T0)
  | C: T1 * ... * Tn -> T0 (res = Some T0)
*)

and type_extension =
    {
     ptyext_path: Longident.t loc;
     ptyext_params: (core_type * variance) list;
     ptyext_constructors: extension_constructor list;
     ptyext_private: private_flag;
     ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
    }
(*
  type t += ...
*)

and extension_constructor =
    {
     pext_name: string loc;
     pext_kind : extension_constructor_kind;
     pext_loc : Location.t;
     pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }

and extension_constructor_kind =
    Pext_decl of core_type list * core_type option
      (*
         | C of T1 * ... * Tn     ([T1; ...; Tn], None)
         | C: T0                  ([], Some T0)
         | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
       *)
  | Pext_rebind of Longident.t loc
      (*
         | C = D
       *)

(** {2 Class language} *)

(* Type expressions for the class language *)

and class_type =
    {
     pcty_desc: class_type_desc;
     pcty_loc: Location.t;
     pcty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_type_desc =
  | Pcty_constr of Longident.t loc * core_type list
        (* c
           ['a1, ..., 'an] c *)
  | Pcty_signature of class_signature
        (* object ... end *)
  | Pcty_arrow of label * core_type * class_type
        (* T -> CT       (label = "")
           ~l:T -> CT    (label = "l")
           ?l:T -> CT    (label = "?l")
         *)
  | Pcty_extension of extension
        (* [%id] *)

and class_signature =
    {
     pcsig_self: core_type;
     pcsig_fields: class_type_field list;
    }
(* object('selfpat) ... end
   object ... end             (self = Ptyp_any)
 *)

and class_type_field =
    {
     pctf_desc: class_type_field_desc;
     pctf_loc: Location.t;
     pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_type_field_desc =
  | Pctf_inherit of class_type
        (* inherit CT *)
  | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
        (* val x: T *)
  | Pctf_method  of (string * private_flag * virtual_flag * core_type)
        (* method x: T

           Note: T can be a Ptyp_poly.
         *)
  | Pctf_constraint  of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pctf_attribute of attribute
        (* [@@@id] *)
  | Pctf_extension of extension
        (* [%%id] *)

and 'a class_infos =
    {
     pci_virt: virtual_flag;
     pci_params: (core_type * variance) list;
     pci_name: string loc;
     pci_expr: 'a;
     pci_loc: Location.t;
     pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
    }
(* class c = ...
   class ['a1,...,'an] c = ...
   class virtual c = ...

   Also used for "class type" declaration.
*)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)

and class_expr =
    {
     pcl_desc: class_expr_desc;
     pcl_loc: Location.t;
     pcl_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_expr_desc =
  | Pcl_constr of Longident.t loc * core_type list
        (* c
           ['a1, ..., 'an] c *)
  | Pcl_structure of class_structure
        (* object ... end *)
  | Pcl_fun of label * expression option * pattern * class_expr
        (* fun P -> CE                          (lab = "", None)
           fun ~l:P -> CE                       (lab = "l", None)
           fun ?l:P -> CE                       (lab = "?l", None)
           fun ?l:(P = E0) -> CE                (lab = "?l", Some E0)
         *)
  | Pcl_apply of class_expr * (label * expression) list
        (* CE ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
         *)
  | Pcl_let of rec_flag * value_binding list * class_expr
        (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
         *)
  | Pcl_constraint of class_expr * class_type
        (* (CE : CT) *)
  | Pcl_extension of extension
        (* [%id] *)

and class_structure =
    {
     pcstr_self: pattern;
     pcstr_fields: class_field list;
    }
(* object(selfpat) ... end
   object ... end           (self = Ppat_any)
 *)

and class_field =
    {
     pcf_desc: class_field_desc;
     pcf_loc: Location.t;
     pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string option
        (* inherit CE
           inherit CE as x
           inherit! CE
           inherit! CE as x
         *)
  | Pcf_val of (string loc * mutable_flag * class_field_kind)
        (* val x = E
           val virtual x: T
         *)
  | Pcf_method of (string loc * private_flag * class_field_kind)
        (* method x = E            (E can be a Pexp_poly)
           method virtual x: T     (T can be a Ptyp_poly)
         *)
  | Pcf_constraint of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pcf_initializer of expression
        (* initializer E *)
  | Pcf_attribute of attribute
        (* [@@@id] *)
  | Pcf_extension of extension
        (* [%%id] *)

and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration = class_expr class_infos

(** {2 Module language} *)

(* Type expressions for the module language *)

and module_type =
    {
     pmty_desc: module_type_desc;
     pmty_loc: Location.t;
     pmty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_type_desc =
  | Pmty_ident of Longident.t loc
        (* S *)
  | Pmty_signature of signature
        (* sig ... end *)
  | Pmty_functor of string loc * module_type option * module_type
        (* functor(X : MT1) -> MT2 *)
  | Pmty_with of module_type * with_constraint list
        (* MT with ... *)
  | Pmty_typeof of module_expr
        (* module type of ME *)
  | Pmty_extension of extension
        (* [%id] *)
  | Pmty_alias of Longident.t loc
        (* (module M) *)

and signature = signature_item list

and signature_item =
    {
     psig_desc: signature_item_desc;
     psig_loc: Location.t;
    }

and signature_item_desc =
  | Psig_value of value_description
        (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
  | Psig_type of type_declaration list
        (* type t1 = ... and ... and tn = ... *)
  | Psig_typext of type_extension
        (* type t1 += ... *)
  | Psig_exception of extension_constructor
        (* exception C of T *)
  | Psig_module of module_declaration
        (* module X : MT *)
  | Psig_recmodule of module_declaration list
        (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Psig_modtype of module_type_declaration
        (* module type S = MT
           module type S *)
  | Psig_open of open_description
        (* open X *)
  | Psig_include of include_description
        (* include MT *)
  | Psig_class of class_description list
        (* class c1 : ... and ... and cn : ... *)
  | Psig_class_type of class_type_declaration list
        (* class type ct1 = ... and ... and ctn = ... *)
  | Psig_attribute of attribute
        (* [@@@id] *)
  | Psig_extension of extension * attributes
        (* [%%id] *)

and module_declaration =
    {
     pmd_name: string loc;
     pmd_type: module_type;
     pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmd_loc: Location.t;
    }
(* S : MT *)

and module_type_declaration =
    {
     pmtd_name: string loc;
     pmtd_type: module_type option;
     pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmtd_loc: Location.t;
    }
(* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*)

and open_description =
    {
     popen_lid: Longident.t loc;
     popen_override: override_flag;
     popen_loc: Location.t;
     popen_attributes: attributes;
    }
(* open! X - popen_override = Override (silences the 'used identifier
                              shadowing' warning)
   open  X - popen_override = Fresh
 *)

and 'a include_infos =
    {
     pincl_mod: 'a;
     pincl_loc: Location.t;
     pincl_attributes: attributes;
    }

and include_description = module_type include_infos
(* include MT *)

and include_declaration = module_expr include_infos
(* include ME *)

and with_constraint =
  | Pwith_type of Longident.t loc * type_declaration
        (* with type X.t = ...

           Note: the last component of the longident must match
           the name of the type_declaration. *)
  | Pwith_module of Longident.t loc * Longident.t loc
        (* with module X.Y = Z *)
  | Pwith_typesubst of type_declaration
        (* with type t := ... *)
  | Pwith_modsubst of string loc * Longident.t loc
        (* with module X := Z *)

(* Value expressions for the module language *)

and module_expr =
    {
     pmod_desc: module_expr_desc;
     pmod_loc: Location.t;
     pmod_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_expr_desc =
  | Pmod_ident of Longident.t loc
        (* X *)
  | Pmod_structure of structure
        (* struct ... end *)
  | Pmod_functor of string loc * module_type option * module_expr
        (* functor(X : MT1) -> ME *)
  | Pmod_apply of module_expr * module_expr
        (* ME1(ME2) *)
  | Pmod_constraint of module_expr * module_type
        (* (ME : MT) *)
  | Pmod_unpack of expression
        (* (val E) *)
  | Pmod_extension of extension
        (* [%id] *)

and structure = structure_item list

and structure_item =
    {
     pstr_desc: structure_item_desc;
     pstr_loc: Location.t;
    }

and structure_item_desc =
  | Pstr_eval of expression * attributes
        (* E *)
  | Pstr_value of rec_flag * value_binding list
        (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
         *)
  | Pstr_primitive of value_description
        (* external x: T = "s1" ... "sn" *)
  | Pstr_type of type_declaration list
        (* type t1 = ... and ... and tn = ... *)
  | Pstr_typext of type_extension
        (* type t1 += ... *)
  | Pstr_exception of extension_constructor
        (* exception C of T
           exception C = M.X *)
  | Pstr_module of module_binding
        (* module X = ME *)
  | Pstr_recmodule of module_binding list
        (* module rec X1 = ME1 and ... and Xn = MEn *)
  | Pstr_modtype of module_type_declaration
        (* module type S = MT *)
  | Pstr_open of open_description
        (* open X *)
  | Pstr_class of class_declaration list
        (* class c1 = ... and ... and cn = ... *)
  | Pstr_class_type of class_type_declaration list
        (* class type ct1 = ... and ... and ctn = ... *)
  | Pstr_include of include_declaration
        (* include ME *)
  | Pstr_attribute of attribute
        (* [@@@id] *)
  | Pstr_extension of extension * attributes
        (* [%%id] *)

and value_binding =
  {
    pvb_pat: pattern;
    pvb_expr: expression;
    pvb_attributes: attributes;
    pvb_loc: Location.t;
  }

and module_binding =
    {
     pmb_name: string loc;
     pmb_expr: module_expr;
     pmb_attributes: attributes;
     pmb_loc: Location.t;
    }
(* X = ME *)

(** {2 Toplevel} *)

(* Toplevel phrases *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of string * directive_argument
     (* #use, #load ... *)

and directive_argument =
  | Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
  | Pdir_bool of bool

end
module Docstrings : sig 
#1 "docstrings.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                              Leo White                              *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** (Re)Initialise all docstring state *)
val init : unit -> unit

(** Emit warnings for unattached and ambiguous docstrings *)
val warn_bad_docstrings : unit -> unit

(** {3 Docstrings} *)

(** Documentation comments *)
type docstring

(** Create a docstring *)
val docstring : string -> Location.t -> docstring

(** Get the text of a docstring *)
val docstring_body : docstring -> string

(** Get the location of a docstring *)
val docstring_loc : docstring -> Location.t

(** {3 Set functions}

   These functions are used by the lexer to associate docstrings to
   the locations of tokens. *)

(** Docstrings immediately preceding a token *)
val set_pre_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately following a token *)
val set_post_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings not immediately adjacent to a token *)
val set_floating_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately following the token which precedes this one *)
val set_pre_extra_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately preceding the token which follows this one *)
val set_post_extra_docstrings : Lexing.position -> docstring list -> unit

(** {3 Items}

    The {!docs} type represents documentation attached to an item. *)

type docs =
  { docs_pre: docstring option;
    docs_post: docstring option; }

val empty_docs : docs

val docs_attr : docstring -> Parsetree.attribute

(** Convert item documentation to attributes and add them to an
    attribute list *)
val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the item documentation for the current symbol. This also
    marks this documentation (for ambiguity warnings). *)
val symbol_docs : unit -> docs
val symbol_docs_lazy : unit -> docs Lazy.t

(** Fetch the item documentation for the symbols between two
    positions. This also marks this documentation (for ambiguity
    warnings). *)
val rhs_docs : int -> int -> docs
val rhs_docs_lazy : int -> int -> docs Lazy.t

(** Mark the item documentation for the current symbol (for ambiguity
    warnings). *)
val mark_symbol_docs : unit -> unit

(** Mark as associated the item documentation for the symbols between
    two positions (for ambiguity warnings) *)
val mark_rhs_docs : int -> int -> unit

(** {3 Fields and constructors}

    The {!info} type represents documentation attached to a field or
    constructor. *)

type info = docstring option

val empty_info : info

val info_attr : docstring -> Parsetree.attribute

(** Convert field info to attributes and add them to an
    attribute list *)
val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the field info for the current symbol. *)
val symbol_info : unit -> info

(** Fetch the field info following the symbol at a given position. *)
val rhs_info : int -> info

(** {3 Unattached comments}

    The {!text} type represents documentation which is not attached to
    anything. *)

type text = docstring list

val empty_text : text

val text_attr : docstring -> Parsetree.attribute

(** Convert text to attributes and add them to an attribute list *)
val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the text preceding the current symbol. *)
val symbol_text : unit -> text
val symbol_text_lazy : unit -> text Lazy.t

(** Fetch the text preceding the symbol at the given position. *)
val rhs_text : int -> text
val rhs_text_lazy : int -> text Lazy.t

(** {3 Extra text}

    There may be additional text attached to the delimiters of a block
    (e.g. [struct] and [end]). This is fetched by the following
    functions, which are applied to the contents of the block rather
    than the delimiters. *)

(** Fetch additional text preceding the current symbol *)
val symbol_pre_extra_text : unit -> text

(** Fetch additional text following the current symbol *)
val symbol_post_extra_text : unit -> text

(** Fetch additional text preceding the symbol at the given position *)
val rhs_pre_extra_text : int -> text

(** Fetch additional text following the symbol at the given position *)
val rhs_post_extra_text : int -> text

end = struct
#1 "docstrings.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                              Leo White                              *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Location

(* Docstrings *)

(* A docstring is "attached" if it has been inserted in the AST. This
   is used for generating unexpected docstring warnings. *)
type ds_attached =
  | Unattached   (* Not yet attached anything.*)
  | Info         (* Attached to a field or constructor. *)
  | Docs         (* Attached to an item or as floating text. *)

(* A docstring is "associated" with an item if there are no blank lines between
   them. This is used for generating docstring ambiguity warnings. *)
type ds_associated =
  | Zero             (* Not associated with an item *)
  | One              (* Associated with one item *)
  | Many             (* Associated with multiple items (ambiguity) *)

type docstring =
  { ds_body: string;
    ds_loc: Location.t;
    mutable ds_attached: ds_attached;
    mutable ds_associated: ds_associated; }

(* List of docstrings *)

let docstrings : docstring list ref = ref []

(* Warn for unused and ambiguous docstrings *)

let warn_bad_docstrings () =
  if Warnings.is_active (Warnings.Bad_docstring true) then begin
    List.iter
      (fun ds ->
         match ds.ds_attached with
         | Info -> ()
         | Unattached ->
           prerr_warning ds.ds_loc (Warnings.Bad_docstring true)
         | Docs ->
             match ds.ds_associated with
             | Zero | One -> ()
             | Many ->
               prerr_warning ds.ds_loc (Warnings.Bad_docstring false))
      (List.rev !docstrings)
end

(* Docstring constructors and descturctors *)

let docstring body loc =
  let ds =
    { ds_body = body;
      ds_loc = loc;
      ds_attached = Unattached;
      ds_associated = Zero; }
  in
  docstrings := ds :: !docstrings;
  ds

let docstring_body ds = ds.ds_body

let docstring_loc ds = ds.ds_loc

(* Docstrings attached to items *)

type docs =
  { docs_pre: docstring option;
    docs_post: docstring option; }

let empty_docs = { docs_pre = None; docs_post = None }

let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

let docs_attr ds =
  let open Asttypes in
  let open Parsetree in
  let exp =
    { pexp_desc = Pexp_constant (Const_string(ds.ds_body, None));
      pexp_loc = ds.ds_loc;
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
  in
    (doc_loc, PStr [item])

let add_docs_attrs docs attrs =
  let attrs =
    match docs.docs_pre with
    | None -> attrs
    | Some ds -> docs_attr ds :: attrs
  in
  let attrs =
    match docs.docs_post with
    | None -> attrs
    | Some ds -> attrs @ [docs_attr ds]
  in
  attrs

(* Docstrings attached to consturctors or fields *)

type info = docstring option

let empty_info = None

let info_attr = docs_attr

let add_info_attrs info attrs =
  let attrs =
    match info with
    | None -> attrs
    | Some ds -> attrs @ [info_attr ds]
  in
  attrs

(* Docstrings not attached to a specifc item *)

type text = docstring list

let empty_text = []

let text_loc = {txt = "ocaml.text"; loc = Location.none}

let text_attr ds =
  let open Asttypes in
  let open Parsetree in
  let exp =
    { pexp_desc = Pexp_constant (Const_string(ds.ds_body, None));
      pexp_loc = ds.ds_loc;
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
  in
    (text_loc, PStr [item])

let add_text_attrs dsl attrs =
  (List.map text_attr dsl) @ attrs

(* Find the first non-info docstring in a list, attach it and return it *)
let get_docstring ~info dsl =
  let rec loop = function
    | [] -> None
    | {ds_attached = Info; _} :: rest -> loop rest
    | ds :: rest ->
        ds.ds_attached <- if info then Info else Docs;
        Some ds
  in
  loop dsl

(* Find all the non-info docstrings in a list, attach them and return them *)
let get_docstrings dsl =
  let rec loop acc = function
    | [] -> List.rev acc
    | {ds_attached = Info; _} :: rest -> loop acc rest
    | ds :: rest ->
        ds.ds_attached <- Docs;
        loop (ds :: acc) rest
  in
    loop [] dsl

(* "Associate" all the docstrings in a list *)
let associate_docstrings dsl =
  List.iter
    (fun ds ->
       match ds.ds_associated with
       | Zero -> ds.ds_associated <- One
       | (One | Many) -> ds.ds_associated <- Many)
    dsl

(* Map from positions to pre docstrings *)

let pre_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_pre_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add pre_table pos dsl

let get_pre_docs pos =
  try
    let dsl = Hashtbl.find pre_table pos in
      associate_docstrings dsl;
      get_docstring ~info:false dsl
  with Not_found -> None

let mark_pre_docs pos =
  try
    let dsl = Hashtbl.find pre_table pos in
      associate_docstrings dsl
  with Not_found -> ()

(* Map from positions to post docstrings *)

let post_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_post_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add post_table pos dsl

let get_post_docs pos =
  try
    let dsl = Hashtbl.find post_table pos in
      associate_docstrings dsl;
      get_docstring ~info:false dsl
  with Not_found -> None

let mark_post_docs pos =
  try
    let dsl = Hashtbl.find post_table pos in
      associate_docstrings dsl
  with Not_found -> ()

let get_info pos =
  try
    let dsl = Hashtbl.find post_table pos in
      get_docstring ~info:true dsl
  with Not_found -> None

(* Map from positions to floating docstrings *)

let floating_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_floating_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add floating_table pos dsl

let get_text pos =
  try
    let dsl = Hashtbl.find floating_table pos in
      get_docstrings dsl
  with Not_found -> []

(* Maps from positions to extra docstrings *)

let pre_extra_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_pre_extra_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add pre_extra_table pos dsl

let get_pre_extra_text pos =
  try
    let dsl = Hashtbl.find pre_extra_table pos in
      get_docstrings dsl
  with Not_found -> []

let post_extra_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_post_extra_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add post_extra_table pos dsl

let get_post_extra_text pos =
  try
    let dsl = Hashtbl.find post_extra_table pos in
      get_docstrings dsl
  with Not_found -> []

(* Docstrings from parser actions *)

let symbol_docs () =
  { docs_pre = get_pre_docs (Parsing.symbol_start_pos ());
    docs_post = get_post_docs (Parsing.symbol_end_pos ()); }

let symbol_docs_lazy () =
  let p1 = Parsing.symbol_start_pos () in
  let p2 = Parsing.symbol_end_pos () in
    lazy { docs_pre = get_pre_docs p1;
           docs_post = get_post_docs p2; }

let rhs_docs pos1 pos2 =
  { docs_pre = get_pre_docs (Parsing.rhs_start_pos pos1);
    docs_post = get_post_docs (Parsing.rhs_end_pos pos2); }

let rhs_docs_lazy pos1 pos2 =
  let p1 = Parsing.rhs_start_pos pos1 in
  let p2 = Parsing.rhs_end_pos pos2 in
    lazy { docs_pre = get_pre_docs p1;
           docs_post = get_post_docs p2; }

let mark_symbol_docs () =
  mark_pre_docs (Parsing.symbol_start_pos ());
  mark_post_docs (Parsing.symbol_end_pos ())

let mark_rhs_docs pos1 pos2 =
  mark_pre_docs (Parsing.rhs_start_pos pos1);
  mark_post_docs (Parsing.rhs_end_pos pos2)

let symbol_info () =
  get_info (Parsing.symbol_end_pos ())

let rhs_info pos =
  get_info (Parsing.rhs_end_pos pos)

let symbol_text () =
  get_text (Parsing.symbol_start_pos ())

let symbol_text_lazy () =
  let pos = Parsing.symbol_start_pos () in
    lazy (get_text pos)

let rhs_text pos =
  get_text (Parsing.rhs_start_pos pos)

let rhs_text_lazy pos =
  let pos = Parsing.rhs_start_pos pos in
    lazy (get_text pos)

let symbol_pre_extra_text () =
  get_pre_extra_text (Parsing.symbol_start_pos ())

let symbol_post_extra_text () =
  get_post_extra_text (Parsing.symbol_end_pos ())

let rhs_pre_extra_text pos =
  get_pre_extra_text (Parsing.rhs_start_pos pos)

let rhs_post_extra_text pos =
  get_post_extra_text (Parsing.rhs_end_pos pos)


(* (Re)Initialise all comment state *)

let init () =
  docstrings := [];
  Hashtbl.reset pre_table;
  Hashtbl.reset post_table;
  Hashtbl.reset floating_table;
  Hashtbl.reset pre_extra_table;
  Hashtbl.reset post_extra_table




end
module Ast_helper : sig 
#1 "ast_helper.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers to produce Parsetree fragments *)

open Parsetree
open Asttypes
open Docstrings

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list

(** {2 Default locations} *)

val default_loc: loc ref
    (** Default value for all optional location arguments. *)

val with_default_loc: loc -> (unit -> 'a) -> 'a
    (** Set the [default_loc] within the scope of the execution
        of the provided function. *)

(** {2 Core language} *)

(** Type expressions *)
module Typ :
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
    val attr: core_type -> attribute -> core_type

    val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
    val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
    val arrow: ?loc:loc -> ?attrs:attrs -> label -> core_type -> core_type
               -> core_type
    val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val object_: ?loc:loc -> ?attrs:attrs ->
                  (string * attributes * core_type) list -> closed_flag ->
                  core_type
    val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
    val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
                 -> label list option -> core_type
    val poly: ?loc:loc -> ?attrs:attrs -> string list -> core_type -> core_type
    val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
                 -> core_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

    val force_poly: core_type -> core_type
  end

(** Patterns *)
module Pat:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
    val attr:pattern -> attribute -> pattern

    val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
    val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
    val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
    val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
    val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
    val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
    val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
                -> pattern
    val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
    val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
    val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
    val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
    val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val unpack: ?loc:loc -> ?attrs:attrs -> str -> pattern
    val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
  end

(** Expressions *)
module Exp:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
    val attr: expression -> attribute -> expression

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
              -> expression -> expression
    val fun_: ?loc:loc -> ?attrs:attrs -> label -> expression option -> pattern
              -> expression -> expression
    val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
    val apply: ?loc:loc -> ?attrs:attrs -> expression
               -> (label * expression) list -> expression
    val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
                -> expression
    val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
    val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
                   -> expression
    val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
                 -> expression
    val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
                -> expression option -> expression
    val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
    val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
                  -> expression
    val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
    val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                    -> expression option -> expression
    val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression
    val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
                -> expression
    val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
              -> direction_flag -> expression -> expression
    val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                -> core_type -> expression
    val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
                     -> expression
    val send: ?loc:loc -> ?attrs:attrs -> expression -> string -> expression
    val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
    val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
                  -> expression
    val letmodule: ?loc:loc -> ?attrs:attrs -> str -> module_expr -> expression
                   -> expression
    val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option -> expression
    val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
    val newtype: ?loc:loc -> ?attrs:attrs -> string -> expression -> expression
    val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
    val open_: ?loc:loc -> ?attrs:attrs -> override_flag -> lid -> expression -> expression
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression

    val case: pattern -> ?guard:expression -> expression -> case
  end

(** Value declarations *)
module Val:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?prim:string list -> str -> core_type -> value_description
  end

(** Type declarations *)
module Type:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?params:(core_type * variance) list -> ?cstrs:(core_type * core_type * loc) list ->
      ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
      type_declaration

    val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?args:core_type list -> ?res:core_type -> str -> constructor_declaration
    val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?mut:mutable_flag -> str -> core_type -> label_declaration
  end

(** Type extensions *)
module Te:
  sig
    val mk: ?attrs:attrs -> ?docs:docs ->
      ?params:(core_type * variance) list -> ?priv:private_flag ->
      lid -> extension_constructor list -> type_extension

    val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> extension_constructor_kind -> extension_constructor

    val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      ?args:core_type list -> ?res:core_type -> str -> extension_constructor
    val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> lid -> extension_constructor
  end

(** {2 Module language} *)

(** Module type expressions *)
module Mty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
    val attr: module_type -> attribute -> module_type

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
    val functor_: ?loc:loc -> ?attrs:attrs ->
      str -> module_type option -> module_type -> module_type
    val with_: ?loc:loc -> ?attrs:attrs -> module_type -> with_constraint list -> module_type
    val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
  end

(** Module expressions *)
module Mod:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
    val attr: module_expr -> attribute -> module_expr

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
    val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
    val functor_: ?loc:loc -> ?attrs:attrs ->
      str -> module_type option -> module_expr -> module_expr
    val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr -> module_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type -> module_expr
    val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
  end

(** Signature items *)
module Sig:
  sig
    val mk: ?loc:loc -> signature_item_desc -> signature_item

    val value: ?loc:loc -> value_description -> signature_item
    val type_: ?loc:loc -> type_declaration list -> signature_item
    val type_extension: ?loc:loc -> type_extension -> signature_item
    val exception_: ?loc:loc -> extension_constructor -> signature_item
    val module_: ?loc:loc -> module_declaration -> signature_item
    val rec_module: ?loc:loc -> module_declaration list -> signature_item
    val modtype: ?loc:loc -> module_type_declaration -> signature_item
    val open_: ?loc:loc -> open_description -> signature_item
    val include_: ?loc:loc -> include_description -> signature_item
    val class_: ?loc:loc -> class_description list -> signature_item
    val class_type: ?loc:loc -> class_type_declaration list -> signature_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
    val attribute: ?loc:loc -> attribute -> signature_item
    val text: text -> signature_item list
  end

(** Structure items *)
module Str:
  sig
    val mk: ?loc:loc -> structure_item_desc -> structure_item

    val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
    val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
    val primitive: ?loc:loc -> value_description -> structure_item
    val type_: ?loc:loc -> type_declaration list -> structure_item
    val type_extension: ?loc:loc -> type_extension -> structure_item
    val exception_: ?loc:loc -> extension_constructor -> structure_item
    val module_: ?loc:loc -> module_binding -> structure_item
    val rec_module: ?loc:loc -> module_binding list -> structure_item
    val modtype: ?loc:loc -> module_type_declaration -> structure_item
    val open_: ?loc:loc -> open_description -> structure_item
    val class_: ?loc:loc -> class_declaration list -> structure_item
    val class_type: ?loc:loc -> class_type_declaration list -> structure_item
    val include_: ?loc:loc -> include_declaration -> structure_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
    val attribute: ?loc:loc -> attribute -> structure_item
    val text: text -> structure_item list
  end

(** Module declarations *)
module Md:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> module_type -> module_declaration
  end

(** Module type declarations *)
module Mtd:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?typ:module_type -> str -> module_type_declaration
  end

(** Module bindings *)
module Mb:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> module_expr -> module_binding
  end

(* Opens *)
module Opn:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
      ?override:override_flag -> lid -> open_description
  end

(* Includes *)
module Incl:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
  end

(** Value bindings *)

module Vb:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      pattern -> expression -> value_binding
  end


(** {2 Class language} *)

(** Class type expressions *)
module Cty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
    val attr: class_type -> attribute -> class_type

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
    val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
    val arrow: ?loc:loc -> ?attrs:attrs -> label -> core_type -> class_type -> class_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
  end

(** Class type fields *)
module Ctf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      class_type_field_desc -> class_type_field
    val attr: class_type_field -> attribute -> class_type_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
    val val_: ?loc:loc -> ?attrs:attrs -> string -> mutable_flag -> virtual_flag -> core_type -> class_type_field
    val method_: ?loc:loc -> ?attrs:attrs -> string -> private_flag -> virtual_flag -> core_type -> class_type_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type -> class_type_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
    val attribute: ?loc:loc -> attribute -> class_type_field
    val text: text -> class_type_field list
  end

(** Class expressions *)
module Cl:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
    val attr: class_expr -> attribute -> class_expr

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
    val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
    val fun_: ?loc:loc -> ?attrs:attrs -> label -> expression option -> pattern -> class_expr -> class_expr
    val apply: ?loc:loc -> ?attrs:attrs -> class_expr -> (label * expression) list -> class_expr
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list -> class_expr -> class_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type -> class_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
  end

(** Class fields *)
module Cf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc -> class_field
    val attr: class_field -> attribute -> class_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr -> string option -> class_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag -> class_field_kind -> class_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag -> class_field_kind -> class_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type -> class_field
    val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
    val attribute: ?loc:loc -> attribute -> class_field
    val text: text -> class_field list

    val virtual_: core_type -> class_field_kind
    val concrete: override_flag -> expression -> class_field_kind

  end

(** Classes *)
module Ci:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?virt:virtual_flag -> ?params:(core_type * variance) list ->
      str -> 'a -> 'a class_infos
  end

(** Class signatures *)
module Csig:
  sig
    val mk: core_type -> class_type_field list -> class_signature
  end

(** Class structures *)
module Cstr:
  sig
    val mk: pattern -> class_field list -> class_structure
  end

end = struct
#1 "ast_helper.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree
open Docstrings

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list

let default_loc = ref Location.none

let with_default_loc l f =
  let old = !default_loc in
  default_loc := l;
  try let r = f () in default_loc := old; r
  with exn -> default_loc := old; raise exn

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
  let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
  let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
  let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
  let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
  let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
  let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

  let force_poly t =
    match t.ptyp_desc with
    | Ptyp_poly _ -> t
    | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)
end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
  let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
  let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
  let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
  let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
  let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
  let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
  let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
  let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
end

module Exp = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
  let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
  let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
  let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
  let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
  let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
  let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
  let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
  let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
  let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
  let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
  let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
  let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
  let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
  let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
  let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
  let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
  let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
  let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
  let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
  let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
  let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
  let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)

  let case lhs ?guard rhs =
    {
     pc_lhs = lhs;
     pc_guard = guard;
     pc_rhs = rhs;
    }
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
  let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
  let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
  let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
end

module Mod = struct
let mk ?(loc = !default_loc) ?(attrs = []) d =
  {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
  let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

  let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
  let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
  let functor_ ?loc ?attrs arg arg_ty body =
    mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
  let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
  let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
  let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
end

module Sig = struct
  let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc a = mk ?loc (Psig_type a)
  let type_extension ?loc a = mk ?loc (Psig_typext a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
  let rec_module ?loc a = mk ?loc (Psig_recmodule a)
  let modtype ?loc a = mk ?loc (Psig_modtype a)
  let open_ ?loc a = mk ?loc (Psig_open a)
  let include_ ?loc a = mk ?loc (Psig_include a)
  let class_ ?loc a = mk ?loc (Psig_class a)
  let class_type ?loc a = mk ?loc (Psig_class_type a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Psig_attribute a)
  let text txt =
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      txt
end

module Str = struct
  let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

  let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
  let value ?loc a b = mk ?loc (Pstr_value (a, b))
  let primitive ?loc a = mk ?loc (Pstr_primitive a)
  let type_ ?loc a = mk ?loc (Pstr_type a)
  let type_extension ?loc a = mk ?loc (Pstr_typext a)
  let exception_ ?loc a = mk ?loc (Pstr_exception a)
  let module_ ?loc a = mk ?loc (Pstr_module a)
  let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
  let modtype ?loc a = mk ?loc (Pstr_modtype a)
  let open_ ?loc a = mk ?loc (Pstr_open a)
  let class_ ?loc a = mk ?loc (Pstr_class a)
  let class_type ?loc a = mk ?loc (Pstr_class_type a)
  let include_ ?loc a = mk ?loc (Pstr_include a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Pstr_attribute a)
  let text txt =
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      txt
end

module Cl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcl_desc = d;
     pcl_loc = loc;
     pcl_attributes = attrs;
    }
  let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
  let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
end

module Cty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcty_desc = d;
     pcty_loc = loc;
     pcty_attributes = attrs;
    }
  let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
end

module Ctf = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
           ?(docs = empty_docs) d =
    {
     pctf_desc = d;
     pctf_loc = loc;
     pctf_attributes = add_docs_attrs docs attrs;
    }

  let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
  let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
  let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
  let attribute ?loc a = mk ?loc (Pctf_attribute a)
  let text txt =
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      txt

  let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

end

module Cf = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) d =
    {
     pcf_desc = d;
     pcf_loc = loc;
     pcf_attributes = add_docs_attrs docs attrs;
    }

  let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
  let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
  let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
  let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
  let attribute ?loc a = mk ?loc (Pcf_attribute a)
  let text txt =
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      txt

  let virtual_ ct = Cfk_virtual ct
  let concrete o e = Cfk_concrete (o, e)

  let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

end

module Val = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(prim = []) name typ =
    {
     pval_name = name;
     pval_type = typ;
     pval_attributes = add_docs_attrs docs attrs;
     pval_loc = loc;
     pval_prim = prim;
    }
end

module Md = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name typ =
    {
     pmd_name = name;
     pmd_type = typ;
     pmd_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmd_loc = loc;
    }
end

module Mtd = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) ?typ name =
    {
     pmtd_name = name;
     pmtd_type = typ;
     pmtd_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmtd_loc = loc;
    }
end

module Mb = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name expr =
    {
     pmb_name = name;
     pmb_expr = expr;
     pmb_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmb_loc = loc;
    }
end

module Opn = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(override = Fresh) lid =
    {
     popen_lid = lid;
     popen_override = override;
     popen_loc = loc;
     popen_attributes = add_docs_attrs docs attrs;
    }
end

module Incl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
    {
     pincl_mod = mexpr;
     pincl_loc = loc;
     pincl_attributes = add_docs_attrs docs attrs;
    }

end

module Vb = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(text = []) pat expr =
    {
     pvb_pat = pat;
     pvb_expr = expr;
     pvb_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pvb_loc = loc;
    }
end

module Ci = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
        ?(virt = Concrete) ?(params = []) name expr =
    {
     pci_virt = virt;
     pci_params = params;
     pci_name = name;
     pci_expr = expr;
     pci_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pci_loc = loc;
    }
end

module Type = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
      ?(params = [])
      ?(cstrs = [])
      ?(kind = Ptype_abstract)
      ?(priv = Public)
      ?manifest
      name =
    {
     ptype_name = name;
     ptype_params = params;
     ptype_cstrs = cstrs;
     ptype_kind = kind;
     ptype_private = priv;
     ptype_manifest = manifest;
     ptype_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     ptype_loc = loc;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(args = []) ?res name =
    {
     pcd_name = name;
     pcd_args = args;
     pcd_res = res;
     pcd_loc = loc;
     pcd_attributes = add_info_attrs info attrs;
    }

  let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(mut = Immutable) name typ =
    {
     pld_name = name;
     pld_mutable = mut;
     pld_type = typ;
     pld_loc = loc;
     pld_attributes = add_info_attrs info attrs;
    }

end

(** Type extensions *)
module Te = struct
  let mk ?(attrs = []) ?(docs = empty_docs)
        ?(params = []) ?(priv = Public) path constructors =
    {
     ptyext_path = path;
     ptyext_params = params;
     ptyext_constructors = constructors;
     ptyext_private = priv;
     ptyext_attributes = add_docs_attrs docs attrs;
    }

  let constructor ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name kind =
    {
     pext_name = name;
     pext_kind = kind;
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

  let decl ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) ?(args = []) ?res name =
    {
     pext_name = name;
     pext_kind = Pext_decl(args, res);
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

  let rebind ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name lid =
    {
     pext_name = name;
     pext_kind = Pext_rebind lid;
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

end

module Csig = struct
  let mk self fields =
    {
     pcsig_self = self;
     pcsig_fields = fields;
    }
end

module Cstr = struct
  let mk self fields =
    {
     pcstr_self = self;
     pcstr_fields = fields;
    }
end


end
module Ast_convenience : sig 
#1 "ast_convenience.mli"
(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(** {1 Convenience functions to help build and deconstruct AST fragments.} *)

open Parsetree
open Asttypes
open Ast_helper

(** {2 Compatibility modules} *)

module Label : sig
  type t = string

  type desc =
      Nolabel
    | Labelled of string
    | Optional of string

  val explode : t -> desc

  val nolabel : t
  val labelled : string -> t
  val optional : string -> t

end

(** {2 Provides abstraction over Asttypes.constant type }*)
module Constant : sig 
  type t = 
     Pconst_integer of string * char option 
   | Pconst_char of char 
   | Pconst_string of string * string option 
   | Pconst_float of string * char option 

  exception Unknown_literal of string * char

  (** Converts Asttypes.constant to Constant.t *)
  val of_constant : constant -> t

  (** Converts Constant.t to Asttypes.constant. Raises Unknown_literal if conversion fails *)
  val to_constant : t -> constant 
end

(** {2 Misc} *)

val lid: ?loc:loc -> string -> lid

(** {2 Expressions} *)

val evar: ?loc:loc -> ?attrs:attrs -> string -> expression
val let_in: ?loc:loc -> ?attrs:attrs -> ?recursive:bool -> value_binding list -> expression -> expression

val constr: ?loc:loc -> ?attrs:attrs -> string -> expression list -> expression
val record: ?loc:loc -> ?attrs:attrs -> ?over:expression -> (string * expression) list -> expression
val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression

val nil: ?loc:loc -> ?attrs:attrs -> unit -> expression
val cons: ?loc:loc -> ?attrs:attrs -> expression -> expression -> expression
val list: ?loc:loc -> ?attrs:attrs -> expression list -> expression

val unit: ?loc:loc -> ?attrs:attrs -> unit -> expression

val func: ?loc:loc -> ?attrs:attrs -> (pattern * expression) list -> expression
val lam: ?loc:loc -> ?attrs:attrs -> ?label:Label.t -> ?default:expression -> pattern -> expression -> expression
val app: ?loc:loc -> ?attrs:attrs -> expression -> expression list -> expression

val str: ?loc:loc -> ?attrs:attrs -> string -> expression
val int: ?loc:loc -> ?attrs:attrs -> int -> expression
val int32: ?loc:loc -> ?attrs:attrs -> int32 -> expression
val int64: ?loc:loc -> ?attrs:attrs -> int64 -> expression
val char: ?loc:loc -> ?attrs:attrs -> char -> expression
val float: ?loc:loc -> ?attrs:attrs -> float -> expression

val sequence: ?loc:loc -> ?attrs:attrs -> expression list -> expression
(** Return [()] if the list is empty. Tail rec. *)

(** {2 Patterns} *)

val pvar: ?loc:loc -> ?attrs:attrs -> string -> pattern
val pconstr: ?loc:loc -> ?attrs:attrs -> string -> pattern list -> pattern
val precord: ?loc:loc -> ?attrs:attrs -> ?closed:closed_flag -> (string * pattern) list -> pattern
val ptuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern

val pnil: ?loc:loc -> ?attrs:attrs -> unit -> pattern
val pcons: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
val plist: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern

val pstr: ?loc:loc -> ?attrs:attrs -> string -> pattern
val pint: ?loc:loc -> ?attrs:attrs -> int -> pattern
val pchar: ?loc:loc -> ?attrs:attrs -> char -> pattern
val pfloat: ?loc:loc -> ?attrs:attrs -> float -> pattern

val punit: ?loc:loc -> ?attrs:attrs -> unit -> pattern


(** {2 Types} *)

val tconstr: ?loc:loc -> ?attrs:attrs -> string -> core_type list -> core_type

(** {2 AST deconstruction} *)

val get_str: expression -> string option
val get_str_with_quotation_delimiter: expression -> (string * string option) option
val get_lid: expression -> string option

val has_attr: string -> attributes -> bool
val find_attr: string -> attributes -> payload option
val find_attr_expr: string -> attributes -> expression option

end = struct
#1 "ast_convenience.ml"
(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

open Parsetree
open Asttypes
open Location
open Ast_helper


module Label = struct

  type t = string

  type desc =
      Nolabel
    | Labelled of string
    | Optional of string

  let explode s =
    if s = "" then Nolabel
    else if s.[0] = '?' then Optional (String.sub s 1 (String.length s - 1))
    else Labelled s

  let nolabel = ""
  let labelled s = s
  let optional s = "?"^s

end

module Constant = struct
  type t =
     Pconst_integer of string * char option
   | Pconst_char of char
   | Pconst_string of string * string option
   | Pconst_float of string * char option

  exception Unknown_literal of string * char

  (** Backport Int_literal_converter from ocaml 4.03 -
   * https://github.com/ocaml/ocaml/blob/trunk/utils/misc.ml#L298 *)
  module Int_literal_converter = struct
    let cvt_int_aux str neg of_string =
      if String.length str = 0 || str.[0] = '-'
      then of_string str
      else neg (of_string ("-" ^ str))
    let int s = cvt_int_aux s (~-) int_of_string
    let int32 s = cvt_int_aux s Int32.neg Int32.of_string
    let int64 s = cvt_int_aux s Int64.neg Int64.of_string
    let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
  end

  let of_constant = function
    | Asttypes.Const_int32(i) -> Pconst_integer(Int32.to_string i, Some 'l')
    | Asttypes.Const_int64(i) -> Pconst_integer(Int64.to_string i, Some 'L')
    | Asttypes.Const_nativeint(i) -> Pconst_integer(Nativeint.to_string i, Some 'n')
    | Asttypes.Const_int(i) -> Pconst_integer(string_of_int i, None)
    | Asttypes.Const_char c -> Pconst_char c
    | Asttypes.Const_string(s, s_opt) -> Pconst_string(s, s_opt)
    | Asttypes.Const_float f -> Pconst_float(f, None)

  let to_constant = function
    | Pconst_integer(i,Some 'l') -> Asttypes.Const_int32 (Int_literal_converter.int32 i)
    | Pconst_integer(i,Some 'L') -> Asttypes.Const_int64 (Int_literal_converter.int64 i)
    | Pconst_integer(i,Some 'n') -> Asttypes.Const_nativeint (Int_literal_converter.nativeint i)
    | Pconst_integer(i,None) -> Asttypes.Const_int (Int_literal_converter.int i)
    | Pconst_integer(i,Some c) -> raise (Unknown_literal (i, c))
    | Pconst_char c -> Asttypes.Const_char c
    | Pconst_string(s,d) -> Asttypes.Const_string(s, d)
    | Pconst_float(f,None) -> Asttypes.Const_float f
    | Pconst_float(f,Some c) -> raise (Unknown_literal (f, c))
end

let may_tuple ?loc tup = function
  | [] -> None
  | [x] -> Some x
  | l -> Some (tup ?loc ?attrs:None l)

let lid ?(loc = !default_loc) s = mkloc (Longident.parse s) loc
let constr ?loc ?attrs s args = Exp.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Exp.tuple args)
let nil ?loc ?attrs () = constr ?loc ?attrs "[]" []
let unit ?loc ?attrs () = constr ?loc ?attrs "()" []
let tuple ?loc ?attrs = function
  | [] -> unit ?loc ?attrs ()
  | [x] -> x
  | xs -> Exp.tuple ?loc ?attrs xs
let cons ?loc ?attrs hd tl = constr ?loc ?attrs "::" [hd; tl]
let list ?loc ?attrs l = List.fold_right (cons ?loc ?attrs) l (nil ?loc ?attrs ())
let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Const_string (s, None))
let int ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int x)
let int32 ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int32 x)
let int64 ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int64 x)
let char ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_char x)
let float ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_float (string_of_float x))
let record ?loc ?attrs ?over l =
  Exp.record ?loc ?attrs (List.map (fun (s, e) -> (lid ~loc:e.pexp_loc s, e)) l) over
let func ?loc ?attrs l = Exp.function_ ?loc ?attrs (List.map (fun (p, e) -> Exp.case p e) l)
let lam ?loc ?attrs ?(label = Label.nolabel) ?default pat exp = Exp.fun_ ?loc ?attrs label default pat exp
let app ?loc ?attrs f l = if l = [] then f else Exp.apply ?loc ?attrs f (List.map (fun a -> Label.nolabel, a) l)
let evar ?loc ?attrs s = Exp.ident ?loc ?attrs (lid ?loc s)
let let_in ?loc ?attrs ?(recursive = false) b body =
  Exp.let_ ?loc ?attrs (if recursive then Recursive else Nonrecursive) b body

let sequence ?loc ?attrs = function
  | [] -> unit ?loc ?attrs ()
  | hd :: tl -> List.fold_left (fun e1 e2 -> Exp.sequence ?loc ?attrs e1 e2) hd tl

let pvar ?(loc = !default_loc) ?attrs s = Pat.var ~loc ?attrs (mkloc s loc)
let pconstr ?loc ?attrs s args = Pat.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Pat.tuple args)
let precord ?loc ?attrs ?(closed = Open) l =
  Pat.record ?loc ?attrs (List.map (fun (s, e) -> (lid ~loc:e.ppat_loc s, e)) l) closed
let pnil ?loc ?attrs () = pconstr ?loc ?attrs "[]" []
let pcons ?loc ?attrs hd tl = pconstr ?loc ?attrs "::" [hd; tl]
let punit ?loc ?attrs () = pconstr ?loc ?attrs "()" []
let ptuple ?loc ?attrs = function
  | [] -> punit ?loc ?attrs ()
  | [x] -> x
  | xs -> Pat.tuple ?loc ?attrs xs
let plist ?loc ?attrs l = List.fold_right (pcons ?loc ?attrs) l (pnil ?loc ?attrs ())

let pstr ?loc ?attrs s = Pat.constant ?loc ?attrs (Const_string (s, None))
let pint ?loc ?attrs x = Pat.constant ?loc ?attrs (Const_int x)
let pchar ?loc ?attrs x = Pat.constant ?loc ?attrs (Const_char x)
let pfloat ?loc ?attrs x = Pat.constant ?loc ?attrs (Const_float (string_of_float x))

let tconstr ?loc ?attrs c l = Typ.constr ?loc ?attrs (lid ?loc c) l

let get_str = function
  | {pexp_desc=Pexp_constant (Const_string (s, _)); _} -> Some s
  | _ -> None

let get_str_with_quotation_delimiter = function
  | {pexp_desc=Pexp_constant (Const_string (s, d)); _} -> Some (s, d)
  | _ -> None

let get_lid = function
  | {pexp_desc=Pexp_ident{txt=id;_};_} ->
      Some (String.concat "." (Longident.flatten id))
  | _ -> None

let find_attr s attrs =
  try Some (snd (List.find (fun (x, _) -> x.txt = s) attrs))
  with Not_found -> None

let expr_of_payload = function
  | PStr [{pstr_desc=Pstr_eval(e, _); _}] -> Some e
  | _ -> None

let find_attr_expr s attrs =
  match find_attr s attrs with
  | Some e -> expr_of_payload e
  | None -> None

let has_attr s attrs =
  find_attr s attrs <> None

end
module Ast_lifter
= struct
#1 "ast_lifter.ml"
class virtual ['res] lifter =
  object (this)
    method lift_Parsetree_expression : Parsetree.expression -> 'res=
      (fun
         { Parsetree.pexp_desc = pexp_desc; Parsetree.pexp_loc = pexp_loc;
           Parsetree.pexp_attributes = pexp_attributes }
          ->
         this#record "Parsetree.expression"
           [("pexp_desc", (this#lift_Parsetree_expression_desc pexp_desc));
           ("pexp_loc", (this#lift_Location_t pexp_loc));
           ("pexp_attributes",
             (this#lift_Parsetree_attributes pexp_attributes))] : Parsetree.expression
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_expression_desc :
      Parsetree.expression_desc -> 'res=
      (function
       | Parsetree.Pexp_ident x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_ident",
               [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Pexp_constant x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_constant", [this#lift_Asttypes_constant x0])
       | Parsetree.Pexp_let (x0,x1,x2) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_let",
               [this#lift_Asttypes_rec_flag x0;
               this#list (List.map this#lift_Parsetree_value_binding x1);
               this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_function x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_function",
               [this#list (List.map this#lift_Parsetree_case x0)])
       | Parsetree.Pexp_fun (x0,x1,x2,x3) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_fun",
               [this#lift_Asttypes_label x0;
               this#lift_option this#lift_Parsetree_expression x1;
               this#lift_Parsetree_pattern x2;
               this#lift_Parsetree_expression x3])
       | Parsetree.Pexp_apply (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_apply",
               [this#lift_Parsetree_expression x0;
               this#list
                 (List.map
                    (fun x  ->
                       let (x0,x1) = x in
                       this#tuple
                         [this#lift_Asttypes_label x0;
                         this#lift_Parsetree_expression x1]) x1)])
       | Parsetree.Pexp_match (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_match",
               [this#lift_Parsetree_expression x0;
               this#list (List.map this#lift_Parsetree_case x1)])
       | Parsetree.Pexp_try (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_try",
               [this#lift_Parsetree_expression x0;
               this#list (List.map this#lift_Parsetree_case x1)])
       | Parsetree.Pexp_tuple x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_tuple",
               [this#list (List.map this#lift_Parsetree_expression x0)])
       | Parsetree.Pexp_construct (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_construct",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_option this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_variant (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_variant",
               [this#lift_Asttypes_label x0;
               this#lift_option this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_record (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_record",
               [this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1) = x in
                        this#tuple
                          [this#lift_Asttypes_loc this#lift_Longident_t x0;
                          this#lift_Parsetree_expression x1]) x0);
               this#lift_option this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_field (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_field",
               [this#lift_Parsetree_expression x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1])
       | Parsetree.Pexp_setfield (x0,x1,x2) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_setfield",
               [this#lift_Parsetree_expression x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1;
               this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_array x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_array",
               [this#list (List.map this#lift_Parsetree_expression x0)])
       | Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_ifthenelse",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_expression x1;
               this#lift_option this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_sequence (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_sequence",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_while (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_while",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_for",
               [this#lift_Parsetree_pattern x0;
               this#lift_Parsetree_expression x1;
               this#lift_Parsetree_expression x2;
               this#lift_Asttypes_direction_flag x3;
               this#lift_Parsetree_expression x4])
       | Parsetree.Pexp_constraint (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_constraint",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_core_type x1])
       | Parsetree.Pexp_coerce (x0,x1,x2) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_coerce",
               [this#lift_Parsetree_expression x0;
               this#lift_option this#lift_Parsetree_core_type x1;
               this#lift_Parsetree_core_type x2])
       | Parsetree.Pexp_send (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_send",
               [this#lift_Parsetree_expression x0; this#string x1])
       | Parsetree.Pexp_new x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_new", [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Pexp_setinstvar (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_setinstvar",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_override x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_override",
               [this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1) = x in
                        this#tuple
                          [this#lift_Asttypes_loc this#string x0;
                          this#lift_Parsetree_expression x1]) x0)])
       | Parsetree.Pexp_letmodule (x0,x1,x2) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_letmodule",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_Parsetree_module_expr x1;
               this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_assert x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_assert", [this#lift_Parsetree_expression x0])
       | Parsetree.Pexp_lazy x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_lazy", [this#lift_Parsetree_expression x0])
       | Parsetree.Pexp_poly (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_poly",
               [this#lift_Parsetree_expression x0;
               this#lift_option this#lift_Parsetree_core_type x1])
       | Parsetree.Pexp_object x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_object", [this#lift_Parsetree_class_structure x0])
       | Parsetree.Pexp_newtype (x0,x1) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_newtype",
               [this#string x0; this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_pack x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_pack", [this#lift_Parsetree_module_expr x0])
       | Parsetree.Pexp_open (x0,x1,x2) ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_open",
               [this#lift_Asttypes_override_flag x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1;
               this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_extension x0 ->
           this#constr "Parsetree.expression_desc"
             ("Pexp_extension", [this#lift_Parsetree_extension x0]) : 
      Parsetree.expression_desc -> 'res)
    method lift_Asttypes_direction_flag : Asttypes.direction_flag -> 'res=
      (function
       | Asttypes.Upto  -> this#constr "Asttypes.direction_flag" ("Upto", [])
       | Asttypes.Downto  ->
           this#constr "Asttypes.direction_flag" ("Downto", []) : Asttypes.direction_flag
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_case : Parsetree.case -> 'res=
      (fun
         { Parsetree.pc_lhs = pc_lhs; Parsetree.pc_guard = pc_guard;
           Parsetree.pc_rhs = pc_rhs }
          ->
         this#record "Parsetree.case"
           [("pc_lhs", (this#lift_Parsetree_pattern pc_lhs));
           ("pc_guard",
             (this#lift_option this#lift_Parsetree_expression pc_guard));
           ("pc_rhs", (this#lift_Parsetree_expression pc_rhs))] : Parsetree.case
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_value_binding : Parsetree.value_binding -> 'res=
      (fun
         { Parsetree.pvb_pat = pvb_pat; Parsetree.pvb_expr = pvb_expr;
           Parsetree.pvb_attributes = pvb_attributes;
           Parsetree.pvb_loc = pvb_loc }
          ->
         this#record "Parsetree.value_binding"
           [("pvb_pat", (this#lift_Parsetree_pattern pvb_pat));
           ("pvb_expr", (this#lift_Parsetree_expression pvb_expr));
           ("pvb_attributes",
             (this#lift_Parsetree_attributes pvb_attributes));
           ("pvb_loc", (this#lift_Location_t pvb_loc))] : Parsetree.value_binding
                                                            -> 'res)
    method lift_Parsetree_pattern : Parsetree.pattern -> 'res=
      (fun
         { Parsetree.ppat_desc = ppat_desc; Parsetree.ppat_loc = ppat_loc;
           Parsetree.ppat_attributes = ppat_attributes }
          ->
         this#record "Parsetree.pattern"
           [("ppat_desc", (this#lift_Parsetree_pattern_desc ppat_desc));
           ("ppat_loc", (this#lift_Location_t ppat_loc));
           ("ppat_attributes",
             (this#lift_Parsetree_attributes ppat_attributes))] : Parsetree.pattern
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_pattern_desc : Parsetree.pattern_desc -> 'res=
      (function
       | Parsetree.Ppat_any  ->
           this#constr "Parsetree.pattern_desc" ("Ppat_any", [])
       | Parsetree.Ppat_var x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_var", [this#lift_Asttypes_loc this#string x0])
       | Parsetree.Ppat_alias (x0,x1) ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_alias",
               [this#lift_Parsetree_pattern x0;
               this#lift_Asttypes_loc this#string x1])
       | Parsetree.Ppat_constant x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_constant", [this#lift_Asttypes_constant x0])
       | Parsetree.Ppat_interval (x0,x1) ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_interval",
               [this#lift_Asttypes_constant x0;
               this#lift_Asttypes_constant x1])
       | Parsetree.Ppat_tuple x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_tuple",
               [this#list (List.map this#lift_Parsetree_pattern x0)])
       | Parsetree.Ppat_construct (x0,x1) ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_construct",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_option this#lift_Parsetree_pattern x1])
       | Parsetree.Ppat_variant (x0,x1) ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_variant",
               [this#lift_Asttypes_label x0;
               this#lift_option this#lift_Parsetree_pattern x1])
       | Parsetree.Ppat_record (x0,x1) ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_record",
               [this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1) = x in
                        this#tuple
                          [this#lift_Asttypes_loc this#lift_Longident_t x0;
                          this#lift_Parsetree_pattern x1]) x0);
               this#lift_Asttypes_closed_flag x1])
       | Parsetree.Ppat_array x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_array",
               [this#list (List.map this#lift_Parsetree_pattern x0)])
       | Parsetree.Ppat_or (x0,x1) ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_or",
               [this#lift_Parsetree_pattern x0;
               this#lift_Parsetree_pattern x1])
       | Parsetree.Ppat_constraint (x0,x1) ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_constraint",
               [this#lift_Parsetree_pattern x0;
               this#lift_Parsetree_core_type x1])
       | Parsetree.Ppat_type x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_type", [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Ppat_lazy x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_lazy", [this#lift_Parsetree_pattern x0])
       | Parsetree.Ppat_unpack x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_unpack", [this#lift_Asttypes_loc this#string x0])
       | Parsetree.Ppat_exception x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_exception", [this#lift_Parsetree_pattern x0])
       | Parsetree.Ppat_extension x0 ->
           this#constr "Parsetree.pattern_desc"
             ("Ppat_extension", [this#lift_Parsetree_extension x0]) : 
      Parsetree.pattern_desc -> 'res)
    method lift_Parsetree_core_type : Parsetree.core_type -> 'res=
      (fun
         { Parsetree.ptyp_desc = ptyp_desc; Parsetree.ptyp_loc = ptyp_loc;
           Parsetree.ptyp_attributes = ptyp_attributes }
          ->
         this#record "Parsetree.core_type"
           [("ptyp_desc", (this#lift_Parsetree_core_type_desc ptyp_desc));
           ("ptyp_loc", (this#lift_Location_t ptyp_loc));
           ("ptyp_attributes",
             (this#lift_Parsetree_attributes ptyp_attributes))] : Parsetree.core_type
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_core_type_desc : Parsetree.core_type_desc -> 'res=
      (function
       | Parsetree.Ptyp_any  ->
           this#constr "Parsetree.core_type_desc" ("Ptyp_any", [])
       | Parsetree.Ptyp_var x0 ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_var", [this#string x0])
       | Parsetree.Ptyp_arrow (x0,x1,x2) ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_arrow",
               [this#lift_Asttypes_label x0;
               this#lift_Parsetree_core_type x1;
               this#lift_Parsetree_core_type x2])
       | Parsetree.Ptyp_tuple x0 ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_tuple",
               [this#list (List.map this#lift_Parsetree_core_type x0)])
       | Parsetree.Ptyp_constr (x0,x1) ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_constr",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#list (List.map this#lift_Parsetree_core_type x1)])
       | Parsetree.Ptyp_object (x0,x1) ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_object",
               [this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1,x2) = x in
                        this#tuple
                          [this#string x0;
                          this#lift_Parsetree_attributes x1;
                          this#lift_Parsetree_core_type x2]) x0);
               this#lift_Asttypes_closed_flag x1])
       | Parsetree.Ptyp_class (x0,x1) ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_class",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#list (List.map this#lift_Parsetree_core_type x1)])
       | Parsetree.Ptyp_alias (x0,x1) ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_alias",
               [this#lift_Parsetree_core_type x0; this#string x1])
       | Parsetree.Ptyp_variant (x0,x1,x2) ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_variant",
               [this#list (List.map this#lift_Parsetree_row_field x0);
               this#lift_Asttypes_closed_flag x1;
               this#lift_option
                 (fun x  -> this#list (List.map this#lift_Asttypes_label x))
                 x2])
       | Parsetree.Ptyp_poly (x0,x1) ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_poly",
               [this#list (List.map this#string x0);
               this#lift_Parsetree_core_type x1])
       | Parsetree.Ptyp_package x0 ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_package", [this#lift_Parsetree_package_type x0])
       | Parsetree.Ptyp_extension x0 ->
           this#constr "Parsetree.core_type_desc"
             ("Ptyp_extension", [this#lift_Parsetree_extension x0]) : 
      Parsetree.core_type_desc -> 'res)
    method lift_Parsetree_package_type : Parsetree.package_type -> 'res=
      (fun x  ->
         let (x0,x1) = x in
         this#tuple
           [this#lift_Asttypes_loc this#lift_Longident_t x0;
           this#list
             (List.map
                (fun x  ->
                   let (x0,x1) = x in
                   this#tuple
                     [this#lift_Asttypes_loc this#lift_Longident_t x0;
                     this#lift_Parsetree_core_type x1]) x1)] : Parsetree.package_type
                                                                 -> 'res)
    method lift_Parsetree_row_field : Parsetree.row_field -> 'res=
      (function
       | Parsetree.Rtag (x0,x1,x2,x3) ->
           this#constr "Parsetree.row_field"
             ("Rtag",
               [this#lift_Asttypes_label x0;
               this#lift_Parsetree_attributes x1;
               this#lift_bool x2;
               this#list (List.map this#lift_Parsetree_core_type x3)])
       | Parsetree.Rinherit x0 ->
           this#constr "Parsetree.row_field"
             ("Rinherit", [this#lift_Parsetree_core_type x0]) : Parsetree.row_field
                                                                  -> 
                                                                  'res)
    method lift_Parsetree_attributes : Parsetree.attributes -> 'res=
      (fun x  -> this#list (List.map this#lift_Parsetree_attribute x) : 
      Parsetree.attributes -> 'res)
    method lift_Parsetree_attribute : Parsetree.attribute -> 'res=
      (fun x  ->
         let (x0,x1) = x in
         this#tuple
           [this#lift_Asttypes_loc this#string x0;
           this#lift_Parsetree_payload x1] : Parsetree.attribute -> 'res)
    method lift_Parsetree_payload : Parsetree.payload -> 'res=
      (function
       | Parsetree.PStr x0 ->
           this#constr "Parsetree.payload"
             ("PStr", [this#lift_Parsetree_structure x0])
       | Parsetree.PTyp x0 ->
           this#constr "Parsetree.payload"
             ("PTyp", [this#lift_Parsetree_core_type x0])
       | Parsetree.PPat (x0,x1) ->
           this#constr "Parsetree.payload"
             ("PPat",
               [this#lift_Parsetree_pattern x0;
               this#lift_option this#lift_Parsetree_expression x1]) : 
      Parsetree.payload -> 'res)
    method lift_Parsetree_structure : Parsetree.structure -> 'res=
      (fun x  -> this#list (List.map this#lift_Parsetree_structure_item x) : 
      Parsetree.structure -> 'res)
    method lift_Parsetree_structure_item : Parsetree.structure_item -> 'res=
      (fun { Parsetree.pstr_desc = pstr_desc; Parsetree.pstr_loc = pstr_loc }
          ->
         this#record "Parsetree.structure_item"
           [("pstr_desc",
              (this#lift_Parsetree_structure_item_desc pstr_desc));
           ("pstr_loc", (this#lift_Location_t pstr_loc))] : Parsetree.structure_item
                                                              -> 'res)
    method lift_Parsetree_structure_item_desc :
      Parsetree.structure_item_desc -> 'res=
      (function
       | Parsetree.Pstr_eval (x0,x1) ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_eval",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_attributes x1])
       | Parsetree.Pstr_value (x0,x1) ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_value",
               [this#lift_Asttypes_rec_flag x0;
               this#list (List.map this#lift_Parsetree_value_binding x1)])
       | Parsetree.Pstr_primitive x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_primitive", [this#lift_Parsetree_value_description x0])
       | Parsetree.Pstr_type x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_type",
               [this#list (List.map this#lift_Parsetree_type_declaration x0)])
       | Parsetree.Pstr_typext x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_typext", [this#lift_Parsetree_type_extension x0])
       | Parsetree.Pstr_exception x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_exception",
               [this#lift_Parsetree_extension_constructor x0])
       | Parsetree.Pstr_module x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_module", [this#lift_Parsetree_module_binding x0])
       | Parsetree.Pstr_recmodule x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_recmodule",
               [this#list (List.map this#lift_Parsetree_module_binding x0)])
       | Parsetree.Pstr_modtype x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_modtype",
               [this#lift_Parsetree_module_type_declaration x0])
       | Parsetree.Pstr_open x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_open", [this#lift_Parsetree_open_description x0])
       | Parsetree.Pstr_class x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_class",
               [this#list (List.map this#lift_Parsetree_class_declaration x0)])
       | Parsetree.Pstr_class_type x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_class_type",
               [this#list
                  (List.map this#lift_Parsetree_class_type_declaration x0)])
       | Parsetree.Pstr_include x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_include", [this#lift_Parsetree_include_declaration x0])
       | Parsetree.Pstr_attribute x0 ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_attribute", [this#lift_Parsetree_attribute x0])
       | Parsetree.Pstr_extension (x0,x1) ->
           this#constr "Parsetree.structure_item_desc"
             ("Pstr_extension",
               [this#lift_Parsetree_extension x0;
               this#lift_Parsetree_attributes x1]) : Parsetree.structure_item_desc
                                                       -> 'res)
    method lift_Parsetree_include_declaration :
      Parsetree.include_declaration -> 'res=
      (fun x  ->
         this#lift_Parsetree_include_infos this#lift_Parsetree_module_expr x : 
      Parsetree.include_declaration -> 'res)
    method lift_Parsetree_class_declaration :
      Parsetree.class_declaration -> 'res=
      (fun x  ->
         this#lift_Parsetree_class_infos this#lift_Parsetree_class_expr x : 
      Parsetree.class_declaration -> 'res)
    method lift_Parsetree_class_expr : Parsetree.class_expr -> 'res=
      (fun
         { Parsetree.pcl_desc = pcl_desc; Parsetree.pcl_loc = pcl_loc;
           Parsetree.pcl_attributes = pcl_attributes }
          ->
         this#record "Parsetree.class_expr"
           [("pcl_desc", (this#lift_Parsetree_class_expr_desc pcl_desc));
           ("pcl_loc", (this#lift_Location_t pcl_loc));
           ("pcl_attributes",
             (this#lift_Parsetree_attributes pcl_attributes))] : Parsetree.class_expr
                                                                   -> 
                                                                   'res)
    method lift_Parsetree_class_expr_desc :
      Parsetree.class_expr_desc -> 'res=
      (function
       | Parsetree.Pcl_constr (x0,x1) ->
           this#constr "Parsetree.class_expr_desc"
             ("Pcl_constr",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#list (List.map this#lift_Parsetree_core_type x1)])
       | Parsetree.Pcl_structure x0 ->
           this#constr "Parsetree.class_expr_desc"
             ("Pcl_structure", [this#lift_Parsetree_class_structure x0])
       | Parsetree.Pcl_fun (x0,x1,x2,x3) ->
           this#constr "Parsetree.class_expr_desc"
             ("Pcl_fun",
               [this#lift_Asttypes_label x0;
               this#lift_option this#lift_Parsetree_expression x1;
               this#lift_Parsetree_pattern x2;
               this#lift_Parsetree_class_expr x3])
       | Parsetree.Pcl_apply (x0,x1) ->
           this#constr "Parsetree.class_expr_desc"
             ("Pcl_apply",
               [this#lift_Parsetree_class_expr x0;
               this#list
                 (List.map
                    (fun x  ->
                       let (x0,x1) = x in
                       this#tuple
                         [this#lift_Asttypes_label x0;
                         this#lift_Parsetree_expression x1]) x1)])
       | Parsetree.Pcl_let (x0,x1,x2) ->
           this#constr "Parsetree.class_expr_desc"
             ("Pcl_let",
               [this#lift_Asttypes_rec_flag x0;
               this#list (List.map this#lift_Parsetree_value_binding x1);
               this#lift_Parsetree_class_expr x2])
       | Parsetree.Pcl_constraint (x0,x1) ->
           this#constr "Parsetree.class_expr_desc"
             ("Pcl_constraint",
               [this#lift_Parsetree_class_expr x0;
               this#lift_Parsetree_class_type x1])
       | Parsetree.Pcl_extension x0 ->
           this#constr "Parsetree.class_expr_desc"
             ("Pcl_extension", [this#lift_Parsetree_extension x0]) : 
      Parsetree.class_expr_desc -> 'res)
    method lift_Parsetree_class_structure :
      Parsetree.class_structure -> 'res=
      (fun
         { Parsetree.pcstr_self = pcstr_self;
           Parsetree.pcstr_fields = pcstr_fields }
          ->
         this#record "Parsetree.class_structure"
           [("pcstr_self", (this#lift_Parsetree_pattern pcstr_self));
           ("pcstr_fields",
             (this#list
                (List.map this#lift_Parsetree_class_field pcstr_fields)))] : 
      Parsetree.class_structure -> 'res)
    method lift_Parsetree_class_field : Parsetree.class_field -> 'res=
      (fun
         { Parsetree.pcf_desc = pcf_desc; Parsetree.pcf_loc = pcf_loc;
           Parsetree.pcf_attributes = pcf_attributes }
          ->
         this#record "Parsetree.class_field"
           [("pcf_desc", (this#lift_Parsetree_class_field_desc pcf_desc));
           ("pcf_loc", (this#lift_Location_t pcf_loc));
           ("pcf_attributes",
             (this#lift_Parsetree_attributes pcf_attributes))] : Parsetree.class_field
                                                                   -> 
                                                                   'res)
    method lift_Parsetree_class_field_desc :
      Parsetree.class_field_desc -> 'res=
      (function
       | Parsetree.Pcf_inherit (x0,x1,x2) ->
           this#constr "Parsetree.class_field_desc"
             ("Pcf_inherit",
               [this#lift_Asttypes_override_flag x0;
               this#lift_Parsetree_class_expr x1;
               this#lift_option this#string x2])
       | Parsetree.Pcf_val x0 ->
           this#constr "Parsetree.class_field_desc"
             ("Pcf_val",
               [(let (x0,x1,x2) = x0 in
                 this#tuple
                   [this#lift_Asttypes_loc this#string x0;
                   this#lift_Asttypes_mutable_flag x1;
                   this#lift_Parsetree_class_field_kind x2])])
       | Parsetree.Pcf_method x0 ->
           this#constr "Parsetree.class_field_desc"
             ("Pcf_method",
               [(let (x0,x1,x2) = x0 in
                 this#tuple
                   [this#lift_Asttypes_loc this#string x0;
                   this#lift_Asttypes_private_flag x1;
                   this#lift_Parsetree_class_field_kind x2])])
       | Parsetree.Pcf_constraint x0 ->
           this#constr "Parsetree.class_field_desc"
             ("Pcf_constraint",
               [(let (x0,x1) = x0 in
                 this#tuple
                   [this#lift_Parsetree_core_type x0;
                   this#lift_Parsetree_core_type x1])])
       | Parsetree.Pcf_initializer x0 ->
           this#constr "Parsetree.class_field_desc"
             ("Pcf_initializer", [this#lift_Parsetree_expression x0])
       | Parsetree.Pcf_attribute x0 ->
           this#constr "Parsetree.class_field_desc"
             ("Pcf_attribute", [this#lift_Parsetree_attribute x0])
       | Parsetree.Pcf_extension x0 ->
           this#constr "Parsetree.class_field_desc"
             ("Pcf_extension", [this#lift_Parsetree_extension x0]) : 
      Parsetree.class_field_desc -> 'res)
    method lift_Parsetree_class_field_kind :
      Parsetree.class_field_kind -> 'res=
      (function
       | Parsetree.Cfk_virtual x0 ->
           this#constr "Parsetree.class_field_kind"
             ("Cfk_virtual", [this#lift_Parsetree_core_type x0])
       | Parsetree.Cfk_concrete (x0,x1) ->
           this#constr "Parsetree.class_field_kind"
             ("Cfk_concrete",
               [this#lift_Asttypes_override_flag x0;
               this#lift_Parsetree_expression x1]) : Parsetree.class_field_kind
                                                       -> 'res)
    method lift_Parsetree_module_binding : Parsetree.module_binding -> 'res=
      (fun
         { Parsetree.pmb_name = pmb_name; Parsetree.pmb_expr = pmb_expr;
           Parsetree.pmb_attributes = pmb_attributes;
           Parsetree.pmb_loc = pmb_loc }
          ->
         this#record "Parsetree.module_binding"
           [("pmb_name", (this#lift_Asttypes_loc this#string pmb_name));
           ("pmb_expr", (this#lift_Parsetree_module_expr pmb_expr));
           ("pmb_attributes",
             (this#lift_Parsetree_attributes pmb_attributes));
           ("pmb_loc", (this#lift_Location_t pmb_loc))] : Parsetree.module_binding
                                                            -> 'res)
    method lift_Parsetree_module_expr : Parsetree.module_expr -> 'res=
      (fun
         { Parsetree.pmod_desc = pmod_desc; Parsetree.pmod_loc = pmod_loc;
           Parsetree.pmod_attributes = pmod_attributes }
          ->
         this#record "Parsetree.module_expr"
           [("pmod_desc", (this#lift_Parsetree_module_expr_desc pmod_desc));
           ("pmod_loc", (this#lift_Location_t pmod_loc));
           ("pmod_attributes",
             (this#lift_Parsetree_attributes pmod_attributes))] : Parsetree.module_expr
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_module_expr_desc :
      Parsetree.module_expr_desc -> 'res=
      (function
       | Parsetree.Pmod_ident x0 ->
           this#constr "Parsetree.module_expr_desc"
             ("Pmod_ident",
               [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Pmod_structure x0 ->
           this#constr "Parsetree.module_expr_desc"
             ("Pmod_structure", [this#lift_Parsetree_structure x0])
       | Parsetree.Pmod_functor (x0,x1,x2) ->
           this#constr "Parsetree.module_expr_desc"
             ("Pmod_functor",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_option this#lift_Parsetree_module_type x1;
               this#lift_Parsetree_module_expr x2])
       | Parsetree.Pmod_apply (x0,x1) ->
           this#constr "Parsetree.module_expr_desc"
             ("Pmod_apply",
               [this#lift_Parsetree_module_expr x0;
               this#lift_Parsetree_module_expr x1])
       | Parsetree.Pmod_constraint (x0,x1) ->
           this#constr "Parsetree.module_expr_desc"
             ("Pmod_constraint",
               [this#lift_Parsetree_module_expr x0;
               this#lift_Parsetree_module_type x1])
       | Parsetree.Pmod_unpack x0 ->
           this#constr "Parsetree.module_expr_desc"
             ("Pmod_unpack", [this#lift_Parsetree_expression x0])
       | Parsetree.Pmod_extension x0 ->
           this#constr "Parsetree.module_expr_desc"
             ("Pmod_extension", [this#lift_Parsetree_extension x0]) : 
      Parsetree.module_expr_desc -> 'res)
    method lift_Parsetree_module_type : Parsetree.module_type -> 'res=
      (fun
         { Parsetree.pmty_desc = pmty_desc; Parsetree.pmty_loc = pmty_loc;
           Parsetree.pmty_attributes = pmty_attributes }
          ->
         this#record "Parsetree.module_type"
           [("pmty_desc", (this#lift_Parsetree_module_type_desc pmty_desc));
           ("pmty_loc", (this#lift_Location_t pmty_loc));
           ("pmty_attributes",
             (this#lift_Parsetree_attributes pmty_attributes))] : Parsetree.module_type
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_module_type_desc :
      Parsetree.module_type_desc -> 'res=
      (function
       | Parsetree.Pmty_ident x0 ->
           this#constr "Parsetree.module_type_desc"
             ("Pmty_ident",
               [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Pmty_signature x0 ->
           this#constr "Parsetree.module_type_desc"
             ("Pmty_signature", [this#lift_Parsetree_signature x0])
       | Parsetree.Pmty_functor (x0,x1,x2) ->
           this#constr "Parsetree.module_type_desc"
             ("Pmty_functor",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_option this#lift_Parsetree_module_type x1;
               this#lift_Parsetree_module_type x2])
       | Parsetree.Pmty_with (x0,x1) ->
           this#constr "Parsetree.module_type_desc"
             ("Pmty_with",
               [this#lift_Parsetree_module_type x0;
               this#list (List.map this#lift_Parsetree_with_constraint x1)])
       | Parsetree.Pmty_typeof x0 ->
           this#constr "Parsetree.module_type_desc"
             ("Pmty_typeof", [this#lift_Parsetree_module_expr x0])
       | Parsetree.Pmty_extension x0 ->
           this#constr "Parsetree.module_type_desc"
             ("Pmty_extension", [this#lift_Parsetree_extension x0])
       | Parsetree.Pmty_alias x0 ->
           this#constr "Parsetree.module_type_desc"
             ("Pmty_alias",
               [this#lift_Asttypes_loc this#lift_Longident_t x0]) : Parsetree.module_type_desc
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_with_constraint :
      Parsetree.with_constraint -> 'res=
      (function
       | Parsetree.Pwith_type (x0,x1) ->
           this#constr "Parsetree.with_constraint"
             ("Pwith_type",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_Parsetree_type_declaration x1])
       | Parsetree.Pwith_module (x0,x1) ->
           this#constr "Parsetree.with_constraint"
             ("Pwith_module",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1])
       | Parsetree.Pwith_typesubst x0 ->
           this#constr "Parsetree.with_constraint"
             ("Pwith_typesubst", [this#lift_Parsetree_type_declaration x0])
       | Parsetree.Pwith_modsubst (x0,x1) ->
           this#constr "Parsetree.with_constraint"
             ("Pwith_modsubst",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1]) : Parsetree.with_constraint
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_signature : Parsetree.signature -> 'res=
      (fun x  -> this#list (List.map this#lift_Parsetree_signature_item x) : 
      Parsetree.signature -> 'res)
    method lift_Parsetree_signature_item : Parsetree.signature_item -> 'res=
      (fun { Parsetree.psig_desc = psig_desc; Parsetree.psig_loc = psig_loc }
          ->
         this#record "Parsetree.signature_item"
           [("psig_desc",
              (this#lift_Parsetree_signature_item_desc psig_desc));
           ("psig_loc", (this#lift_Location_t psig_loc))] : Parsetree.signature_item
                                                              -> 'res)
    method lift_Parsetree_signature_item_desc :
      Parsetree.signature_item_desc -> 'res=
      (function
       | Parsetree.Psig_value x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_value", [this#lift_Parsetree_value_description x0])
       | Parsetree.Psig_type x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_type",
               [this#list (List.map this#lift_Parsetree_type_declaration x0)])
       | Parsetree.Psig_typext x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_typext", [this#lift_Parsetree_type_extension x0])
       | Parsetree.Psig_exception x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_exception",
               [this#lift_Parsetree_extension_constructor x0])
       | Parsetree.Psig_module x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_module", [this#lift_Parsetree_module_declaration x0])
       | Parsetree.Psig_recmodule x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_recmodule",
               [this#list
                  (List.map this#lift_Parsetree_module_declaration x0)])
       | Parsetree.Psig_modtype x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_modtype",
               [this#lift_Parsetree_module_type_declaration x0])
       | Parsetree.Psig_open x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_open", [this#lift_Parsetree_open_description x0])
       | Parsetree.Psig_include x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_include", [this#lift_Parsetree_include_description x0])
       | Parsetree.Psig_class x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_class",
               [this#list (List.map this#lift_Parsetree_class_description x0)])
       | Parsetree.Psig_class_type x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_class_type",
               [this#list
                  (List.map this#lift_Parsetree_class_type_declaration x0)])
       | Parsetree.Psig_attribute x0 ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_attribute", [this#lift_Parsetree_attribute x0])
       | Parsetree.Psig_extension (x0,x1) ->
           this#constr "Parsetree.signature_item_desc"
             ("Psig_extension",
               [this#lift_Parsetree_extension x0;
               this#lift_Parsetree_attributes x1]) : Parsetree.signature_item_desc
                                                       -> 'res)
    method lift_Parsetree_class_type_declaration :
      Parsetree.class_type_declaration -> 'res=
      (fun x  ->
         this#lift_Parsetree_class_infos this#lift_Parsetree_class_type x : 
      Parsetree.class_type_declaration -> 'res)
    method lift_Parsetree_class_description :
      Parsetree.class_description -> 'res=
      (fun x  ->
         this#lift_Parsetree_class_infos this#lift_Parsetree_class_type x : 
      Parsetree.class_description -> 'res)
    method lift_Parsetree_class_type : Parsetree.class_type -> 'res=
      (fun
         { Parsetree.pcty_desc = pcty_desc; Parsetree.pcty_loc = pcty_loc;
           Parsetree.pcty_attributes = pcty_attributes }
          ->
         this#record "Parsetree.class_type"
           [("pcty_desc", (this#lift_Parsetree_class_type_desc pcty_desc));
           ("pcty_loc", (this#lift_Location_t pcty_loc));
           ("pcty_attributes",
             (this#lift_Parsetree_attributes pcty_attributes))] : Parsetree.class_type
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_class_type_desc :
      Parsetree.class_type_desc -> 'res=
      (function
       | Parsetree.Pcty_constr (x0,x1) ->
           this#constr "Parsetree.class_type_desc"
             ("Pcty_constr",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#list (List.map this#lift_Parsetree_core_type x1)])
       | Parsetree.Pcty_signature x0 ->
           this#constr "Parsetree.class_type_desc"
             ("Pcty_signature", [this#lift_Parsetree_class_signature x0])
       | Parsetree.Pcty_arrow (x0,x1,x2) ->
           this#constr "Parsetree.class_type_desc"
             ("Pcty_arrow",
               [this#lift_Asttypes_label x0;
               this#lift_Parsetree_core_type x1;
               this#lift_Parsetree_class_type x2])
       | Parsetree.Pcty_extension x0 ->
           this#constr "Parsetree.class_type_desc"
             ("Pcty_extension", [this#lift_Parsetree_extension x0]) : 
      Parsetree.class_type_desc -> 'res)
    method lift_Parsetree_class_signature :
      Parsetree.class_signature -> 'res=
      (fun
         { Parsetree.pcsig_self = pcsig_self;
           Parsetree.pcsig_fields = pcsig_fields }
          ->
         this#record "Parsetree.class_signature"
           [("pcsig_self", (this#lift_Parsetree_core_type pcsig_self));
           ("pcsig_fields",
             (this#list
                (List.map this#lift_Parsetree_class_type_field pcsig_fields)))] : 
      Parsetree.class_signature -> 'res)
    method lift_Parsetree_class_type_field :
      Parsetree.class_type_field -> 'res=
      (fun
         { Parsetree.pctf_desc = pctf_desc; Parsetree.pctf_loc = pctf_loc;
           Parsetree.pctf_attributes = pctf_attributes }
          ->
         this#record "Parsetree.class_type_field"
           [("pctf_desc",
              (this#lift_Parsetree_class_type_field_desc pctf_desc));
           ("pctf_loc", (this#lift_Location_t pctf_loc));
           ("pctf_attributes",
             (this#lift_Parsetree_attributes pctf_attributes))] : Parsetree.class_type_field
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_class_type_field_desc :
      Parsetree.class_type_field_desc -> 'res=
      (function
       | Parsetree.Pctf_inherit x0 ->
           this#constr "Parsetree.class_type_field_desc"
             ("Pctf_inherit", [this#lift_Parsetree_class_type x0])
       | Parsetree.Pctf_val x0 ->
           this#constr "Parsetree.class_type_field_desc"
             ("Pctf_val",
               [(let (x0,x1,x2,x3) = x0 in
                 this#tuple
                   [this#string x0;
                   this#lift_Asttypes_mutable_flag x1;
                   this#lift_Asttypes_virtual_flag x2;
                   this#lift_Parsetree_core_type x3])])
       | Parsetree.Pctf_method x0 ->
           this#constr "Parsetree.class_type_field_desc"
             ("Pctf_method",
               [(let (x0,x1,x2,x3) = x0 in
                 this#tuple
                   [this#string x0;
                   this#lift_Asttypes_private_flag x1;
                   this#lift_Asttypes_virtual_flag x2;
                   this#lift_Parsetree_core_type x3])])
       | Parsetree.Pctf_constraint x0 ->
           this#constr "Parsetree.class_type_field_desc"
             ("Pctf_constraint",
               [(let (x0,x1) = x0 in
                 this#tuple
                   [this#lift_Parsetree_core_type x0;
                   this#lift_Parsetree_core_type x1])])
       | Parsetree.Pctf_attribute x0 ->
           this#constr "Parsetree.class_type_field_desc"
             ("Pctf_attribute", [this#lift_Parsetree_attribute x0])
       | Parsetree.Pctf_extension x0 ->
           this#constr "Parsetree.class_type_field_desc"
             ("Pctf_extension", [this#lift_Parsetree_extension x0]) : 
      Parsetree.class_type_field_desc -> 'res)
    method lift_Parsetree_extension : Parsetree.extension -> 'res=
      (fun x  ->
         let (x0,x1) = x in
         this#tuple
           [this#lift_Asttypes_loc this#string x0;
           this#lift_Parsetree_payload x1] : Parsetree.extension -> 'res)
    method lift_Parsetree_class_infos :
      'f0 . ('f0 -> 'res) -> 'f0 Parsetree.class_infos -> 'res= fun (type f0)
      ->
      (fun f0  ->
         fun
           { Parsetree.pci_virt = pci_virt;
             Parsetree.pci_params = pci_params;
             Parsetree.pci_name = pci_name; Parsetree.pci_expr = pci_expr;
             Parsetree.pci_loc = pci_loc;
             Parsetree.pci_attributes = pci_attributes }
            ->
           this#record "Parsetree.class_infos"
             [("pci_virt", (this#lift_Asttypes_virtual_flag pci_virt));
             ("pci_params",
               (this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1) = x in
                        this#tuple
                          [this#lift_Parsetree_core_type x0;
                          this#lift_Asttypes_variance x1]) pci_params)));
             ("pci_name", (this#lift_Asttypes_loc this#string pci_name));
             ("pci_expr", (f0 pci_expr));
             ("pci_loc", (this#lift_Location_t pci_loc));
             ("pci_attributes",
               (this#lift_Parsetree_attributes pci_attributes))] : (f0 ->
                                                                    'res) ->
                                                                    f0
                                                                    Parsetree.class_infos
                                                                    -> 
                                                                    'res)
    method lift_Asttypes_virtual_flag : Asttypes.virtual_flag -> 'res=
      (function
       | Asttypes.Virtual  ->
           this#constr "Asttypes.virtual_flag" ("Virtual", [])
       | Asttypes.Concrete  ->
           this#constr "Asttypes.virtual_flag" ("Concrete", []) : Asttypes.virtual_flag
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_include_description :
      Parsetree.include_description -> 'res=
      (fun x  ->
         this#lift_Parsetree_include_infos this#lift_Parsetree_module_type x : 
      Parsetree.include_description -> 'res)
    method lift_Parsetree_include_infos :
      'f0 . ('f0 -> 'res) -> 'f0 Parsetree.include_infos -> 'res= fun (type
      f0) ->
      (fun f0  ->
         fun
           { Parsetree.pincl_mod = pincl_mod;
             Parsetree.pincl_loc = pincl_loc;
             Parsetree.pincl_attributes = pincl_attributes }
            ->
           this#record "Parsetree.include_infos"
             [("pincl_mod", (f0 pincl_mod));
             ("pincl_loc", (this#lift_Location_t pincl_loc));
             ("pincl_attributes",
               (this#lift_Parsetree_attributes pincl_attributes))] : 
      (f0 -> 'res) -> f0 Parsetree.include_infos -> 'res)
    method lift_Parsetree_open_description :
      Parsetree.open_description -> 'res=
      (fun
         { Parsetree.popen_lid = popen_lid;
           Parsetree.popen_override = popen_override;
           Parsetree.popen_loc = popen_loc;
           Parsetree.popen_attributes = popen_attributes }
          ->
         this#record "Parsetree.open_description"
           [("popen_lid",
              (this#lift_Asttypes_loc this#lift_Longident_t popen_lid));
           ("popen_override",
             (this#lift_Asttypes_override_flag popen_override));
           ("popen_loc", (this#lift_Location_t popen_loc));
           ("popen_attributes",
             (this#lift_Parsetree_attributes popen_attributes))] : Parsetree.open_description
                                                                    -> 
                                                                    'res)
    method lift_Asttypes_override_flag : Asttypes.override_flag -> 'res=
      (function
       | Asttypes.Override  ->
           this#constr "Asttypes.override_flag" ("Override", [])
       | Asttypes.Fresh  ->
           this#constr "Asttypes.override_flag" ("Fresh", []) : Asttypes.override_flag
                                                                  -> 
                                                                  'res)
    method lift_Parsetree_module_type_declaration :
      Parsetree.module_type_declaration -> 'res=
      (fun
         { Parsetree.pmtd_name = pmtd_name; Parsetree.pmtd_type = pmtd_type;
           Parsetree.pmtd_attributes = pmtd_attributes;
           Parsetree.pmtd_loc = pmtd_loc }
          ->
         this#record "Parsetree.module_type_declaration"
           [("pmtd_name", (this#lift_Asttypes_loc this#string pmtd_name));
           ("pmtd_type",
             (this#lift_option this#lift_Parsetree_module_type pmtd_type));
           ("pmtd_attributes",
             (this#lift_Parsetree_attributes pmtd_attributes));
           ("pmtd_loc", (this#lift_Location_t pmtd_loc))] : Parsetree.module_type_declaration
                                                              -> 'res)
    method lift_Parsetree_module_declaration :
      Parsetree.module_declaration -> 'res=
      (fun
         { Parsetree.pmd_name = pmd_name; Parsetree.pmd_type = pmd_type;
           Parsetree.pmd_attributes = pmd_attributes;
           Parsetree.pmd_loc = pmd_loc }
          ->
         this#record "Parsetree.module_declaration"
           [("pmd_name", (this#lift_Asttypes_loc this#string pmd_name));
           ("pmd_type", (this#lift_Parsetree_module_type pmd_type));
           ("pmd_attributes",
             (this#lift_Parsetree_attributes pmd_attributes));
           ("pmd_loc", (this#lift_Location_t pmd_loc))] : Parsetree.module_declaration
                                                            -> 'res)
    method lift_Parsetree_type_extension : Parsetree.type_extension -> 'res=
      (fun
         { Parsetree.ptyext_path = ptyext_path;
           Parsetree.ptyext_params = ptyext_params;
           Parsetree.ptyext_constructors = ptyext_constructors;
           Parsetree.ptyext_private = ptyext_private;
           Parsetree.ptyext_attributes = ptyext_attributes }
          ->
         this#record "Parsetree.type_extension"
           [("ptyext_path",
              (this#lift_Asttypes_loc this#lift_Longident_t ptyext_path));
           ("ptyext_params",
             (this#list
                (List.map
                   (fun x  ->
                      let (x0,x1) = x in
                      this#tuple
                        [this#lift_Parsetree_core_type x0;
                        this#lift_Asttypes_variance x1]) ptyext_params)));
           ("ptyext_constructors",
             (this#list
                (List.map this#lift_Parsetree_extension_constructor
                   ptyext_constructors)));
           ("ptyext_private",
             (this#lift_Asttypes_private_flag ptyext_private));
           ("ptyext_attributes",
             (this#lift_Parsetree_attributes ptyext_attributes))] : Parsetree.type_extension
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_extension_constructor :
      Parsetree.extension_constructor -> 'res=
      (fun
         { Parsetree.pext_name = pext_name; Parsetree.pext_kind = pext_kind;
           Parsetree.pext_loc = pext_loc;
           Parsetree.pext_attributes = pext_attributes }
          ->
         this#record "Parsetree.extension_constructor"
           [("pext_name", (this#lift_Asttypes_loc this#string pext_name));
           ("pext_kind",
             (this#lift_Parsetree_extension_constructor_kind pext_kind));
           ("pext_loc", (this#lift_Location_t pext_loc));
           ("pext_attributes",
             (this#lift_Parsetree_attributes pext_attributes))] : Parsetree.extension_constructor
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_extension_constructor_kind :
      Parsetree.extension_constructor_kind -> 'res=
      (function
       | Parsetree.Pext_decl (x0,x1) ->
           this#constr "Parsetree.extension_constructor_kind"
             ("Pext_decl",
               [this#list (List.map this#lift_Parsetree_core_type x0);
               this#lift_option this#lift_Parsetree_core_type x1])
       | Parsetree.Pext_rebind x0 ->
           this#constr "Parsetree.extension_constructor_kind"
             ("Pext_rebind",
               [this#lift_Asttypes_loc this#lift_Longident_t x0]) : Parsetree.extension_constructor_kind
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_type_declaration :
      Parsetree.type_declaration -> 'res=
      (fun
         { Parsetree.ptype_name = ptype_name;
           Parsetree.ptype_params = ptype_params;
           Parsetree.ptype_cstrs = ptype_cstrs;
           Parsetree.ptype_kind = ptype_kind;
           Parsetree.ptype_private = ptype_private;
           Parsetree.ptype_manifest = ptype_manifest;
           Parsetree.ptype_attributes = ptype_attributes;
           Parsetree.ptype_loc = ptype_loc }
          ->
         this#record "Parsetree.type_declaration"
           [("ptype_name", (this#lift_Asttypes_loc this#string ptype_name));
           ("ptype_params",
             (this#list
                (List.map
                   (fun x  ->
                      let (x0,x1) = x in
                      this#tuple
                        [this#lift_Parsetree_core_type x0;
                        this#lift_Asttypes_variance x1]) ptype_params)));
           ("ptype_cstrs",
             (this#list
                (List.map
                   (fun x  ->
                      let (x0,x1,x2) = x in
                      this#tuple
                        [this#lift_Parsetree_core_type x0;
                        this#lift_Parsetree_core_type x1;
                        this#lift_Location_t x2]) ptype_cstrs)));
           ("ptype_kind", (this#lift_Parsetree_type_kind ptype_kind));
           ("ptype_private", (this#lift_Asttypes_private_flag ptype_private));
           ("ptype_manifest",
             (this#lift_option this#lift_Parsetree_core_type ptype_manifest));
           ("ptype_attributes",
             (this#lift_Parsetree_attributes ptype_attributes));
           ("ptype_loc", (this#lift_Location_t ptype_loc))] : Parsetree.type_declaration
                                                                -> 'res)
    method lift_Asttypes_private_flag : Asttypes.private_flag -> 'res=
      (function
       | Asttypes.Private  ->
           this#constr "Asttypes.private_flag" ("Private", [])
       | Asttypes.Public  ->
           this#constr "Asttypes.private_flag" ("Public", []) : Asttypes.private_flag
                                                                  -> 
                                                                  'res)
    method lift_Parsetree_type_kind : Parsetree.type_kind -> 'res=
      (function
       | Parsetree.Ptype_abstract  ->
           this#constr "Parsetree.type_kind" ("Ptype_abstract", [])
       | Parsetree.Ptype_variant x0 ->
           this#constr "Parsetree.type_kind"
             ("Ptype_variant",
               [this#list
                  (List.map this#lift_Parsetree_constructor_declaration x0)])
       | Parsetree.Ptype_record x0 ->
           this#constr "Parsetree.type_kind"
             ("Ptype_record",
               [this#list (List.map this#lift_Parsetree_label_declaration x0)])
       | Parsetree.Ptype_open  ->
           this#constr "Parsetree.type_kind" ("Ptype_open", []) : Parsetree.type_kind
                                                                    -> 
                                                                    'res)
    method lift_Parsetree_label_declaration :
      Parsetree.label_declaration -> 'res=
      (fun
         { Parsetree.pld_name = pld_name;
           Parsetree.pld_mutable = pld_mutable;
           Parsetree.pld_type = pld_type; Parsetree.pld_loc = pld_loc;
           Parsetree.pld_attributes = pld_attributes }
          ->
         this#record "Parsetree.label_declaration"
           [("pld_name", (this#lift_Asttypes_loc this#string pld_name));
           ("pld_mutable", (this#lift_Asttypes_mutable_flag pld_mutable));
           ("pld_type", (this#lift_Parsetree_core_type pld_type));
           ("pld_loc", (this#lift_Location_t pld_loc));
           ("pld_attributes",
             (this#lift_Parsetree_attributes pld_attributes))] : Parsetree.label_declaration
                                                                   -> 
                                                                   'res)
    method lift_Asttypes_mutable_flag : Asttypes.mutable_flag -> 'res=
      (function
       | Asttypes.Immutable  ->
           this#constr "Asttypes.mutable_flag" ("Immutable", [])
       | Asttypes.Mutable  ->
           this#constr "Asttypes.mutable_flag" ("Mutable", []) : Asttypes.mutable_flag
                                                                   -> 
                                                                   'res)
    method lift_Parsetree_constructor_declaration :
      Parsetree.constructor_declaration -> 'res=
      (fun
         { Parsetree.pcd_name = pcd_name; Parsetree.pcd_args = pcd_args;
           Parsetree.pcd_res = pcd_res; Parsetree.pcd_loc = pcd_loc;
           Parsetree.pcd_attributes = pcd_attributes }
          ->
         this#record "Parsetree.constructor_declaration"
           [("pcd_name", (this#lift_Asttypes_loc this#string pcd_name));
           ("pcd_args",
             (this#list (List.map this#lift_Parsetree_core_type pcd_args)));
           ("pcd_res",
             (this#lift_option this#lift_Parsetree_core_type pcd_res));
           ("pcd_loc", (this#lift_Location_t pcd_loc));
           ("pcd_attributes",
             (this#lift_Parsetree_attributes pcd_attributes))] : Parsetree.constructor_declaration
                                                                   -> 
                                                                   'res)
    method lift_Asttypes_variance : Asttypes.variance -> 'res=
      (function
       | Asttypes.Covariant  ->
           this#constr "Asttypes.variance" ("Covariant", [])
       | Asttypes.Contravariant  ->
           this#constr "Asttypes.variance" ("Contravariant", [])
       | Asttypes.Invariant  ->
           this#constr "Asttypes.variance" ("Invariant", []) : Asttypes.variance
                                                                 -> 'res)
    method lift_Parsetree_value_description :
      Parsetree.value_description -> 'res=
      (fun
         { Parsetree.pval_name = pval_name; Parsetree.pval_type = pval_type;
           Parsetree.pval_prim = pval_prim;
           Parsetree.pval_attributes = pval_attributes;
           Parsetree.pval_loc = pval_loc }
          ->
         this#record "Parsetree.value_description"
           [("pval_name", (this#lift_Asttypes_loc this#string pval_name));
           ("pval_type", (this#lift_Parsetree_core_type pval_type));
           ("pval_prim", (this#list (List.map this#string pval_prim)));
           ("pval_attributes",
             (this#lift_Parsetree_attributes pval_attributes));
           ("pval_loc", (this#lift_Location_t pval_loc))] : Parsetree.value_description
                                                              -> 'res)
    method lift_Asttypes_closed_flag : Asttypes.closed_flag -> 'res=
      (function
       | Asttypes.Closed  ->
           this#constr "Asttypes.closed_flag" ("Closed", [])
       | Asttypes.Open  -> this#constr "Asttypes.closed_flag" ("Open", []) : 
      Asttypes.closed_flag -> 'res)
    method lift_Asttypes_label : Asttypes.label -> 'res=
      (this#string : Asttypes.label -> 'res)
    method lift_Asttypes_rec_flag : Asttypes.rec_flag -> 'res=
      (function
       | Asttypes.Nonrecursive  ->
           this#constr "Asttypes.rec_flag" ("Nonrecursive", [])
       | Asttypes.Recursive  ->
           this#constr "Asttypes.rec_flag" ("Recursive", []) : Asttypes.rec_flag
                                                                 -> 'res)
    method lift_Asttypes_constant : Asttypes.constant -> 'res=
      (function
       | Asttypes.Const_int x0 ->
           this#constr "Asttypes.constant" ("Const_int", [this#int x0])
       | Asttypes.Const_char x0 ->
           this#constr "Asttypes.constant" ("Const_char", [this#char x0])
       | Asttypes.Const_string (x0,x1) ->
           this#constr "Asttypes.constant"
             ("Const_string",
               [this#string x0; this#lift_option this#string x1])
       | Asttypes.Const_float x0 ->
           this#constr "Asttypes.constant" ("Const_float", [this#string x0])
       | Asttypes.Const_int32 x0 ->
           this#constr "Asttypes.constant" ("Const_int32", [this#int32 x0])
       | Asttypes.Const_int64 x0 ->
           this#constr "Asttypes.constant" ("Const_int64", [this#int64 x0])
       | Asttypes.Const_nativeint x0 ->
           this#constr "Asttypes.constant"
             ("Const_nativeint", [this#nativeint x0]) : Asttypes.constant ->
                                                          'res)
    method lift_option : 'f0 . ('f0 -> 'res) -> 'f0 option -> 'res= fun (type
      f0) ->
      (fun f0  ->
         function
         | None  -> this#constr "option" ("None", [])
         | Some x0 -> this#constr "option" ("Some", [f0 x0]) : (f0 -> 'res)
                                                                 ->
                                                                 f0 option ->
                                                                   'res)
    method lift_Longident_t : Longident.t -> 'res=
      (function
       | Longident.Lident x0 ->
           this#constr "Longident.t" ("Lident", [this#string x0])
       | Longident.Ldot (x0,x1) ->
           this#constr "Longident.t"
             ("Ldot", [this#lift_Longident_t x0; this#string x1])
       | Longident.Lapply (x0,x1) ->
           this#constr "Longident.t"
             ("Lapply", [this#lift_Longident_t x0; this#lift_Longident_t x1]) : 
      Longident.t -> 'res)
    method lift_Asttypes_loc :
      'f0 . ('f0 -> 'res) -> 'f0 Asttypes.loc -> 'res= fun (type f0) ->
      (fun f0  ->
         fun { Asttypes.txt = txt; Asttypes.loc = loc }  ->
           this#record "Asttypes.loc"
             [("txt", (f0 txt)); ("loc", (this#lift_Location_t loc))] : 
      (f0 -> 'res) -> f0 Asttypes.loc -> 'res)
    method lift_Location_t : Location.t -> 'res=
      (fun
         { Location.loc_start = loc_start; Location.loc_end = loc_end;
           Location.loc_ghost = loc_ghost }
          ->
         this#record "Location.t"
           [("loc_start", (this#lift_Lexing_position loc_start));
           ("loc_end", (this#lift_Lexing_position loc_end));
           ("loc_ghost", (this#lift_bool loc_ghost))] : Location.t -> 'res)
    method lift_bool : bool -> 'res=
      (function
       | false  -> this#constr "bool" ("false", [])
       | true  -> this#constr "bool" ("true", []) : bool -> 'res)
    method lift_Lexing_position : Lexing.position -> 'res=
      (fun
         { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
           Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
          ->
         this#record "Lexing.position"
           [("pos_fname", (this#string pos_fname));
           ("pos_lnum", (this#int pos_lnum));
           ("pos_bol", (this#int pos_bol));
           ("pos_cnum", (this#int pos_cnum))] : Lexing.position -> 'res)
  end

end
module Ast_mapper : sig 
#1 "ast_mapper.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The interface of a -ppx rewriter

  A -ppx rewriter is a program that accepts a serialized abstract syntax
  tree and outputs another, possibly modified, abstract syntax tree.
  This module encapsulates the interface between the compiler and
  the -ppx rewriters, handling such details as the serialization format,
  forwarding of command-line flags, and storing state.

  {!mapper} allows to implement AST rewriting using open recursion.
  A typical mapper would be based on {!default_mapper}, a deep
  identity mapper, and will fall back on it for handling the syntax it
  does not modify. For example:

  {[
open Asttypes
open Parsetree
open Ast_mapper

let test_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
        Ast_helper.Exp.constant (Const_int 42)
      | other -> default_mapper.expr mapper other; }

let () =
  register "ppx_test" test_mapper]}

  This -ppx rewriter, which replaces [[%test]] in expressions with
  the constant [42], can be compiled using
  [ocamlc -o ppx_test -I +compiler-libs ocamlcommon.cma ppx_test.ml].

  *)

open Parsetree

(** {2 A generic Parsetree mapper} *)

type mapper = {
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  class_declaration: mapper -> class_declaration -> class_declaration;
  class_description: mapper -> class_description -> class_description;
  class_expr: mapper -> class_expr -> class_expr;
  class_field: mapper -> class_field -> class_field;
  class_signature: mapper -> class_signature -> class_signature;
  class_structure: mapper -> class_structure -> class_structure;
  class_type: mapper -> class_type -> class_type;
  class_type_declaration: mapper -> class_type_declaration
                          -> class_type_declaration;
  class_type_field: mapper -> class_type_field -> class_type_field;
  constructor_declaration: mapper -> constructor_declaration
                           -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  extension_constructor: mapper -> extension_constructor
                         -> extension_constructor;
  include_declaration: mapper -> include_declaration -> include_declaration;
  include_description: mapper -> include_description -> include_description;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration: mapper -> module_type_declaration
                           -> module_type_declaration;
  open_description: mapper -> open_description -> open_description;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  type_extension: mapper -> type_extension -> type_extension;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}
(** A mapper record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the mapper to be applied to children in the syntax
    tree. *)

val default_mapper: mapper
(** A default mapper, which implements a "deep identity" mapping. *)

(** {2 Apply mappers to compilation units} *)

val tool_name: unit -> string
(** Can be used within a ppx preprocessor to know which tool is
    calling it ["ocamlc"], ["ocamlopt"], ["ocamldoc"], ["ocamldep"],
    ["ocaml"], ...  Some global variables that reflect command-line
    options are automatically synchronized between the calling tool
    and the ppx preprocessor: [Clflags.include_dirs],
    [Config.load_path], [Clflags.open_modules], [Clflags.for_package],
    [Clflags.debug]. *)


val apply: source:string -> target:string -> mapper -> unit
(** Apply a mapper (parametrized by the unit name) to a dumped
    parsetree found in the [source] file and put the result in the
    [target] file. The [structure] or [signature] field of the mapper
    is applied to the implementation or interface.  *)

val run_main: (string list -> mapper) -> unit
(** Entry point to call to implement a standalone -ppx rewriter from a
    mapper, parametrized by the command line arguments.  The current
    unit name can be obtained from [Location.input_name].  This
    function implements proper error reporting for uncaught
    exceptions. *)

(** {2 Registration API} *)

val register_function: (string -> (string list -> mapper) -> unit) ref

val register: string -> (string list -> mapper) -> unit
(** Apply the [register_function].  The default behavior is to run the
    mapper immediately, taking arguments from the process command
    line.  This is to support a scenario where a mapper is linked as a
    stand-alone executable.

    It is possible to overwrite the [register_function] to define
    "-ppx drivers", which combine several mappers in a single process.
    Typically, a driver starts by defining [register_function] to a
    custom implementation, then lets ppx rewriters (linked statically
    or dynamically) register themselves, and then run all or some of
    them.  It is also possible to have -ppx drivers apply rewriters to
    only specific parts of an AST.

    The first argument to [register] is a symbolic name to be used by
    the ppx driver.  *)


(** {2 Convenience functions to write mappers} *)

val map_opt: ('a -> 'b) -> 'a option -> 'b option

val extension_of_error: Location.error -> extension
(** Encode an error into an 'ocaml.error' extension node which can be
    inserted in a generated Parsetree.  The compiler will be
    responsible for reporting the error. *)

val attribute_of_warning: Location.t -> string -> attribute
(** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
    inserted in a generated Parsetree.  The compiler will be
    responsible for reporting the warning. *)

(** {2 Helper functions to call external mappers} *)

val add_ppx_context_str: tool_name:string -> Parsetree.structure -> Parsetree.structure
(** Extract information from the current environment and encode it
    into an attribute which is prepended to the list of structure
    items in order to pass the information to an external
    processor. *)

val add_ppx_context_sig: tool_name:string -> Parsetree.signature -> Parsetree.signature
(** Same as [add_ppx_context_str], but for signatures. *)

val drop_ppx_context_str: restore:bool -> Parsetree.structure -> Parsetree.structure
(** Drop the ocaml.ppx.context attribute from a structure.  If
    [restore] is true, also restore the associated data in the current
    process. *)

val drop_ppx_context_sig: restore:bool -> Parsetree.signature -> Parsetree.signature
(** Same as [drop_ppx_context_str], but for signatures. *)

(** {2 Cookies} *)

(** Cookies are used to pass information from a ppx processor to
    a further invocation of itself, when called from the OCaml
    toplevel (or other tools that support cookies). *)

val set_cookie: string -> Parsetree.expression -> unit
val get_cookie: string -> Parsetree.expression option

end = struct
#1 "ast_mapper.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* A generic Parsetree mapping class *)

(*
[@@@ocaml.warning "+9"]
  (* Ensure that record patterns don't miss any field. *)
*)


open Asttypes
open Parsetree
open Ast_helper
open Location

type mapper = {
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  class_declaration: mapper -> class_declaration -> class_declaration;
  class_description: mapper -> class_description -> class_description;
  class_expr: mapper -> class_expr -> class_expr;
  class_field: mapper -> class_field -> class_field;
  class_signature: mapper -> class_signature -> class_signature;
  class_structure: mapper -> class_structure -> class_structure;
  class_type: mapper -> class_type -> class_type;
  class_type_declaration: mapper -> class_type_declaration
                          -> class_type_declaration;
  class_type_field: mapper -> class_type_field -> class_type_field;
  constructor_declaration: mapper -> constructor_declaration
                           -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  extension_constructor: mapper -> extension_constructor
                         -> extension_constructor;
  include_declaration: mapper -> include_declaration -> include_declaration;
  include_description: mapper -> include_description -> include_description;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration: mapper -> module_type_declaration
                           -> module_type_declaration;
  open_description: mapper -> open_description -> open_description;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  type_extension: mapper -> type_extension -> type_extension;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}

let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function None -> None | Some x -> Some (f x)

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (l, attrs, b, tl) ->
        Rtag (l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
    | Rinherit t -> Rinherit (sub.typ sub t)

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    let open Typ in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ptyp_any -> any ~loc ~attrs ()
    | Ptyp_var s -> var ~loc ~attrs s
    | Ptyp_arrow (lab, t1, t2) ->
        arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2)
    | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
    | Ptyp_constr (lid, tl) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_object (l, o) ->
        let f (s, a, t) = (s, sub.attributes sub a, sub.typ sub t) in
        object_ ~loc ~attrs (List.map f l) o
    | Ptyp_class (lid, tl) ->
        class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
    | Ptyp_variant (rl, b, ll) ->
        variant ~loc ~attrs (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) -> poly ~loc ~attrs sl (sub.typ sub t)
    | Ptyp_package (lid, l) ->
        package ~loc ~attrs (map_loc sub lid)
          (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
    | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    Type.mk (map_loc sub ptype_name)
      ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
      ~priv:ptype_private
      ~cstrs:(List.map
                (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                ptype_cstrs)
      ~kind:(sub.type_kind sub ptype_kind)
      ?manifest:(map_opt (sub.typ sub) ptype_manifest)
      ~loc:(sub.location sub ptype_loc)
      ~attrs:(sub.attributes sub ptype_attributes)

  let map_type_kind sub = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant l ->
        Ptype_variant (List.map (sub.constructor_declaration sub) l)
    | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
    | Ptype_open -> Ptype_open

  let map_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private;
       ptyext_attributes} =
    Te.mk
      (map_loc sub ptyext_path)
      (List.map (sub.extension_constructor sub) ptyext_constructors)
      ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
      ~priv:ptyext_private
      ~attrs:(sub.attributes sub ptyext_attributes)

  let map_extension_constructor_kind sub = function
      Pext_decl(ctl, cto) ->
        Pext_decl(List.map (sub.typ sub) ctl, map_opt (sub.typ sub) cto)
    | Pext_rebind li ->
        Pext_rebind (map_loc sub li)

  let map_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    Te.constructor
      (map_loc sub pext_name)
      (map_extension_constructor_kind sub pext_kind)
      ~loc:(sub.location sub pext_loc)
      ~attrs:(sub.attributes sub pext_attributes)

end

module CT = struct
  (* Type expressions for the class language *)

  let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    let open Cty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcty_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
    | Pcty_arrow (lab, t, ct) ->
        arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
    | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    let open Ctf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
    | Pctf_val (s, m, v, t) -> val_ ~loc ~attrs s m v (sub.typ sub t)
    | Pctf_method (s, p, v, t) -> method_ ~loc ~attrs s p v (sub.typ sub t)
    | Pctf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_signature sub {pcsig_self; pcsig_fields} =
    Csig.mk
      (sub.typ sub pcsig_self)
      (List.map (sub.class_type_field sub) pcsig_fields)
end

module MT = struct
  (* Type expressions for the module language *)

  let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    let open Mty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
    | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
    | Pmty_functor (s, mt1, mt2) ->
        functor_ ~loc ~attrs (map_loc sub s)
          (Misc.may_map (sub.module_type sub) mt1)
          (sub.module_type sub mt2)
    | Pmty_with (mt, l) ->
        with_ ~loc ~attrs (sub.module_type sub mt)
          (List.map (sub.with_constraint sub) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
    | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_with_constraint sub = function
    | Pwith_type (lid, d) ->
        Pwith_type (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_module (lid, lid2) ->
        Pwith_module (map_loc sub lid, map_loc sub lid2)
    | Pwith_typesubst d -> Pwith_typesubst (sub.type_declaration sub d)
    | Pwith_modsubst (s, lid) ->
        Pwith_modsubst (map_loc sub s, map_loc sub lid)

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let loc = sub.location sub loc in
    match desc with
    | Psig_value vd -> value ~loc (sub.value_description sub vd)
    | Psig_type l -> type_ ~loc (List.map (sub.type_declaration sub) l)
    | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
    | Psig_recmodule l ->
        rec_module ~loc (List.map (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc (sub.open_description sub x)
    | Psig_include x -> include_ ~loc (sub.include_description sub x)
    | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
    | Psig_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Psig_extension (x, attrs) ->
        extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
    | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
end


module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
    | Pmod_functor (arg, arg_ty, body) ->
        functor_ ~loc ~attrs (map_loc sub arg)
          (Misc.may_map (sub.module_type sub) arg_ty)
          (sub.module_expr sub body)
    | Pmod_apply (m1, m2) ->
        apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
    | Pmod_constraint (m, mty) ->
        constraint_ ~loc ~attrs (sub.module_expr sub m)
                    (sub.module_type sub mty)
    | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
    | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    let open Str in
    let loc = sub.location sub loc in
    match desc with
    | Pstr_eval (x, attrs) ->
        eval ~loc ~attrs:(sub.attributes sub attrs) (sub.expr sub x)
    | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type l -> type_ ~loc (List.map (sub.type_declaration sub) l)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_description sub x)
    | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
        extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
    | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
end

module E = struct
  (* Value expressions for the core language *)

  let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    let open Exp in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs x
    | Pexp_let (r, vbs, e) ->
        let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
          (sub.expr sub e)
    | Pexp_fun (lab, def, p, e) ->
        fun_ ~loc ~attrs lab (map_opt (sub.expr sub) def) (sub.pat sub p)
          (sub.expr sub e)
    | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
    | Pexp_apply (e, l) ->
        apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
    | Pexp_match (e, pel) ->
        match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_construct (lid, arg) ->
        construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
    | Pexp_variant (lab, eo) ->
        variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
    | Pexp_record (l, eo) ->
        record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
          (map_opt (sub.expr sub) eo)
    | Pexp_field (e, lid) ->
        field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
        setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
          (sub.expr sub e2)
    | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_ifthenelse (e1, e2, e3) ->
        ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
          (map_opt (sub.expr sub) e3)
    | Pexp_sequence (e1, e2) ->
        sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_while (e1, e2) ->
        while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_for (p, e1, e2, d, e3) ->
        for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
          (sub.expr sub e3)
    | Pexp_coerce (e, t1, t2) ->
        coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
          (sub.typ sub t2)
    | Pexp_constraint (e, t) ->
        constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
    | Pexp_send (e, s) -> send ~loc ~attrs (sub.expr sub e) s
    | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
    | Pexp_setinstvar (s, e) ->
        setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_override sel ->
        override ~loc ~attrs
          (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
    | Pexp_letmodule (s, me, e) ->
        letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
          (sub.expr sub e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
    | Pexp_poly (e, t) ->
        poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
    | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
    | Pexp_newtype (s, e) -> newtype ~loc ~attrs s (sub.expr sub e)
    | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
    | Pexp_open (ovf, lid, e) ->
        open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
    | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
end

module P = struct
  (* Patterns *)

  let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    let open Pat in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ppat_any -> any ~loc ~attrs ()
    | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs c
    | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_construct (l, p) ->
        construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
        record ~loc ~attrs
               (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
    | Ppat_constraint (p, t) ->
        constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
    | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
    | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
    | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
end

module CE = struct
  (* Value expressions for the class language *)

  let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    let open Cl in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcl_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcl_structure s ->
        structure ~loc ~attrs (sub.class_structure sub s)
    | Pcl_fun (lab, e, p, ce) ->
        fun_ ~loc ~attrs lab
          (map_opt (sub.expr sub) e)
          (sub.pat sub p)
          (sub.class_expr sub ce)
    | Pcl_apply (ce, l) ->
        apply ~loc ~attrs (sub.class_expr sub ce)
          (List.map (map_snd (sub.expr sub)) l)
    | Pcl_let (r, vbs, ce) ->
        let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
          (sub.class_expr sub ce)
    | Pcl_constraint (ce, ct) ->
        constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
    | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_kind sub = function
    | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
    | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

  let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    let open Cf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcf_inherit (o, ce, s) -> inherit_ ~loc ~attrs o (sub.class_expr sub ce) s
    | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
    | Pcf_method (s, p, k) ->
        method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
    | Pcf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
    | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure sub {pcstr_self; pcstr_fields} =
    {
      pcstr_self = sub.pat sub pcstr_self;
      pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
    }

  let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    Ci.mk
     ~virt:pci_virt
     ~params:(List.map (map_fst (sub.typ sub)) pl)
      (map_loc sub pci_name)
      (f pci_expr)
      ~loc:(sub.location sub pci_loc)
      ~attrs:(sub.attributes sub pci_attributes)
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  {
    structure = (fun this l -> List.map (this.structure_item this) l);
    structure_item = M.map_structure_item;
    module_expr = M.map;
    signature = (fun this l -> List.map (this.signature_item this) l);
    signature_item = MT.map_signature_item;
    module_type = MT.map;
    with_constraint = MT.map_with_constraint;
    class_declaration =
      (fun this -> CE.class_infos this (this.class_expr this));
    class_expr = CE.map;
    class_field = CE.map_field;
    class_structure = CE.map_structure;
    class_type = CT.map;
    class_type_field = CT.map_field;
    class_signature = CT.map_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.map_type_declaration;
    type_kind = T.map_type_kind;
    typ = T.map;
    type_extension = T.map_type_extension;
    extension_constructor = T.map_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim; pval_loc;
                 pval_attributes} ->
        Val.mk
          (map_loc this pval_name)
          (this.typ this pval_type)
          ~attrs:(this.attributes this pval_attributes)
          ~loc:(this.location this pval_loc)
          ~prim:pval_prim
      );

    pat = P.map;
    expr = E.map;

    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
         Md.mk
           (map_loc this pmd_name)
           (this.module_type this pmd_type)
           ~attrs:(this.attributes this pmd_attributes)
           ~loc:(this.location this pmd_loc)
      );

    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
         Mtd.mk
           (map_loc this pmtd_name)
           ?typ:(map_opt (this.module_type this) pmtd_type)
           ~attrs:(this.attributes this pmtd_attributes)
           ~loc:(this.location this pmtd_loc)
      );

    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
         Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
           ~attrs:(this.attributes this pmb_attributes)
           ~loc:(this.location this pmb_loc)
      );


    open_description =
      (fun this {popen_lid; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (map_loc this popen_lid)
           ~override:popen_override
           ~loc:(this.location this popen_loc)
           ~attrs:(this.attributes this popen_attributes)
      );


    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         Incl.mk (this.module_type this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );

    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         Incl.mk (this.module_expr this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );


    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
         Vb.mk
           (this.pat this pvb_pat)
           (this.expr this pvb_expr)
           ~loc:(this.location this pvb_loc)
           ~attrs:(this.attributes this pvb_attributes)
      );


    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
        Type.constructor
          (map_loc this pcd_name)
          ~args:(List.map (this.typ this) pcd_args)
          ?res:(map_opt (this.typ this) pcd_res)
          ~loc:(this.location this pcd_loc)
          ~attrs:(this.attributes this pcd_attributes)
      );

    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
         Type.field
           (map_loc this pld_name)
           (this.typ this pld_type)
           ~mut:pld_mutable
           ~loc:(this.location this pld_loc)
           ~attrs:(this.attributes this pld_attributes)
      );

    cases = (fun this l -> List.map (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
         {
           pc_lhs = this.pat this pc_lhs;
           pc_guard = map_opt (this.expr this) pc_guard;
           pc_rhs = this.expr this pc_rhs;
         }
      );



    location = (fun this l -> l);

    extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attributes = (fun this l -> List.map (this.attribute this) l);
    payload =
      (fun this -> function
         | PStr x -> PStr (this.structure this x)
         | PTyp x -> PTyp (this.typ this x)
         | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
      );
  }

let rec extension_of_error {loc; msg; if_highlight; sub} =
  { loc; txt = "ocaml.error" },
  PStr ([Str.eval (Exp.constant (Const_string (msg, None)));
         Str.eval (Exp.constant (Const_string (if_highlight, None)))] @
        (List.map (fun ext -> Str.extension (extension_of_error ext)) sub))

let attribute_of_warning loc s =
  { loc; txt = "ocaml.ppwarning" },
  PStr ([Str.eval ~loc (Exp.constant (Const_string (s, None)))])

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
end)

let cookies = ref StringMap.empty

let get_cookie k =
  try Some (StringMap.find k !cookies)
  with Not_found -> None

let set_cookie k v =
  cookies := StringMap.add k v !cookies

let tool_name_ref = ref "_none_"

let tool_name () = !tool_name_ref


module PpxContext = struct
  open Longident
  open Asttypes
  open Ast_helper

  let lid name = { txt = Lident name; loc = Location.none }

  let make_string x = Exp.constant (Const_string (x, None))

  let make_bool x =
    if x
    then Exp.construct (lid "true") None
    else Exp.construct (lid "false") None

  let rec make_list f lst =
    match lst with
    | x :: rest ->
      Exp.construct (lid "::") (Some (Exp.tuple [f x; make_list f rest]))
    | [] ->
      Exp.construct (lid "[]") None

  let make_pair f1 f2 (x1, x2) =
    Exp.tuple [f1 x1; f2 x2]

  let make_option f opt =
    match opt with
    | Some x -> Exp.construct (lid "Some") (Some (f x))
    | None   -> Exp.construct (lid "None") None

  let get_cookies () =
    lid "cookies",
    make_list (make_pair make_string (fun x -> x))
      (StringMap.bindings !cookies)

  let mk fields =
    { txt = "ocaml.ppx.context"; loc = Location.none },
    Parsetree.PStr [Str.eval (Exp.record fields None)]

  let make ~tool_name () =
    let fields =
      [
        lid "tool_name",    make_string tool_name;
        lid "include_dirs", make_list make_string !Clflags.include_dirs;
        lid "load_path",    make_list make_string !Config.load_path;
        lid "open_modules", make_list make_string !Clflags.open_modules;
        lid "for_package",  make_option make_string !Clflags.for_package;
        lid "debug",        make_bool !Clflags.debug;
        get_cookies ()
      ]
    in
    mk fields

  let get_fields = function
    | PStr [{pstr_desc = Pstr_eval
                 ({ pexp_desc = Pexp_record (fields, None) }, [])}] ->
        fields
    | _ ->
        raise_errorf "Internal error: invalid [@@@ocaml.ppx.context] syntax"

  let restore fields =
    let field name payload =
      let rec get_string = function
        | { pexp_desc = Pexp_constant (Const_string (str, None)) } -> str
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] string syntax"
              name
      and get_bool pexp =
        match pexp with
        | {pexp_desc = Pexp_construct ({txt = Longident.Lident "true"}, None)} ->
            true
        | {pexp_desc = Pexp_construct ({txt = Longident.Lident "false"}, None)} ->
            false
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] bool syntax"
              name
      and get_list elem = function
        | {pexp_desc =
             Pexp_construct ({txt = Longident.Lident "::"},
                             Some {pexp_desc = Pexp_tuple [exp; rest]}) } ->
            elem exp :: get_list elem rest
        | {pexp_desc =
             Pexp_construct ({txt = Longident.Lident "[]"}, None)} ->
            []
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] list syntax"
              name
      and get_pair f1 f2 = function
        | {pexp_desc = Pexp_tuple [e1; e2]} ->
            (f1 e1, f2 e2)
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] pair syntax"
              name
      and get_option elem = function
        | { pexp_desc =
              Pexp_construct ({ txt = Longident.Lident "Some" }, Some exp) } ->
            Some (elem exp)
        | { pexp_desc =
              Pexp_construct ({ txt = Longident.Lident "None" }, None) } ->
            None
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] option syntax"
              name
      in
      match name with
      | "tool_name" ->
          tool_name_ref := get_string payload
      | "include_dirs" ->
          Clflags.include_dirs := get_list get_string payload
      | "load_path" ->
          Config.load_path := get_list get_string payload
      | "open_modules" ->
          Clflags.open_modules := get_list get_string payload
      | "for_package" ->
          Clflags.for_package := get_option get_string payload
      | "debug" ->
          Clflags.debug := get_bool payload
      | "cookies" ->
          let l = get_list (get_pair get_string (fun x -> x)) payload in
          cookies :=
            List.fold_left
              (fun s (k, v) -> StringMap.add k v s) StringMap.empty
              l
      | _ ->
          ()
    in
    List.iter (function ({txt=Lident name}, x) -> field name x | _ -> ()) fields

  let update_cookies fields =
    let fields =
      List.filter
        (function ({txt=Lident "cookies"}, _) -> false | _ -> true)
        fields
    in
    fields @ [get_cookies ()]
end

let ppx_context = PpxContext.make


let apply_lazy ~source ~target mapper =
  let ic = open_in_bin source in
  let magic =
    really_input_string ic (String.length Config.ast_impl_magic_number)
  in
  if magic <> Config.ast_impl_magic_number
  && magic <> Config.ast_intf_magic_number then
    failwith "Ast_mapper: OCaml version mismatch or malformed input";
  Location.input_name := input_value ic;
  let ast = input_value ic in
  close_in ic;

  let implem ast =
    try
      let fields, ast =
        match ast with
        | {pstr_desc = Pstr_attribute ({txt = "ocaml.ppx.context"}, x)} :: l ->
            PpxContext.get_fields x, l
        | _ -> [], ast
      in
      PpxContext.restore fields;
      let mapper = mapper () in
      let ast = mapper.structure mapper ast in
      let fields = PpxContext.update_cookies fields in
      Str.attribute (PpxContext.mk fields) :: ast
    with exn ->
      match error_of_exn exn with
      | Some error ->
          [{pstr_desc = Pstr_extension (extension_of_error error, []);
            pstr_loc  = Location.none}]
      | None -> raise exn
  in
  let iface ast =
    try
      let fields, ast =
        match ast with
        | {psig_desc = Psig_attribute ({txt = "ocaml.ppx.context"}, x)} :: l ->
            PpxContext.get_fields x, l
        | _ -> [], ast
      in
      PpxContext.restore fields;
      let mapper = mapper () in
      let ast = mapper.signature mapper ast in
      let fields = PpxContext.update_cookies fields in
      Sig.attribute (PpxContext.mk fields) :: ast
    with exn ->
      match error_of_exn exn with
      | Some error ->
          [{psig_desc = Psig_extension (extension_of_error error, []);
            psig_loc  = Location.none}]
      | None -> raise exn
  in
  let ast =
    if magic = Config.ast_impl_magic_number
    then Obj.magic (implem (Obj.magic ast))
    else Obj.magic (iface (Obj.magic ast))
  in
  let oc = open_out_bin target in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc

let drop_ppx_context_str ~restore = function
  | {pstr_desc = Pstr_attribute({Location.txt = "ocaml.ppx.context"}, a)}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let drop_ppx_context_sig ~restore = function
  | {psig_desc = Psig_attribute({Location.txt = "ocaml.ppx.context"}, a)}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let add_ppx_context_str ~tool_name ast =
  Ast_helper.Str.attribute (ppx_context ~tool_name ()) :: ast

let add_ppx_context_sig ~tool_name ast =
  Ast_helper.Sig.attribute (ppx_context ~tool_name ()) :: ast


let apply ~source ~target mapper =
  apply_lazy ~source ~target (fun () -> mapper)

let run_main mapper =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      let mapper () =
        try mapper (Array.to_list (Array.sub a 1 (n - 3)))
        with exn ->
          (* PR #6463 *)
          let f _ _ = raise exn in
          {default_mapper with structure = f; signature = f}
      in
      apply_lazy ~source:a.(n - 2) ~target:a.(n - 1) mapper
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

let register_function = ref (fun _name f -> run_main f)
let register name f = !register_function name f

end
module Ppx_metaquot
= struct
#1 "Ppx_metaquot.ml"
(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(* A -ppx rewriter to be used to write Parsetree-generating code
   (including other -ppx rewriters) using concrete syntax.

   We support the following extensions in expression position:

   [%expr ...]  maps to code which creates the expression represented by ...
   [%pat? ...] maps to code which creates the pattern represented by ...
   [%str ...] maps to code which creates the structure represented by ...
   [%stri ...] maps to code which creates the structure item represented by ...
   [%type: ...] maps to code which creates the core type represented by ...

   Quoted code can refer to expressions representing AST fragments,
   using the following extensions:

     [%e ...] where ... is an expression of type Parsetree.expression
     [%t ...] where ... is an expression of type Parsetree.core_type
     [%p ...] where ... is an expression of type Parsetree.pattern


   All locations generated by the meta quotation are by default set
   to [Ast_helper.default_loc].  This can be overriden by providing a custom
   expression which will be inserted whereever a location is required
   in the generated AST.  This expression can be specified globally
   (for the current structure) as a structure item attribute:

     ;;[@@metaloc ...]

   or locally for the scope of an expression:

     e [@metaloc ...]



   Support is also provided to use concrete syntax in pattern
   position.  The location and attribute fields are currently ignored
   by patterns generated from meta quotations.

   We support the following extensions in pattern position:

   [%expr ...]  maps to code which creates the expression represented by ...
   [%pat? ...] maps to code which creates the pattern represented by ...
   [%str ...] maps to code which creates the structure represented by ...
   [%type: ...] maps to code which creates the core type represented by ...

   Quoted code can refer to expressions representing AST fragments,
   using the following extensions:

     [%e? ...] where ... is a pattern of type Parsetree.expression
     [%t? ...] where ... is a pattern of type Parsetree.core_type
     [%p? ...] where ... is a pattern of type Parsetree.pattern

*)

module Main : sig end = struct
  open Asttypes
  open Parsetree
  open Ast_helper
  open Ast_convenience

  let prefix ty s =
    let open Longident in
    match parse ty with
    | Ldot(m, _) -> String.concat "." (Longident.flatten m) ^ "." ^ s
    | _ -> s

  class exp_builder =
    object
      method record ty x = record (List.map (fun (l, e) -> prefix ty l, e) x)
      method constr ty (c, args) = constr (prefix ty c) args
      method list l = list l
      method tuple l = tuple l
      method int i = int i
      method string s = str s
      method char c = char c
      method int32 x = Exp.constant (Const_int32 x)
      method int64 x = Exp.constant (Const_int64 x)
      method nativeint x = Exp.constant (Const_nativeint x)
    end

  class pat_builder =
    object
      method record ty x = precord ~closed:Closed (List.map (fun (l, e) -> prefix ty l, e) x)
      method constr ty (c, args) = pconstr (prefix ty c) args
      method list l = plist l
      method tuple l = ptuple l
      method int i = pint i
      method string s = pstr s
      method char c = pchar c
      method int32 x = Pat.constant (Const_int32 x)
      method int64 x = Pat.constant (Const_int64 x)
      method nativeint x = Pat.constant (Const_nativeint x)
    end


  let get_exp loc = function
    | PStr [ {pstr_desc=Pstr_eval (e, _); _} ] -> e
    | _ ->
        Format.eprintf "%aExpression expected@."
          Location.print_error loc;
        exit 2

  let get_typ loc = function
    | PTyp t -> t
    | _ ->
        Format.eprintf "%aType expected@."
          Location.print_error loc;
        exit 2

  let get_pat loc = function
    | PPat (t, None) -> t
    | _ ->
        Format.eprintf "%aPattern expected@."
          Location.print_error loc;
        exit 2

  let exp_lifter loc map =
    let map = map.Ast_mapper.expr map in
    object
      inherit [_] Ast_lifter.lifter as super
      inherit exp_builder

      (* Special support for location in the generated AST *)
      method! lift_Location_t _ = loc

      (* Support for antiquotations *)
      method! lift_Parsetree_expression = function
        | {pexp_desc=Pexp_extension({txt="e";loc}, e); _} -> map (get_exp loc e)
        | x -> super # lift_Parsetree_expression x

      method! lift_Parsetree_pattern = function
        | {ppat_desc=Ppat_extension({txt="p";loc}, e); _} -> map (get_exp loc e)
        | x -> super # lift_Parsetree_pattern x

      method! lift_Parsetree_core_type = function
        | {ptyp_desc=Ptyp_extension({txt="t";loc}, e); _} -> map (get_exp loc e)
        | x -> super # lift_Parsetree_core_type x
    end

  let pat_lifter map =
    let map = map.Ast_mapper.pat map in
    object
      inherit [_] Ast_lifter.lifter as super
      inherit pat_builder

      (* Special support for location and attributes in the generated AST *)
      method! lift_Location_t _ = Pat.any ()
      method! lift_Parsetree_attributes _ = Pat.any ()

      (* Support for antiquotations *)
      method! lift_Parsetree_expression = function
        | {pexp_desc=Pexp_extension({txt="e";loc}, e); _} -> map (get_pat loc e)
        | x -> super # lift_Parsetree_expression x

      method! lift_Parsetree_pattern = function
        | {ppat_desc=Ppat_extension({txt="p";loc}, e); _} -> map (get_pat loc e)
        | x -> super # lift_Parsetree_pattern x

      method! lift_Parsetree_core_type = function
        | {ptyp_desc=Ptyp_extension({txt="t";loc}, e); _} -> map (get_pat loc e)
        | x -> super # lift_Parsetree_core_type x
    end

  let loc = ref (app (evar "Pervasives.!") [evar "Ast_helper.default_loc"])

  let handle_attr = function
    | {txt="metaloc";loc=l}, e -> loc := get_exp l e
    | _ -> ()

  let with_loc ?(attrs = []) f =
    let old_loc = !loc in
    List.iter handle_attr attrs;
    let r = f () in
    loc := old_loc;
    r

  let expander _args =
    let open Ast_mapper in
    let super = default_mapper in
    let expr this e =
      with_loc ~attrs:e.pexp_attributes
        (fun () ->
           match e.pexp_desc with
           | Pexp_extension({txt="expr";loc=l}, e) ->
               (exp_lifter !loc this) # lift_Parsetree_expression (get_exp l e)
           | Pexp_extension({txt="pat";loc=l}, e) ->
               (exp_lifter !loc this) # lift_Parsetree_pattern (get_pat l e)
           | Pexp_extension({txt="str";_}, PStr e) ->
               (exp_lifter !loc this) # lift_Parsetree_structure e
           | Pexp_extension({txt="stri";_}, PStr [e]) ->
               (exp_lifter !loc this) # lift_Parsetree_structure_item e
           | Pexp_extension({txt="type";loc=l}, e) ->
               (exp_lifter !loc this) # lift_Parsetree_core_type (get_typ l e)
           | _ ->
               super.expr this e
        )
    and pat this p =
      with_loc ~attrs:p.ppat_attributes
        (fun () ->
           match p.ppat_desc with
           | Ppat_extension({txt="expr";loc=l}, e) ->
               (pat_lifter this) # lift_Parsetree_expression (get_exp l e)
           | Ppat_extension({txt="pat";loc=l}, e) ->
               (pat_lifter this) # lift_Parsetree_pattern (get_pat l e)
           | Ppat_extension({txt="str";_}, PStr e) ->
               (pat_lifter this) # lift_Parsetree_structure e
           | Ppat_extension({txt="stri";_}, PStr [e]) ->
               (pat_lifter this) # lift_Parsetree_structure_item e
           | Ppat_extension({txt="type";loc=l}, e) ->
               (pat_lifter this) # lift_Parsetree_core_type (get_typ l e)
           | _ ->
               super.pat this p
        )
    and structure this l =
      with_loc
        (fun () -> super.structure this l)

    and structure_item this x =
      begin match x.pstr_desc with
      | Pstr_attribute x -> handle_attr x
      | _ -> ()
      end;
      super.structure_item this x

    in
    {super with expr; pat; structure; structure_item}

  let () = Ast_mapper.run_main expander
end

end
