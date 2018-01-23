module Install =
  struct
    type field =
      [ `Bin  | `Doc  | `Etc  | `Lib  | `Lib_root  | `Libexec
      | `Libexec_root  | `Man  | `Misc  | `Sbin  | `Share  | `Share_root
      | `Stublibs  | `Toplevel  | `Unknown of string ]
    let field_to_string =
      function
      | `Bin -> "bin"
      | `Doc -> "doc"
      | `Etc -> "etc"
      | `Lib -> "lib"
      | `Lib_root -> "lib_root"
      | `Libexec -> "libexec"
      | `Libexec_root -> "libexec_root"
      | `Man -> "man"
      | `Misc -> "misc"
      | `Sbin -> "sbin"
      | `Share -> "share"
      | `Share_root -> "share_root"
      | `Stublibs -> "stublibs"
      | `Toplevel -> "toplevel"
      | `Unknown name -> name
    type move = {
      src: string;
      dst: string option;
      maybe: bool;}
    let move ?(maybe= false)  ?dst  src = { src; dst; maybe }
    type t = ([ `Header of string option ]* (field* move) list)
    let to_buffer (`Header h,mvs) =
      let b = Buffer.create 1024 in
      let pr b fmt = Printf.bprintf b fmt in
      let pr_header b =
        function
        | None  -> ()
        | ((Some (h))[@explicit_arity ]) -> pr b "# %s\n\n" h in
      let pr_src b src maybe =
        pr b "  \"%s%s\"" (if maybe then "?" else "") src in
      let pr_dst b dst =
        match dst with
        | None  -> ()
        | ((Some (dst))[@explicit_arity ]) -> pr b " {\"%s\"}" dst in
      let pr_field_end b last = if last <> "" then pr b " ]\n" in
      let pr_field b last field =
        if last = field
        then pr b "\n"
        else (pr_field_end b last; pr b "%s: [\n" field) in
      let pr_move b last (field,{ src; dst; maybe }) =
        pr_field b last field; pr_src b src maybe; pr_dst b dst; field in
      let sortable (field,mv) = ((field_to_string field), mv) in
      let moves = let open List in sort compare (rev_map sortable mvs) in
      pr_header b h;
      (let last = List.fold_left (pr_move b) "" moves in
       pr_field_end b last; b)
  end
module Io =
  struct
    type err
    let readJSONFile f cb =
      let ic = open_in f in
      cb (Ext_json_parse.parse_json_from_chan f ic); close_in ic
    let writeFile f contents =
      let oc = open_out f in output_string oc contents; close_out oc
    type processStatus =
      | WEXITED of int
      | WSIGNALED of int
      | WSTOPPED of int
    let exec cmd =
      let (ic,oc) = Unix.open_process cmd in
      let buf = Buffer.create 64 in
      (try while true do Buffer.add_channel buf ic 1 done
       with | End_of_file  -> ());
      (let err =
         match Unix.close_process (ic, oc) with
         | ((Unix.WEXITED (c))[@explicit_arity ]) ->
             ((WEXITED (c))[@explicit_arity ])
         | ((Unix.WSIGNALED (c))[@explicit_arity ]) ->
             ((WSIGNALED (c))[@explicit_arity ])
         | ((Unix.WSTOPPED (c))[@explicit_arity ]) ->
             ((WSTOPPED (c))[@explicit_arity ]) in
       (err, (Buffer.contents buf)))
  end
let generate cwd =
  let (+|+) = Filename.concat in
  let packagejson = "package.json" in
  Io.readJSONFile packagejson
    (fun json  ->
       let open Ext_json_types in
         (let b = Buffer.create 1024 in
          let pr b fmt = Printf.bprintf b fmt in
          let pr_field b key value = pr b "%s: \"%s\"\n" key value in
          let (||>>) m field =
            match String_map.find_exn field m with
            | exception Not_found  -> m
            | ((Str ({ str }))[@explicit_arity ]) ->
                (pr_field b field str; m)
            | _ -> assert false in
          (match json with
           | ((Obj ({ map }))[@explicit_arity ]) ->
               (pr b "opam-version: \"1.2\"\n";
                (let opamMap =
                   match String_map.find_exn "opam" map with
                   | exception Not_found  -> None
                   | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                       Some innerMap
                   | _ -> assert false in
                 (match opamMap with
                  | ((Some (opamMap))[@explicit_arity ]) ->
                      let name =
                        match String_map.find_exn "libraryName" opamMap with
                        | exception Not_found  ->
                            (match String_map.find_exn "name" map with
                             | exception Not_found  ->
                                 failwith "Field `name` doesn't exist."
                             | ((Str ({ str }))[@explicit_arity ]) -> str
                             | _ -> failwith "Field `name` doesn't exist.")
                        | ((Str ({ str }))[@explicit_arity ]) -> str
                        | _ -> failwith "Field `libraryName` isn't a string." in
                      pr b "name: \"%s\"\n" name
                  | _ -> ignore (map ||>> "name"));
                 ignore @@
                   (((map ||>> "version") ||>> "license") ||>> "tags");
                 (let homepage_found =
                    match String_map.find_exn "homepage" map with
                    | exception Not_found  -> false
                    | ((Str ({ str }))[@explicit_arity ]) ->
                        (pr b "homepage: \"%s\"\n" str; true)
                    | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                        let url =
                          match String_map.find_exn "url" innerMap with
                          | ((Str ({ str }))[@explicit_arity ]) -> str
                          | exception Not_found  ->
                              failwith
                                "Field `url` not found inside `homepage`"
                          | _ -> failwith "Field `url` should be a string." in
                        (pr b "homepage: \"%s\"\n" url; true)
                    | _ -> failwith "Field `homepage` wasn't a string." in
                  let bugs_found =
                    match String_map.find_exn "bugs" map with
                    | exception Not_found  -> false
                    | ((Str ({ str }))[@explicit_arity ]) ->
                        (pr b "bug-reports: \"%s\"\n" str; true)
                    | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                        let url =
                          match String_map.find_exn "url" innerMap with
                          | ((Str ({ str }))[@explicit_arity ]) -> str
                          | exception Not_found  ->
                              failwith "Field `url` not found inside `bugs`"
                          | _ -> failwith "Field `url` should be a string." in
                        (pr b "bug-reports: \"%s\"\n" url; true)
                    | _ -> failwith "Field `bugs` wasn't a string." in
                  (match String_map.find_exn "repository" map with
                   | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                       let url =
                         match String_map.find_exn "url" innerMap with
                         | ((Str ({ str }))[@explicit_arity ]) -> str
                         | exception Not_found  ->
                             failwith
                               "Field `url` not found inside `repository`"
                         | _ -> failwith "Field `url` should be a string." in
                       let t =
                         match String_map.find_exn "type" innerMap with
                         | ((Str ({ str }))[@explicit_arity ]) -> str
                         | exception Not_found  ->
                             failwith
                               "Field `type` not found inside `repository`"
                         | _ -> failwith "Field `type` should be a string." in
                       let ending = ref true in
                       (for i = 0 to (String.length t) - 1 do
                          (let j = ((String.length url) - 1) - i in
                           if (t.[((String.length t) - 1) - i]) <> (url.[j])
                           then ending := false)
                        done;
                        (let homepage =
                           if not (!ending)
                           then (pr b "dev-repo: \"%s.%s\"\n" url t; url)
                           else
                             (pr b "dev-repo: \"%s\"\n" url;
                              String.sub url 0
                                (((String.length url) - (String.length t)) -
                                   1)) in
                         if not homepage_found
                         then pr b "homepage: \"%s\"\n" homepage;
                         if not bugs_found
                         then pr b "bug-reports: \"%s/issues\"\n" homepage))
                   | exception Not_found  ->
                       failwith "Field `repository` not found."
                   | _ ->
                       failwith
                         "Field `repository` should be an object with a `type` and a `url`.");
                  (let printAuthors key =
                     match String_map.find_exn key map with
                     | exception Not_found  -> false
                     | ((Arr ({ content }))[@explicit_arity ]) ->
                         (pr b "authors: [\n";
                          Array.iter
                            (fun x  ->
                               match x with
                               | ((Str ({ str }))[@explicit_arity ]) ->
                                   pr b "  \"%s\"\n" str
                               | ((Obj
                                   ({ map = innerMap }))[@explicit_arity ])
                                   ->
                                   let name =
                                     match String_map.find_exn "name"
                                             innerMap
                                     with
                                     | exception Not_found  ->
                                         failwith @@
                                           (Printf.sprintf
                                              "Field `name` inside `%s` not found."
                                              key)
                                     | ((Str ({ str }))[@explicit_arity ]) ->
                                         str
                                     | _ ->
                                         failwith
                                           "Field `name` should have type string." in
                                   let email =
                                     match String_map.find_exn "email"
                                             innerMap
                                     with
                                     | exception Not_found  -> ""
                                     | ((Str ({ str }))[@explicit_arity ]) ->
                                         str
                                     | _ ->
                                         failwith
                                           "Field `email` should have type string." in
                                   pr b "  \"%s <%s>\"\n" name email
                               | _ ->
                                   failwith @@
                                     (Printf.sprintf
                                        "Field `%s` should be an array of strings or objects with a `name` and `email` fields."
                                        key)) content;
                          pr b "]\n";
                          true)
                     | ((Str ({ str }))[@explicit_arity ]) ->
                         (pr b "authors: [ \"%s\" ]\n" str; true)
                     | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                         let name =
                           match String_map.find_exn "name" innerMap with
                           | exception Not_found  ->
                               failwith @@
                                 (Printf.sprintf
                                    "Field `name` inside `%s` not found." key)
                           | ((Str ({ str }))[@explicit_arity ]) -> str
                           | _ ->
                               failwith
                                 "Field `name` should have type string." in
                         let email =
                           match String_map.find_exn "email" innerMap with
                           | exception Not_found  -> ""
                           | ((Str ({ str }))[@explicit_arity ]) -> str
                           | _ ->
                               failwith
                                 "Field `email` should have type string." in
                         (pr b "authors: [ \"%s <%s>\" ]\n" name email; true)
                     | _ ->
                         failwith @@
                           (Printf.sprintf
                              "Field `%s` should be a string or an object containing the fields `name` and `email`."
                              key) in
                   let hasContributors = printAuthors "contributors" in
                   let hasContributors =
                     (not hasContributors) && (printAuthors "author") in
                   let hasMaintainers =
                     let key = "maintainers" in
                     match String_map.find_exn key map with
                     | exception Not_found  -> false
                     | ((Arr ({ content = [||] }))[@explicit_arity ]) ->
                         failwith
                           "Empty array found for field `maintainers` please add something in there!"
                     | ((Arr ({ content }))[@explicit_arity ]) ->
                         ((match content.(0) with
                           | ((Str ({ str }))[@explicit_arity ]) ->
                               pr b "maintainer: \"%s\"\n" str
                           | ((Obj ({ map = innerMap }))[@explicit_arity ])
                               ->
                               let name =
                                 match String_map.find_exn "name" innerMap
                                 with
                                 | exception Not_found  ->
                                     failwith @@
                                       (Printf.sprintf
                                          "Field `name` inside `%s` not found."
                                          key)
                                 | ((Str ({ str }))[@explicit_arity ]) -> str
                                 | _ ->
                                     failwith
                                       "Field `name` should have type string." in
                               let email =
                                 match String_map.find_exn "email" innerMap
                                 with
                                 | exception Not_found  ->
                                     failwith @@
                                       (Printf.sprintf
                                          "Field `email` inside `%s` not found."
                                          key)
                                 | ((Str ({ str }))[@explicit_arity ]) -> str
                                 | _ ->
                                     failwith
                                       "Field `email` should have type string." in
                               pr b "maintainer: \"%s <%s>\"\n" name email
                           | _ ->
                               failwith @@
                                 (Printf.sprintf
                                    "Field `%s` should be an array of strings or objects with a `name` and `email` fields."
                                    key));
                          true)
                     | _ ->
                         failwith @@
                           (Printf.sprintf
                              "Field `%s` should be a string or an object containing the fields `name` and `email`."
                              key) in
                   if not hasMaintainers
                   then
                     (match String_map.find_exn "author" map with
                      | exception Not_found  ->
                          failwith "Field `author` not found but required."
                      | ((Str ({ str }))[@explicit_arity ]) ->
                          (pr b "maintainer: \"%s\"\n" str;
                           if (not hasContributors) && (not hasMaintainers)
                           then pr b "authors: [ \"%s\" ]\n" str)
                      | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                          let name =
                            match String_map.find_exn "name" innerMap with
                            | exception Not_found  ->
                                failwith
                                  "Field `name` inside `author` not found."
                            | ((Str ({ str }))[@explicit_arity ]) -> str
                            | _ ->
                                failwith
                                  "Field `name` should have type string." in
                          let email =
                            match String_map.find_exn "email" innerMap with
                            | exception Not_found  ->
                                (match hasContributors || hasMaintainers with
                                 | true  -> ""
                                 | false  ->
                                     failwith
                                       "Field `email` inside `author` not found.")
                            | ((Str ({ str }))[@explicit_arity ]) -> str
                            | _ ->
                                failwith
                                  "Field `email` should have type string." in
                          (pr b "maintainer: \"%s <%s>\"\n" name email;
                           if (not hasContributors) && (not hasMaintainers)
                           then pr b "authors: [ \"%s <%s>\" ]\n" name email)
                      | _ ->
                          failwith
                            "Field `author` should be a string or an object containing the fields `name` and `email`.");
                   (match String_map.find_exn "keywords" map with
                    | exception Not_found  -> ()
                    | ((Arr ({ content }))[@explicit_arity ]) ->
                        (pr b "tags: [";
                         Array.iter
                           (fun x  ->
                              match x with
                              | ((Str ({ str }))[@explicit_arity ]) ->
                                  pr b " \"%s\"" str
                              | _ -> assert false) content;
                         pr b " ]\n")
                    | _ -> assert false);
                   (match opamMap with
                    | ((Some (opamMap))[@explicit_arity ]) ->
                        (match String_map.find_exn "dependencies" opamMap
                         with
                         | exception Not_found  -> ()
                         | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                             (pr b "depends: [\n";
                              String_map.iter
                                (fun key  ->
                                   fun v  ->
                                     match v with
                                     | ((Str ({ str }))[@explicit_arity ]) ->
                                         pr b "  \"%s\" { %s }\n" key str
                                     | _ -> assert false) innerMap;
                              pr b "]\n")
                         | _ ->
                             failwith
                               "Field `dependencies` should be an object.")
                    | None  ->
                        (pr b "depends: [\n";
                         pr b "  \"reason\" { build & >=  \"1.13.3\" }\n";
                         pr b "]\n"));
                   (let defaultBuildCommand =
                      "bsb -build-library -make-world -backend bytecode && -build-library -make-world -backend native" in
                    let buildCommand =
                      match opamMap with
                      | ((Some (opamMap))[@explicit_arity ]) ->
                          (match String_map.find_exn "buildCommand" opamMap
                           with
                           | exception Not_found  ->
                               (match String_map.find_exn "scripts" map with
                                | exception Not_found  -> defaultBuildCommand
                                | ((Obj
                                    ({ map = innerMap }))[@explicit_arity ])
                                    ->
                                    (match String_map.find_exn "postinstall"
                                             innerMap
                                     with
                                     | exception Not_found  ->
                                         defaultBuildCommand
                                     | ((Str ({ str }))[@explicit_arity ]) ->
                                         str
                                     | _ ->
                                         failwith
                                           "Field `postinstall` under `scripts` was not a string.")
                                | _ ->
                                    failwith
                                      "Field `scripts` was not an object.")
                           | ((Str ({ str }))[@explicit_arity ]) -> str
                           | _ ->
                               failwith
                                 "Field `buildCommand` only supports a string for now.")
                      | None  -> defaultBuildCommand in
                    let splitChar str ~on  =
                      if str = ""
                      then []
                      else
                        (let rec loop acc offset =
                           try
                             let index = String.rindex_from str offset on in
                             if index = offset
                             then loop ("" :: acc) (index - 1)
                             else
                               (let token =
                                  String.sub str (index + 1) (offset - index) in
                                loop (token :: acc) (index - 1))
                           with
                           | Not_found  -> (String.sub str 0 (offset + 1)) ::
                               acc in
                         loop [] ((String.length str) - 1)) in
                    if (String.length buildCommand) > 0
                    then
                      (let commandParts =
                         List.map (fun l  -> "\"" ^ (l ^ "\""))
                           (splitChar buildCommand ~on:' ') in
                       let str =
                         List.fold_right
                           (fun v  -> fun acc  -> v ^ (" " ^ acc))
                           commandParts "" in
                       pr b "build: [\n"; pr b "  [ %s ]\n" str; pr b "]\n");
                    (let ocamlVersion =
                       match opamMap with
                       | ((Some (opamMap))[@explicit_arity ]) ->
                           (match String_map.find_exn "ocamlVersion" opamMap
                            with
                            | exception Not_found  ->
                                "ocaml-version = \"4.02.3\""
                            | ((Str ({ str }))[@explicit_arity ]) -> str
                            | _ ->
                                failwith
                                  "Field `ocamlVersion` isn't a string.")
                       | _ -> "ocaml-version = \"4.02.3\"" in
                     pr b "available: [ %s ]\n" ocamlVersion))))))
           | _ -> assert false);
          Io.writeFile (cwd +|+ "opam") (Buffer.contents b));

          Io.readJSONFile "bsconfig.json"
         (fun bsjson  -> let bytecode_default_ext = [ ".cma" ] in
          let native_default_ext = [".cmxa"; ".a"] in
          match json with
          | ((Obj ({ map }))[@explicit_arity ]) ->
              let (package_name, res, namespace) = begin match bsjson with
                | ((Obj ({ map = bsjsonmap }))[@explicit_arity ]) ->
                  let package_name = match String_map.find_exn "name" bsjsonmap with
                    | exception Not_found  ->
                        failwith "Field `name` doesn't exist."
                    | ((Str ({ str }))[@explicit_arity ]) -> str
                    | _ -> failwith "Field `name` doesn't exist." in
                  let namespace = begin match String_map.find_exn "namespace" bsjsonmap with
                    | exception Not_found -> None
                    | False _ -> None
                    | True _ -> Some package_name
                  end in
                  let res = begin match String_map.find_exn Bsb_build_schemas.sources bsjsonmap with
                  | exception Not_found -> failwith "Field `sources` doesn't exist."
                  | x ->
                    Bsb_parse_sources.parse_sources
                      package_name
                        {not_dev = true;
                         dir_index =
                           Bsb_dir_index.lib_dir_index;
                         cwd = Filename.current_dir_name;
                         root = cwd;
                         cut_generators = false;
                         traverse = false;
                         backend = [Bsb_parse_sources.Native; Bsb_parse_sources.Bytecode];
                         namespace = namespace; (* @HACK this should probably parse the bsconfig? *)
                        }  x
                      end in
                    (package_name, res, namespace)
                | _ -> assert false
              end in
              let (libraryName,installList) =
                match String_map.find_exn "opam" map with
                | exception Not_found  ->
                    let path = "lib" +|+ "bs" in
                    (package_name,
                      ((List.map
                          (let open Install in
                             fun str  ->
                               (`Lib,
                                 {
                                   src =
                                     ((path +|+ "bytecode") +|+ ("lib" ^ str));
                                   dst = (Some ("lib" ^ str));
                                   maybe = false
                                 })) bytecode_default_ext)
                         @
                         (List.map
                            (let open Install in
                               fun str  ->
                                 (`Lib,
                                   {
                                     src =
                                       ((path +|+ "native") +|+ ("lib" ^ str));
                                     dst = (Some ("lib" ^ str));
                                     maybe = false
                                   })) native_default_ext)))
                | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                    let libraryName =
                      match String_map.find_exn "libraryName" innerMap with
                      | exception Not_found  -> package_name
                      | ((Str ({ str }))[@explicit_arity ]) -> str
                      | _ ->
                          failwith
                            "Field `libraryName` inside field `opam` wasn't a simple string." in
                    let path =
                      match String_map.find_exn "installPath" innerMap with
                      | exception Not_found  -> "_build" +|+ "src"
                      | ((Str ({ str }))[@explicit_arity ]) -> str
                      | _ ->
                          failwith
                            "Couldn't find a `installPath` field or isn't a simple string." in
                    let installType =
                      match String_map.find_exn "type" innerMap with
                      | exception Not_found  -> `Lib
                      | ((Str ({ str }))[@explicit_arity ]) when
                          str = "binary" -> `Bin
                      | ((Str ({ str }))[@explicit_arity ]) when
                          str = "library" -> `Lib
                      | _ ->
                          failwith
                            "Field `type` should be either \"binary\" or \"library\"." in
                    let mainModule =
                      match String_map.find_exn "mainModule" innerMap with
                      | exception Not_found  -> package_name
                      | ((Str ({ str }))[@explicit_arity ]) -> str
                      | _ ->
                          failwith
                            "Field `mainModule` inside field `opam` isn't a simple string." in
                    let installedBinaries =
                      match String_map.find_exn "binaries" innerMap with
                      | exception Not_found  ->
                          [let open Install in
                             (`Bin,
                               {
                                 src = (path +|+ (mainModule ^ ".native"));
                                 dst = (Some (mainModule ^ ".exe"));
                                 maybe = false
                               })]
                      | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                          String_map.fold
                            (fun srcPath  ->
                               fun execName  ->
                                 fun acc  ->
                                   match execName with
                                   | ((Str
                                       ({ str = execName }))[@explicit_arity
                                                              ])
                                       ->
                                       (let open Install in
                                          (`Bin,
                                            {
                                              src = srcPath;
                                              dst = (Some execName);
                                              maybe = false
                                            }))
                                       :: acc
                                   | _ ->
                                       failwith @@
                                         (Printf.sprintf
                                            "The value of the key `%s` inside `binaries` should be a string."
                                            srcPath)) innerMap []
                      | _ ->
                          failwith
                            "Field `binaries` should be an object with {'path/to/binary': 'binaryName' }" in
                    let localBinaries =
                      match String_map.find_exn "localBinaries" innerMap with
                      | exception Not_found  -> []
                      | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                          String_map.fold
                            (fun srcPath  ->
                               fun execName  ->
                                 fun acc  ->
                                   match execName with
                                   | ((Str
                                       ({ str = execName }))[@explicit_arity
                                                              ])
                                       ->
                                       (let open Install in
                                          (`Share,
                                            {
                                              src = srcPath;
                                              dst = (Some execName);
                                              maybe = false
                                            }))
                                       :: acc
                                   | _ ->
                                       failwith @@
                                         (Printf.sprintf
                                            "The value of the key `%s` inside `binaries` should be a string."
                                            srcPath)) innerMap []
                      | _ ->
                          failwith
                            "Field `binaries` should be an object with {'path/to/binary': 'binaryName' }" in
                    let installList =
                      match installType with
                      | `Lib ->
                          (List.map
                             (let open Install in
                                fun str  ->
                                  (`Lib,
                                    {
                                      src =
                                        ((path +|+ "bytecode") +|+
                                           ("lib" ^ str));
                                      dst = (Some ("lib" ^ str));
                                      maybe = false
                                    })) bytecode_default_ext)
                            @
                            (List.map
                               (let open Install in
                                  fun str  ->
                                    (`Lib,
                                      {
                                        src =
                                          ((path +|+ "native") +|+
                                             ("lib" ^ str));
                                        dst = (Some ("lib" ^ str));
                                        maybe = false
                                      })) native_default_ext)
                      | `Bin -> installedBinaries @ localBinaries in
                    (libraryName, installList)
                | _ -> failwith "Field `opam` was not an object." in
                let (namespace_str, installList) = begin match namespace with
                  | None -> ("", installList)
                  | Some name ->
                    ("-" ^ name, (`Lib, {
                      src = "lib" +|+ "bs" +|+ "bytecode" +|+ name ^ ".cmi";
                      dst = (Some (name ^ ".cmi"));
                      maybe = false
                    }) :: installList)
                end in
                let installList = List.fold_left (fun (acc : (Install.field* Install.move) list) (group : Bsb_parse_sources.file_group) ->
                  String_map.fold (fun  module_name (module_info : Bsb_db.module_info)  acc ->
                    let open Install in
                    let path = "lib" +|+ "bs" +|+ "bytecode" in
                    let src = Bsb_db.filename_sans_suffix_of_module_info module_info in
                    (`Lib, {
                      src = path +|+ src ^ namespace_str ^ ".cmi";
                      dst = (Some ((Filename.basename src) ^ namespace_str ^ ".cmi"));
                      maybe = false
                    })
                    (* @Todo Generate the .cmt files for Merlin's sake. *)
                    (* :: (`Lib, {
                      src = path +|+ src ^ ".cmt";
                      dst = (Some ((Filename.basename src) ^ ".cmt"));
                      maybe = false
                    })  *)
                    :: acc
                  ) group.sources acc
                ) installList res.files in
                (* bs_file_groups = res.files;  *)
              let thing =
                let open Install in
                  ((`Header (Some libraryName)),
                    ((`Lib,
                       { src = "./opam"; dst = (Some "opam"); maybe = false })
                    ::
                    (`Lib,
                      { src = "./META"; dst = (Some "META"); maybe = false })
                    :: installList)) in
              Io.writeFile (cwd +|+ (libraryName ^ ".install"))
                (Buffer.contents (Install.to_buffer thing))
          | _ -> assert false);
         (let pr b fmt = Printf.bprintf b fmt in
          match json with
          | ((Obj ({ map }))[@explicit_arity ]) ->
              let (mainModule,isLibrary) =
                match String_map.find_exn "opam" map with
                | exception Not_found  ->
                    (match String_map.find_exn "name" map with
                     | exception Not_found  ->
                         failwith "Field `name` doesn't exist."
                     | ((Str ({ str }))[@explicit_arity ]) -> (str, true)
                     | _ -> failwith "Field `name` isn't a simple string.")
                | ((Obj ({ map = innerMap }))[@explicit_arity ]) ->
                    let mainModule =
                      match String_map.find_exn "mainModule" innerMap with
                      | exception Not_found  ->
                          (match String_map.find_exn "name" map with
                           | exception Not_found  ->
                               failwith "Field `name` doesn't exist."
                           | ((Str ({ str }))[@explicit_arity ]) -> str
                           | _ ->
                               failwith "Field `name` isn't a simple string.")
                      | ((Str ({ str }))[@explicit_arity ]) -> str
                      | _ ->
                          failwith
                            "Couldn't find a `mainModule` field inside `opam` or isn't a simple string" in
                    let isLibrary =
                      match String_map.find_exn "type" innerMap with
                      | exception Not_found  -> true
                      | ((Str ({ str = "library" }))[@explicit_arity ]) ->
                          true
                      | ((Str ({ str = "binary" }))[@explicit_arity ]) ->
                          false
                      | _ ->
                          failwith
                            "Field `type` under `opam` wasn't a string (either 'binary' or 'library')" in
                    (mainModule, isLibrary)
                | _ -> assert false in
              let metab = Buffer.create 1024 in
              let version =
                match String_map.find_exn "version" map with
                | exception Not_found  ->
                    failwith "Field `version` doesn't exist. "
                | ((Str ({ str = version }))[@explicit_arity ]) -> version
                | _ -> failwith "Field `version` was not a simple string." in
              let description =
                match String_map.find_exn "description" map with
                | exception Not_found  ->
                    failwith "Field `description` doesn't exist. "
                | ((Str ({ str = description }))[@explicit_arity ]) ->
                    description
                | _ ->
                    failwith "Field `description` was not a simple string." in
              (pr metab "version = \"%s\"\n" version;
               pr metab "description = \"%s\"\n\n" description;
               if isLibrary
               then
                 (pr metab "archive(byte) = \"lib.cma\"\n";
                  pr metab "archive(native) = \"lib.cmxa\"\n");
               Io.writeFile (cwd +|+ "META") (Buffer.contents metab))
          | _ -> assert false))
