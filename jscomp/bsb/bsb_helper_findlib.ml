let (//) = Ext_path.combine

let gen_findlib_conf output_dir cwd =
  let destdir = Bsb_unix.run_command_capture_stdout "ocamlfind printconf destdir" in
  (* TODO(sansouci): Probably pretty brittle. If there is no output to stdout
     it's likely there was an error on stderr of the kind "ocamlfind not found".
     We just assume that it's bad either way.*)
  if destdir = "" then
    Bsb_log.error "Error running `ocamlfind printconf destdir` to generate the findlib.conf. Hint: do you have `ocamlfind` installed?"
  else begin 
    let path = Bsb_unix.run_command_capture_stdout "ocamlfind printconf path" in
    if path = "" then
      Bsb_log.error "Error running `ocamlfind printconf destdir` to generate the findlib.conf. Hint: do you have `ocamlfind` installed?"
    else begin 
      let destdirTrimmed = (String.sub destdir 0 (String.length destdir - 1)) in
      let pathTrimmed = (String.sub path 0 (String.length path - 1)) in
      let ocaml_dir = Bsb_build_util.get_ocaml_dir cwd in
      (* Sigh multi-line string adds all sorts of unwanted indentation and doesn't look that much better :(  *)
      let findlibconf = Printf.sprintf "\
          destdir=\"%s\"\n\
          path=\"%s\"\n\
          ocamlc=\"%s\"\n\
          ocamlopt=\"%s\"\
        "
        destdirTrimmed
        pathTrimmed
        (ocaml_dir // "ocamlc.opt")
        (ocaml_dir // "ocamlopt.opt") in
      
      let oc = open_out_bin output_dir in 
      output_string oc findlibconf; 
      close_out oc
    end
  end
