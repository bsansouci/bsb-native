open Bsb_internals

let (//) = Filename.concat

let _ = 
  let oc = open_out (Helpers.cwd // "build.ninja") in
  Bsb_ninja_util.output_build oc
    ~input:"src/native/Fun.c"
    ~output:"lib/bs/Fun.o"
    ~rule:(Bsb_rule.define ~command:("ocamlc -ccopt -o -ccopt ${out} ${in}") "func");
  let _ = 
    let includePath = Helpers.cwd //"vendor"//"freetype-2.8"//"release"//"include"//"freetype2" in
    Bsb_ninja_util.output_build oc
      ~input:"vendor/camlimages/src/ftintf.c"
      ~output:"lib/bs/libTruetypebindings.o"
      ~rule:(Bsb_rule.define ~command:("ocamlc -ccopt -I"^ includePath ^ " -ccopt -o -ccopt ${out} ${in}") "func")
    in
  close_out oc;
  Helpers.ninja Helpers.cwd;
  

