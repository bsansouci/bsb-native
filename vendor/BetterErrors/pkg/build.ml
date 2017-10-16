#!/usr/bin/env ocaml

#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "BetterErrors" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.bin ~auto:true "src/betterErrorsShell" ~dst:"huh";
    Pkg.lib ~exts:Exts.library "src/BetterErrors";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/betterErrorsTypes";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/betterErrorsParseError";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/betterErrorsMain";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/helpers";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/terminalReporter";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/reportWarning";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/reportError";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/parseWarning";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/Atom";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/NuclideReporter";
    Pkg.doc "README.md";
  ]
