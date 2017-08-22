#!/bin/sh
set -e


# export OCAMLPARAM='_,bin-annot=1'
# export OCAMLRUNPARAM=b
# -no-shared-libs
cd vendor/ocaml
./configure -prefix `pwd`  -no-ocamldoc -no-ocamlbuild -no-curses -no-graph -no-pthread -no-debugger  && make -j9 world.opt && make install  && make -C stdlib install && mkdir -p "$(dirname `pwd`)"/lib/ocaml/caml && cp otherlibs/systhreads/threads.h "$(dirname `pwd`)"/lib/ocaml/caml/threads.h && make -C otherlibs/systhreads && cd ..
