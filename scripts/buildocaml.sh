#!/bin/sh
set -e


# export OCAMLPARAM='_,bin-annot=1'
# export OCAMLRUNPARAM=b
# -no-shared-libs
DIRNAME=`pwd`
cd vendor/ocaml
./configure -prefix `pwd`  -no-ocamldoc -no-ocamlbuild -no-curses -no-graph -no-pthread -no-debugger  && make -j9 world.opt && make install && mkdir -p $DIRNAME/lib/ocaml/caml && make -C otherlibs/systhreads && cp otherlibs/systhreads/threads.h $DIRNAME/lib/ocaml/caml/threads.h && cd ..
