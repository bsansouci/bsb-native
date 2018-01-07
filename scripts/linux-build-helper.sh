#!/bin/bash
set -ex

################################################################
# don't call this script directly, call linux-build.sh instead #
################################################################

# install zip in order to create the zip package later
yum install -y zip

# activate the Holy Build Box environment.
source /hbb/activate

# build ocaml
cd /io/vendor/ocaml
DIRNAME=`pwd` /io/vendor/ocaml/configure -prefix `pwd`  -no-ocamldoc -no-ocamlbuild -no-curses -no-graph -no-debugger
make -j9 world.opt
make install
mkdir -p $DIRNAME/lib/ocaml/caml
make -C otherlibs/systhreads
cp otherlibs/systhreads/threads.h $DIRNAME/lib/ocaml/caml/threads.h

# copy ninja binary
cp /io/vendor/ninja-build/ninja.linux64 /io/lib/ninja.exe

# build bsc/bsb
cd /io
PATH=/io/vendor/ocaml:$PATH make
PATH=/io/vendor/ocaml:$PATH make install

# create zip package
rm -f bsb-native-linux-2.1.1.zip
zip -r bsb-native-linux-2.1.1.zip lib vendor/ocaml/bin/ocamlrun vendor/ocaml/ocamlc.opt vendor/ocaml/ocamlopt.opt vendor/ocaml/lib/ocaml -x lib/bsb -x lib/bsc -x lib/bsrefmt
