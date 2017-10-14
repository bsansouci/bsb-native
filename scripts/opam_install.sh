#!/bin/bash

# This is ported to bash for opam from the install.js script.
# See install.js for a comments.

vendor_ninja_version="1.8.2"
root_dir="$( cd $(dirname $(dirname "${BASH_SOURCE[0]}")) && pwd )"

ninja_bin_output="$root_dir/bin/ninja.exe"
ninja_source_dir="$root_dir/vendor/ninja"
ninja_build_dir="$root_dir/vendor/ninja-build"

build_ninja() {
    echo "No prebuilt Ninja, building Ninja now"
    cd $ninja_source_dir
    ./configure.py --bootstrap
    cp "$ninja_source_dir/ninja" $ninja_bin_output
    echo "ninja binary is ready: $ninja_bin_output"
    cd -
}

# // sanity check to make sure the binary actually runs. Used for Linux. Too many variants
test_ninja_compatible() {
    binary_path=$1
    version=`$binary_path --version`
    if [ "$version" == "$vendor_ninja_version" ]; then
        return 1
    else
        return 0
    fi
};

unameOut="$(uname -s)"
ninja_os_path=""
case "${unameOut}" in
    Linux*)     ninja_os_path="$ninja_build_dir/ninja.linux64";;
    Darwin*)    ninja_os_path="$ninja_build_dir/ninja.darwin";;
    *)          machine="UNKNOWN:${unameOut}"
esac

if [ -f $ninja_bin_output ]; then
    if test_ninja_compatible $ninja_bin_output; then
        echo "ninja binary is already cached: $ninja_bin_output"
    fi
else
    if [ -f $ninja_os_path ]; then
        cp $ninja_os_path $ninja_bin_output
        if test_ninja_compatible $ninja_bin_output; then
            echo "ninja binary is copied from pre-distribution"
        else 
            build_ninja
        fi
    else 
        build_ninja
    fi
fi

MAKE="make"
if [[ "$OSTYPE" == "freebsd"* ]]; then
    MAKE="gmake"
fi

echo "Build a local version of OCaml compiler, it may take a couple of minutes"
$root_dir/scripts/buildocaml.sh

echo "configure again with local ocaml installed"
OCAML_VERSION=`ocamlc.opt -version`
VERSION_WE_WANT="4.02.3"
if [ "${OCAML_VERSION/$VERSION_WE_WANT}" == "$OCAML_VERSION" ]; then
    RED='\033[0;31m'
    BLUE='\033[0;36m'
    GREEN='\033[0;32m'
    NOCOLOR='\033[0m'
    echo "${RED}ERROR${NOCOLOR}: Global ocaml has version ${RED}${OCAML_VERSION}${NOCOLOR} while bsb-native only supports ${GREEN}${VERSION_WE_WANT}${NOCOLOR}. Please run '${BLUE}opam switch 4.02.3${NOCOLOR}' to get a compatible compiler and stdlib."
    exit 1
else 
    # Run opam_config_compiler here with the path given by opam so that we generate a good
    # bsb_default_paths.ml
    # For local dev, we'll rely on the makefile's rules to generate this correctly
    # 
    # Packing command:
    # ./bin/bspack.exe -I ext -I ../scripts -bs-main Opam_config_compiler -o ../scripts/opam_config_compiler_packed.ml
    BIN_DIR=$1
    $root_dir/vendor/ocaml/ocamlc.opt -g -o $root_dir/scripts/opam_config_compiler.exe $root_dir/vendor/ocaml/otherlibs/unix/unix.cma $root_dir/scripts/opam_config_compiler_packed.ml
    $root_dir/scripts/opam_config_compiler.exe $BIN_DIR
    
    echo "config finished"

    cd "$root_dir/jscomp"
    echo "Build the compiler and runtime .. "
    $MAKE world
    echo "Installing"
    $MAKE VERBOSE=true install

fi
