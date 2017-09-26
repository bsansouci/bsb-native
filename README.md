# bsb-native

Bsb-native is like [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code), but compiles to native OCaml instead.


## Install

1) Add `"bsb-native": "bsansouci/bsb-native"` as a devDependency to your `package.json`
2) Add a `bsconfig.json` like you would for bsb. Bsb-native uses the same schema, located [here](http://bucklescript.github.io/bucklescript/docson/#build-schema.json)

For [example](https://github.com/bsansouci/BetterErrors/tree/bsb-support):
```json
{
  "name" : "NameOfLibrary",
  "sources" : "src",
  "entries": [{
    "kind": "bytecode",
    "main": "Index"
  }]
}
```

<<<<<<< HEAD
## Run
=======
## Getting Help and Providing Feedback

If you need help or have a question, comment, or suggestion, please feel free to [open an
issue](https://github.com/bucklescript/bucklescript/issues).

## Acknowledgements

* Thanks to the  [OCaml](https://ocaml.org) team, obviously, without such a beautiful yet practical language, this backend would not exist
* Thanks to [ninja-build](https://ninja-build.org), BuckleScript also comes with a blazing fast build tool on top of it, `ninja` is a truly [well engineered](http://aosabook.org/en/posa/ninja.html) scalable build tool
* Thanks to [Bloomberg](https://techatbloomberg.com)! This project began at Bloomberg and was published in 2016; without the support of Bloomberg, it would not have happened. Now that the project has grown and developed its own community, it has moved to its own GitHub organization.

## Licensing

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)

The [`ocaml`](ocaml) directory contains the official [OCaml](https://ocaml.org) compiler (version 4.02.3).
Refer to its copyright and license notices for information about its licensing.

The [`ninja-build`](ninja-build) directory contains the official [ninja-build](https://github.com/ninja-build/ninja) (version 1.7.2).
Refer to its copyright and license notices for information about its licensing.

BuckleScript builds on parts of [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml):

* [`jscomp/core/js_dump.ml`](jscomp/core/js_dump.ml) (pretty printer)
* [`jscomp/runtime`](jscomp/runtime)

BuckleScript builds on parts of OCaml:

* [`jscomp/core/lam_pass_exits.ml`](jscomp/core/lam_pass_exits.ml)
* [`jscomp/core/lam_pass_lets_dce.ml`](jscomp/core/lam_pass_lets_dce.ml)
>>>>>>> e7bdff222e57ba1d9a3d430a83cf11c63984930e

`./node_modules/.bin/bsb` (or add an [npm script](https://docs.npmjs.com/misc/scripts) to simply run `bsb`).

<<<<<<< HEAD
That will pickup the first entry's `kind` and build all entries to that `kind`. e.g if you have multiple `bytecode` targets, they'll all get built but not the `js` ones nor `native` ones. If you want to build to all targets you need to run the build command multiple times with different `-backend`.

## Useful flags
The `-make-world` flag builds all of the dependencies.
=======
* [`jscomp/core/js_main.ml`](jscomp/core/js_main.ml)

`jscomp/core/js_main.ml` is adapted from [`ocaml/driver/main.ml`](ocaml/driver/main.ml). It is not
actively used but demonstrates that it is easy to assemble a whole compiler using the OCaml compiler
libraries. It also shows how to add more compilation flags to a JS backend.
>>>>>>> e7bdff222e57ba1d9a3d430a83cf11c63984930e

The `-w` enabled the watch mode which will rebuild on any source file change.

The `-backend [js|bytecode|native]` flag tells `bsb-native` to build all entries in the `bsconfig.json` to either `js`, `bytecode` or `native`.

The build artifacts are put into the folder `lib/bs`. The bytecode executable would be at `lib/bs/bytecode/index.byte` and the native one at `lib/bs/native/index.native`.

## Multi-target
`bsb-native` actually supports building Reason/OCaml to JS as well as to native/bytecode. What that enables is code that is truly cross platform, that depends on a JS implementation of a library or a native implementation, and bsb-native will build the right implementation depending on what you target.

For example, you can write code that depends on the module Reasongl, and bsb-native will use `ReasonglNative` as the implementation for Reasongl when building to native/bytecode or it'll use `ReasonglWeb` when building to JS.

Currently the way this works is to have each platform specific dependency expose a module with the same name and rely on the field `allowed-build-kinds` in the `bsconfig.json` to tell bsb-native which one of platform specific dep you want to build, given that you want to build the whole project to a specific target. Say you target JS, then ReasonglWeb will get built which exposes its own [Reasongl](https://github.com/bsansouci/reasongl-web/blob/bsb-support-new/src/reasongl.re) module.
