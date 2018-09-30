# bsb-native

[![NPM](https://nodei.co/npm/bsb-native.png?compact=true)](https://nodei.co/npm/bsb-native/) [![Build Status](https://travis-ci.org/BuckleScript/bucklescript.svg?branch=master)](https://travis-ci.org/bucklescript/bucklescript) [![Coverage Status](https://coveralls.io/repos/github/BuckleScript/bucklescript/badge.svg?branch=master)](https://coveralls.io/github/BuckleScript/bucklescript?branch=master)

Bsb-native is a fork of [bsb](https://bucklescript.github.io/docs/en/build-overview.html) that compiles to native OCaml instead.

## Install

1) Add `"bsb-native": "4.0.6"` as a devDependency to your `package.json`
2) Add a `bsconfig.json` like you would for bsb. Bsb-native uses the same schema, located [here](http://bucklescript.github.io/bucklescript/docson/#build-schema.json) with small additions like `entries` (see below for complete config schema).
3) run `npm install` / `yarn`.

An [example bsconfig.json](https://github.com/bsansouci/bsb-native-example/blob/master/bsconfig.json):
```js
{
  "name" : "NameOfLibrary",
  "sources" : "src",
  "entries": [{
    "backend": "bytecode",
    "main-module": "Index" // Capitalized name of module (not a path)
  }]
}
```

## Run
`./node_modules/.bin/bsb -make-world` (or add an [npm script](https://docs.npmjs.com/misc/scripts) that runs `bsb -make-world`).

That will build the first entry and use its `backend` to build all other entries targetting that `backend`. 

e.g if you have multiple `bytecode` targets, they'll all get built but not the `js` ones nor `native` ones. If you want to build to all targets you need to run the build command multiple times with different `-backend`.

## Initialize a new package

Bsb-native comes with a basic init package to get you started. To create a package named `Hello` run:

```sh
bsb -init Hello
```

And a folder named `Hello` will be created with a basic project layout. If you want to initialize an already created folder, use `.` as the last argument.

## Useful commandline flags
The `-make-world` flag builds all of the dependencies and the project.

The `-clean-world` flag cleans all of the build artifacts.

The `-w` enabled the watch mode which will rebuild on any source file change.

The `-backend [js|bytecode|native]` flag tells `bsb-native` to build all entries in the `bsconfig.json` which match the backend `js`, `bytecode` or `native`.

The build artifacts are put into the folder `lib/bs`. The bytecode executable would be at `lib/bs/bytecode/index.byte` and the native one at `lib/bs/native/index.native` for example.

The `-build-library` flag takes a module name and will pack everything in the current project that depends on it into a library file. Cool trick: you can use this to implement hot reloading for any sort of app, simply call bsb-native with this flag and use the built-in module `Dynlink` to load the file bsb-native generates.

## Opam package support
Yes `bsb-native` supports opam packages (see [ocamlfind example](https://github.com/bsansouci/bsb-native-example/tree/opam-example)).
**BUT** you need to be on the switch `4.02.3+buckle-master` (which you can get to by running `opam switch 4.02.3+buckle-master`).
```js
{
  "name" : "NameOfLibrary",
  "sources" : "src",
  "ocamlfind-dependencies": ["lwt.unix", "lwt.ppx"],
  "entries": [{
    "backend": "bytecode",
    "main-module": "Index"
  }]
}
```

## bsconfig.json schema
```js
{
    // All of the bsb fields will work as expected. 
    // See the bucklescript schema:
    // https://bucklescript.github.io/bucklescript/docson/#build-schema.json
    // Below are just the bsb-native specific features.
    
    // Contains only the added fields, the rest is the same as bsb.
    "sources": [{
      "type": "ppx" // Extra field added to tell bsb-native that a fold contains source for a ppx.
      "ppx": ["Myppx", "theirPpx/theirPpx.exe"] // Array of ppx to run on all files in this directory. If it's a relative path, it's relative to `node_modules`, if it's a module name it's a local ppx.
      }]
  
    // Entries is an array of targets to be built.
    // When running `bsb -backend bytecode`, bsb will filter this array for
    // all the entries compiling to bytecode and compile _all_ of those.
    "entries": [{
      "backend": "bytecode", // Can be an array of string or a string: "bytecode" (ocamlc), "js" (bsc) or "native" (ocamlopt)
      "main-module": "MainModule", // This has to be capitalized
      "output-name": "snake.exe", // Custom executable name.
    }],

    // Array of opam dependencies.
    "ocamlfind-dependencies": ["lwt.unix"],

    // Array of built-in ocaml dependencies that are not Pervasives (ie not
    // linked by default).
    // This is useful for making a ppx, where you need "compiler-libs".
    // All of these except "compiler-libs" is linked by default, so you don't
    // have to worry about it. That said it does increase the binary size so
    // you can remove unused things here.
    "ocaml-dependencies": ["bigarray", "unix", "threads", "compiler-libs"],

    // Array of flags to pass the OCaml compiler. This shouldn't be needed for
    // most things.
    "ocaml-flags": ["-bin-annot"],
    
    // Array of flags passed to ocaml only at the linking phase
    "ocaml-linker-flags": ["-output-obj"],

    // This allows you to write JS specific packages (for example) and depend
    // on them without bsb choking on them when building to another platform.
    // If you have `MyLibJs` which exposes a module `Bla`, and `MyLibNative`
    // which also exposes `Bla`, the compiler will use the native `Bla` when
    // compiling to native and the JS `Bla` when compiling to JS thanks to this
    // flag.
    "allowed-build-kinds": "js", // Can be a string or an array, with the same values as "entries".

    // List of relative paths (relative from library dir) to be linked at the 
    // end. Bsb-native doesn't care how those were generated as long as they
    // are there at the linking stage. Generally you can use `build-script` to
    // build those.
    "static-libraries": ["lib/c/my_lib.o"],

    // File built and ran at compile time. Very useful for generating object files
    // that are then linked into the app.
    // See the C compilation section below for more details about the API exposed.
    "build-script": "build_script.re",

    // List of flags to be passed to the C linker at the linking stage.
    "c-linker-flags": ["-L/path/to/folder/with/linker/stuff"],
}
```

## C compilation
bsb-native allows C compilation through an OCaml/Reason file. To expose that file to bsb you can add `"build-script": "build_script.re"` to your `bsconfig.json`. 

Bsb expose to that file a module called `Bsb_internals` which contains helpers to compile C code.
```reason
/* Command that'll synchronously compile a C file to an object file (or not if `c=false` is passed).  
   c:bool                 Equivalent to the `-c` flag passed to gcc. true by default.
   include_ocaml:bool     Automatically add an include flag for the ocaml runtime. true by default. 
   flags:array(string)    Array of flags passed directly to gcc.
   includes:array(string) Array of paths to folders that will be included during compilation.
   string                 The output file.
   array(string)          The input files. 
*/
let gcc : (?c:bool, ?include_ocaml:bool, ?flags:array(string), ?includes:array(string), string, array(string)) => unit;

/* Contains an absolute path to the current project's node_modules. */
let node_modules : string;
```

Here is a [simple example](https://github.com/bsansouci/reasongl/blob/2364bc0de0dc0d89b85c6bc1fc64b0ceb169038f/build_script.re) of a `build_script.re` file.

The generated artifacts need to be added to the `"static-libraries": []` array inside the `bsconfig.json` in order to be added at the linking phase.

Here's the same [simple example](https://github.com/bsansouci/reasongl/blob/2364bc0de0dc0d89b85c6bc1fc64b0ceb169038f/bsconfig.json#L16)'s `bsconfig.json`.

The `gcc` function exposed to the `build-script` file uses a vendored mingw compiler, which works on all platforms (linux, osx, windows). 

## Multi-target
`bsb-native` actually supports building Reason/OCaml to JS as well as to native/bytecode. What that enables is cross platform code that depends only on an interface, and bsb-native will choose the right implementation depending on what you target.

For example, you can write code that depends on the module Reasongl, and bsb-native will use `ReasonglNative` as the implementation for Reasongl when building to native/bytecode or it'll use `ReasonglWeb` when building to JS.

Currently the way this works is to have each platform specific dependency expose a module with the same name and rely on the field `allowed-build-kinds` in the `bsconfig.json` to tell bsb-native which one of platform specific dep you want to build, given that you want to build the whole project to a specific target. Say you target JS, then ReasonglWeb will get built which exposes its own [Reasongl](https://github.com/bsansouci/reasongl-web/blob/bsb-support-new/src/reasongl.re) module.

### Conditional compilation
If you would like to have all your code in the same package, you can use BuckleScript's [conditional compilation](https://bucklescript.github.io/docs/en/conditional-compilation.html). To do so, place
```ocaml
#if BSB_BACKEND = "bytecode" then
  include MyModule_Native
#elif BSB_BACKEND = "native" then
  include MyModule_Native
#else
  include MyModule_Js
#end

(* We support Darwin, Linux, Windows compile time checks *)
#if OS_TYPE = "Darwin" then 
  external fabs : float -> float = "fabs"
#end
```
inside a file called `MyModule` (for example). When you build to JavaScript (`BSB_BACKEND = "js"`), that module will use the `MyModule_Js` implementation (see [example](https://github.com/Schmavery/reprocessing/blob/2ff7221789dcefff2ae927b8305c938845361d59/src/Reprocessing_Hotreload.ml)). Same for `BSB_BACKEND = "native"` and `BSB_BACKEND = "bytecode"`.

`BSB_BACKEND` value will be filled automatically by `bsb-native`, so you just need to use it at will with the language-level static `if` compilation.

Same for `OS_TYPE`.

Platform specific files (like `MyModule_Native`) should be added to a folder that is only built for that specific backend (`native`, in the `MyModule_Native` case). You can do that by adding this to your `bsconfig.json` file:

```json
"sources": [{
  "dir": "src",
  "subdirs": [{
    "dir": "native",
    "backend": "native"
  }]
}]
```
**Note**: BuckleScript's conditional compilation doesn't work with Reason yet, so any usage of conditional compilation will have to be implemented in OCaml `.ml` files.

### PPX support
We support running ppxes on the whole codebase, like bucklescript with `"ppx-flags"`, but also per `"source"` directory. You can add `ppx: ["my_ppx/my_ppx.exe"]` to any `"source"` directory to run that ppx on the files inside that directory. 

You can also make your own ppx inside you project. To keep things contained, you have to put the code in its own folder which you'll list under `"sources"` with the extra key `"type": "ppx"`. Then add an entry like you would for building a binary, to bytecode specifically, and similarly add to the entry `"type": "ppx"`. Finally add to the `"sources"` that contain the files on which to run that ppx `"ppx": ["Myppx"]` (notice it's just a module name, that tells bsb-native it's a local ppx).

tl;dr
1) add folder to `"sources"` with `"type": "ppx"`
2) add entry built to bytecode with `"type": "ppx"`
3) add `"ppx"` field to right `"sources"` object for files that use that ppx `"ppx": ["Myppx"]`
4) (optional)  bsb-native passes to the ppx a command line argument called `-bsb-backend` followed by the current backend, which is either `"bytecode"`, `"native"` or `"js"`.

Also bsb-native comes with [ppx_tools](https://github.com/ocaml-ppx/ppx_tools) available by default, no config!. Here are some ["docs"](https://github.com/ocaml-ppx/ppx_tools/blob/55825c58535e3600f5c70a0672806a5c3d45eea4/ppx_metaquot.ml#L5).

You can't build ppxes to bytecode or js for now. You also can't have a ppx depend on another ppx. You can't put the ppx code alongside your app code. The ppx will always be built even if you never reference it.
