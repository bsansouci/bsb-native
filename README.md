# bsb-native

Bsb-native is a fork of [bsb](https://bucklescript.github.io/docs/en/build-overview.html) that compiles to native OCaml instead.

## Install

1) Add `"bs-platform": "bsansouci/bsb-native#2.1.1"` as a devDependency to your `package.json`
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
    // See the bucklescript schema: https://bucklescript.github.io/bucklescript/docson/#build-schema.json
    // Below are just the bsb-native specific features.

    // Entries is an array of targets to be built.
    // When running `bsb -backend bytecode`, bsb will filter this array for
    // all the entries compiling to bytecode and compile _all_ of those.
    "entries": [{
      "backend": "bytecode", // can be "bytecode" (ocamlc), "js" (bsc) or "native" (ocamlopt),
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

    // Command invoked first, before bsb tries to build anything else.
    // Useful for building C code linked into the exec using
    // `static-libraries`.
    "build-script": "make",

    // List of flags to be passed to the C linker at the linking stage.
    "c-linker-flags": ["-L/path/to/folder/with/linker/stuff"],
}
```

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
```
inside a file called `MyModule` (for example). When you build to JavaScript (`BSB_BACKEND = "js"`), that module will use the `MyModule_Js` implementation (see [example](https://github.com/Schmavery/reprocessing/blob/2ff7221789dcefff2ae927b8305c938845361d59/src/Reprocessing_Hotreload.ml)). Same for `BSB_BACKEND = "native"` and `BSB_BACKEND = "bytecode"`.

`BSB_BACKEND` value will be filled automatically by `bsb-native`, so you just need to use it at will with the language-level static `if` compilation.

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
