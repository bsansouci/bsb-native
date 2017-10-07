# bsb-native

Bsb-native is like [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code), but compiles to native OCaml instead.


## Install

1) Add `"bsb-native": "bsansouci/bsb-native"` as a devDependency to your `package.json`
2) Add a `bsconfig.json` like you would for bsb. Bsb-native uses the same schema, located [here](http://bucklescript.github.io/bucklescript/docson/#build-schema.json) with small additions like `entries`.

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

## Run
`./node_modules/.bin/bsb -make-world` (or add an [npm script](https://docs.npmjs.com/misc/scripts) to simply run `bsb -make-world`).

That will pickup the first entry's `kind` and build all entries to that `kind`. e.g if you have multiple `bytecode` targets, they'll all get built but not the `js` ones nor `native` ones. If you want to build to all targets you need to run the build command multiple times with different `-backend`.

## Useful flags
The `-make-world` flag builds all of the dependencies and the project.

The `-w` enabled the watch mode which will rebuild on any source file change.

The `-backend [js|bytecode|native]` flag tells `bsb-native` to build all entries in the `bsconfig.json` which match the kind `js`, `bytecode` or `native`.

The build artifacts are put into the folder `lib/bs`. The bytecode executable would be at `lib/bs/bytecode/index.byte` and the native one at `lib/bs/native/index.native` for example.

## Opam packages
Yes `bsb-native` supports opam packages (see [ocamlfind example](https://github.com/bsansouci/bsb-native-example/tree/ocamlfind-trial))
```js
{
  "name" : "NameOfLibrary",
  "sources" : "src",
  "ocamlfind-dependencies": ["lwt.unix", "lwt.ppx"],
  "entries": [{
    "kind": "bytecode",
    "main": "Index"
  }]
}
```

## bsconfig.json schema
```js
{
    // All of the bsb fields will work as expected. Here are just the added 
    // features.

    // Entries is an array of targets to be built.
    // When running `bsb -backend bytecode`, bsb will filter this array for 
    // all the entries compiling to bytecode and compile _all_ of those.
    "entries": [{
      "kind": "bytecode", // can be "bytecode" (ocamlc), "js" (bsc) or "native" (ocamlopt),
      "main": "MainModule",
    }],

    // Array of opam dependencies.
    "ocamlfind-dependencies": ["lwt.unix"],
    
    // This allows you to write JS specific packages (for example) and depend 
    // on them without bsb choking on them when building to another platform.
    // If you have `MyLibJs` which exposes a module `Bla`, and `MyLibNative` 
    // which also exposes `Bla`, the compiler will use the native `Bla` when
    // compiling to native and the JS `Bla` when compiling to JS thanks to this
    // flag.
    "allowed-build-kinds": "js" // Can be a string or an array, with the same values as "entries".

    // List of relative paths (relative from library dir) to be linked at the 
    // end. Bsb-native doesn't care how those were generated as long as they // are there at the linking stage. Generally you can use `build-script` to
    // build those.
    "static-libraries": ["lib/c/my_lib.o"]

    // Command invoked first, before bsb tries to build anything else.
    // Useful for building C code linked into the exec using 
    // `static-libraries`.
    "build-script": "make"

    // List of flags to be passed to the C linker at the linking stage.
    "c-linker-flags": ["-L/path/to/folder/with/linker/stuff"]
}
```


## Multi-target
`bsb-native` actually supports building Reason/OCaml to JS as well as to native/bytecode. What that enables is cross platform code that depends only on an interface, and bsb-native will choose the right implementation depending on what you target.

For example, you can write code that depends on the module Reasongl, and bsb-native will use `ReasonglNative` as the implementation for Reasongl when building to native/bytecode or it'll use `ReasonglWeb` when building to JS.

Currently the way this works is to have each platform specific dependency expose a module with the same name and rely on the field `allowed-build-kinds` in the `bsconfig.json` to tell bsb-native which one of platform specific dep you want to build, given that you want to build the whole project to a specific target. Say you target JS, then ReasonglWeb will get built which exposes its own [Reasongl](https://github.com/bsansouci/reasongl-web/blob/bsb-support-new/src/reasongl.re) module.

This is _not_ the best way. We're working on improving this by allowing conditional compilation within a project using a PPX.
