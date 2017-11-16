# bsb-native

Bsb-native is a fork of [bsb](http://bucklescript.github.io/bucklescript/Manual.html#_bucklescript_build_system_code_bsb_code) that compiles to native OCaml instead.


## Install

1) Add `"bs-platform": "bsansouci/bsb-native"` as a devDependency to your `package.json`
2) Add a `bsconfig.json` like you would for bsb. Bsb-native uses the same schema, located [here](http://bucklescript.github.io/bucklescript/docson/#build-schema.json) with small additions like `entries`.
3) run `npm install` (_warning_: currently this doesn't work with npm5. You can downgrade to npm4 using `npm i -g npm@latest-4` or use yarn)

For [example](https://github.com/bsansouci/BetterErrors/tree/bsb-support):
```json
{
  "name" : "NameOfLibrary",
  "sources" : "src",
  "entries": [{
    "backend": "bytecode",
    "main-module": "Index"
  }]
}
```

## Run
`./node_modules/.bin/bsb -make-world` (or add an [npm script](https://docs.npmjs.com/misc/scripts) that runs `bsb -make-world`).

That will pickup the first entry's `backend` and build all entries to that `backend`. e.g if you have multiple `bytecode` targets, they'll all get built but not the `js` ones nor `native` ones. If you want to build to all targets you need to run the build command multiple times with different `-backend`.

## Useful flags
The `-make-world` flag builds all of the dependencies and the project.

The `-clean-world` flag cleans all of the build artifacts.

The `-w` enabled the watch mode which will rebuild on any source file change.

The `-backend [js|bytecode|native]` flag tells `bsb-native` to build all entries in the `bsconfig.json` which match the backend `js`, `bytecode` or `native`.

The build artifacts are put into the folder `lib/bs`. The bytecode executable would be at `lib/bs/bytecode/index.byte` and the native one at `lib/bs/native/index.native` for example.

## Opam packages
Yes `bsb-native` supports opam packages (see [ocamlfind example](https://github.com/bsansouci/bsb-native-example/tree/ocamlfind-trial)). 
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
    // All of the bsb fields will work as expected. Here are just the added 
    // features.

    // Entries is an array of targets to be built.
    // When running `bsb -backend bytecode`, bsb will filter this array for 
    // all the entries compiling to bytecode and compile _all_ of those.
    "entries": [{
      "backend": "bytecode", // can be "bytecode" (ocamlc), "js" (bsc) or "native" (ocamlopt),
      "main-module": "MainModule",
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
    // end. Bsb-native doesn't care how those were generated as long as they // are there at the linking stage. Generally you can use `build-script` to
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

### matchenv
If you would like to have all your code in the same package, you can use [matchenv](https://github.com/bsansouci/matchenv). That package allows you to write something like
```
include
  [%matchenv
    switch (BSB_BACKEND) {
    | "bytecode" => MyModule_Native
    | "native" => MyModule_Native
    | "js" => MyModule_Js
   }];
```
inside a file called `MyModule` (for example). Then when you build to JS that module will use the `MyModule_Js` implementation. Same for native/bytecode. This is deeply integrated into bsb-native to make everything easier.


# Contributing

The code is inside `jscomp`.

### Dev
1) To contribute you need opam and you need to be on the right switch.

OSX using [homebrew](https://brew.sh)
```
brew install opam
```

Linux
```
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
```

Windows isn't figured out yet.

2) Configure opam
```
opam init --comp=4.02.3+buckle-master
eval `opam config env`

opan switch reinstall 4.02.3+buckle-master # do this if you get errors even from a clean compilation
```

3) Install camlp4
```
opam install camlp4
```

4) Build ocaml. Run in the bucklescript directory
```
./scripts/buildocaml.sh
```

5) Build bsb-native. This command can be the only command used to build while developing.
```
make worldnative
```


Each time you make a change you can run `make worldnative`, it'll incrementally only re-build the right stuff.
If a change isn't picked up you can `make clean` and try again.


### Release
Before making a PR (or at the end of a PR) you should build the release files, which are the packed files inside jscomp/bin. Each file represents one exec and contains all the ML code for that exec.

To do that simply run
```
make force-nativesnapshot
```

You can test those generated artifacts by running 
```
make -C bin all
```
_note_: this is not the same as `make bin/all`. The former will use `./bin`'s makefile to figure out what the rule `all` means. The latter will look into the current directory's makefile for a rule called `bin/all`,
