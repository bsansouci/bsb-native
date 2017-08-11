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

## Use

`./node_modules/.bin/bsb -backend bytecode -make-world -w` (or add an [npm script](https://docs.npmjs.com/misc/scripts))

The `-make-world` flag builds all of the dependencies.

The `-w` enabled the watch mode which will rebuild on any source file change.

The `-backend` flag tells `bsb-native` to build all entries marked as building to bytecode.

The build artifacts are put into a folder called `lib`. The executable, in this example, would be at `lib/bs/bytecode/index.byte`

## Multi-target
`bsb-native` actually supports building Reason/OCaml to JS as well as to native/bytecode. What that enables is code that is truly cross platform, that depends on a JS implementation of a library or a native implementation, and bsb-native will build the right implementation depending on what you target.

For example, you can write code that depends on the module Reasongl, and bsb-native will use `ReasonglNative` as the implementation for Reasongl when building to native/bytecode or it'll use `ReasonglWeb` when building to JS.

Currently the way this works is to have each platform specific dependency expose a module with the same name and rely on the field `allowed-build-kinds` in the `bsconfig.json` to tell bsb-native which one of platform specific dep you want to build, given that you want to build the whole project to a specific target. Say you target JS, then ReasonglWeb will get built which exposes its own [Reasongl](https://github.com/bsansouci/reasongl-web/blob/bsb-support-new/src/reasongl.re) module.
