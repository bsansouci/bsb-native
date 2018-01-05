bsb-native-example
---

This is a quick demo project to showcase [bsb-native](https://github.com/bsansouci/bsb-native).

For publishing on opam see [opam_of_packagejson](https://github.com/bsansouci/opam_of_packagejson/) as a helper.


## Install
`npm i`

## Build bytecode
`npm run build`

To build other targets (like native or js) you can call bsb directly (or add a npm script) with the `-backend` flag, like `./node_nodules/.bin/bsb -backend native` or `./node_nodules/.bin/bsb -backend js`.

## Run
`./lib/bs/bytecode/index.byte`
