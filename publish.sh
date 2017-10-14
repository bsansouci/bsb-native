VERSION="1.9.4"
git tag $VERSION
git push fork master
# wait for URL to be created by github
opam-publish prepare bsbnative.${VERSION} https://github.com/bsansouci/bsb-native/archive/${VERSION}.tar.gz
# Copy opam file and descr file

opam-publish submit ./bsbnative.${VERSION}
