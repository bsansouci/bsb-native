# Linux build script
# Run this with docker installed to build bsb-native for linux using holy build box.
# https://github.com/phusion/holy-build-box

# Creates a bsb-native-linux-2.1.1.zip in the bsb-native repo root

SCRIPTDIR="$( cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd )"
docker run -t -i --rm -v $SCRIPTDIR/..:/io phusion/holy-build-box-64:latest bash /io/scripts/linux-build-helper.sh
