#!/bin/bash

set -e

BINDIR=$( cd "$( dirname "$0" )" && pwd )
JOXA_SRC=./src/joxa.app.src
JOXA_SHELL=./src/joxa-shell.jxa

version=`$BINDIR/semver.sh`

sed -e "s/\({vsn, \"\).*\(\"\},\)/\1${version}\2/g" ${JOXA_SRC} > ${JOXA_SRC}.tmp
mv ${JOXA_SRC}.tmp ${JOXA_SRC}

sed -e "s/\(Joxa Version \).*\(~n~n\)/\1${version}\2/g" ${JOXA_SHELL} > ${JOXA_SHELL}.tmp
mv ${JOXA_SHELL}.tmp ${JOXA_SHELL}
