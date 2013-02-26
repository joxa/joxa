#!/bin/sh

set -e

BINDIR=$( cd "$( dirname "$0" )" && pwd )
JOXA_SRC=./src/joxa.app.src
JOXA_SHELL=./src/joxa-shell.jxa

version=$($BINDIR/semver.sh)

cat ${JOXA_SRC} | perl -p -e "s/({vsn, \")[^\"]*(\"},)/\${1}${version}\${2}/g" > ${JOXA_SRC}.tmp
mv ${JOXA_SRC}.tmp ${JOXA_SRC}

cat ${JOXA_SHELL} | perl -p -e "s/(Joxa Version ).*?(~n~n)/\${1}${version}\${2}/g" > ${JOXA_SHELL}.tmp
mv ${JOXA_SHELL}.tmp ${JOXA_SHELL}
