#!/bin/sh

set -ex

srcdir="$(realpath $(dirname "$0"))"
test -z "$srcdir" && srcdir=.

cd "$srcdir"
autoconf

if test -z "$NOCONFIGURE"; then
    exec "$srcdir"/configure "$@"
fi
