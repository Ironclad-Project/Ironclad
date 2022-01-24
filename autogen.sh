#!/bin/sh

set -ex

origdir="$(pwd -P)"
srcdir="$(dirname "$0")"
test -z "$srcdir" && srcdir=.

cd "$srcdir"
automake --add-missing --copy || true
autoreconf -i

if test -z "$NOCONFIGURE"; then
    cd "$origdir"
    exec "$srcdir"/configure "$@"
fi
