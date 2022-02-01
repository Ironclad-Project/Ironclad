#! /bin/sh

set -ex

origdir="$(pwd -P)"
srcdir="$(dirname "$0")"
test -z "$srcdir" && srcdir=.

cd "$srcdir"
cp "$(automake --print-libdir)/install-sh" .
autoconf

if test -z "$NOCONFIGURE"; then
    cd "$origdir"
    exec "$srcdir"/configure "$@"
fi
