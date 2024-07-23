#! /bin/sh
# Copyright (C) 2023-2024 mintsuki and contributors.
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
# REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

LC_ALL=C
export LC_ALL

srcdir="$(dirname "$0")"
test -z "$srcdir" && srcdir=.

cd "$srcdir"

if [ -f version ]; then
    printf '%s' "$(cat version)"
    exit 0
fi

if ! [ -d .git ] || ! git log -n1 --pretty='%h' >/dev/null 2>&1; then
    printf 'UNVERSIONED'
    exit 0
fi

tmpfile="$(mktemp)"

if ! git describe --exact-match --tags $(git log -n1 --pretty='%h') >"$tmpfile" 2>/dev/null; then
    echo g$(git log -n1 --pretty='%h') >"$tmpfile"
fi

printf '%s' "$(sed 's/^v//g' <"$tmpfile")"

rm -f "$tmpfile"
