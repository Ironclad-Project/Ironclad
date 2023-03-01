#! /bin/sh
#  version.sh: Script for determining Ironclad's version.
#  Copyright (C) 2023 streaksu
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

LC_ALL=C
export LC_ALL

srcdir="$(dirname "$0")"
test -z "$srcdir" && srcdir=.

cd "$srcdir"

[ -f version ] || ( git describe --exact-match --tags $(git log -n1 --pretty='%h') 2>/dev/null || git log -n1 --pretty='%h' ) | sed 's/^v//g' | xargs printf '%s'
[ -f version ] && ( cat version 2>/dev/null ) | xargs printf '%s'
