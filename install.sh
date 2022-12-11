#!/bin/sh
# install.sh: Install script for ironclad and docs.
# Copyright (C) 2021 streaksu
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

set -e

if [ -z "$PREFIX" ]; then
    PREFIX="/usr/local"
fi

# Build documentation.
makeinfo docs/ironclad.texi
makeinfo --pdf docs/ironclad.texi

# Install info.
install -d "$DESTDIR$PREFIX"/share/info
install -d "$DESTDIR$PREFIX"/share/docs
install ironclad.info "$DESTDIR$PREFIX"/share/info/ironclad
install ironclad.pdf  "$DESTDIR$PREFIX"/share/docs/

# Install the executable.
install -d "$DESTDIR$PREFIX"/share/ironclad
if [ "$1" = "strip" ]; then
    install -s ironclad "$DESTDIR$PREFIX"/share/ironclad/
else
    install ironclad "$DESTDIR$PREFIX"/share/ironclad/
fi
