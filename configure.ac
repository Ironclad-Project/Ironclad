# configure.ac: Configure script template for the project.
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

AC_PREREQ(2.69)
AC_INIT(Ironclad, 0.0.1, https://github.com/streaksu/ironclad/issues, ironclad)

# Replace the variables in config.
CONFIG_DATE="$(date -R)"
AC_SUBST([PACKAGE_CONFIG_DATE], [$CONFIG_DATE])
AC_CONFIG_FILES(config.ads)
AC_OUTPUT
