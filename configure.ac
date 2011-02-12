dnl configure.ac
dnl
dnl Copyright (C) 2008, 2009, 2010, 2011 Thien-Thi Nguyen
dnl Copyright (C) 2001, 2002, 2003, 2004, 2005,
dnl    2006, 2007 Free Software Foundation, Inc.
dnl
dnl This file is part of Guile-WWW.
dnl
dnl Guile-WWW is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU General Public License as
dnl published by the Free Software Foundation; either version 3, or
dnl (at your option) any later version.
dnl
dnl Guile-WWW is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public
dnl License along with Guile-WWW; see the file COPYING.  If not,
dnl write to the Free Software Foundation, Inc., 51 Franklin Street,
dnl Fifth Floor, Boston, MA  02110-1301  USA

AC_INIT([Guile-WWW],[2.30],[ttn@gnuvola.org])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.11 gnu])

GUILE_PROGS

GUILE_MODULE_REQUIRED([ice-9 and-let-star])
GUILE_MODULE_REQUIRED([ice-9 rdelim])
GUILE_MODULE_REQUIRED([ice-9 regex])
GUILE_MODULE_REQUIRED([ice-9 rw])
GUILE_MODULE_REQUIRED([srfi srfi-13])
GUILE_MODULE_REQUIRED([srfi srfi-14])

GUILE_MODULE_AVAILABLE([optargs_kw], (ice-9 optargs-kw))
AS_VAR_IF([optargs_kw],[yes],[AS_VAR_APPEND([mmflags],[',optargs-kw'])])

GUILE_MODULE_AVAILABLE([currdef], (ice-9 curried-definitions))
AS_VAR_IF([currdef],[no],[AS_VAR_APPEND([mmflags],[',no-currdef'])])

AS_VAR_SET_IF([mmflags],[AS_VAR_SET([MMFLAGS],["-f$mmflags"])])
AC_SUBST([MMFLAGS])

GUILE_SITE_DIR

AC_CONFIG_FILES([
  Makefile
  source/server-utils/Makefile
  source/data/Makefile
  source/Makefile
  doc/Makefile
])

AC_DEFUN([CONFIG_SCRIPT],[AC_CONFIG_FILES([$1],[chmod +x $1])])
CONFIG_SCRIPT([examples/id.cgi])
CONFIG_SCRIPT([examples/wily-guile])

AC_OUTPUT

dnl configure.ac ends here
