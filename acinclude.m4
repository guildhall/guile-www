dnl	Copyright (C) 2004 Free Software Foundation, Inc.
dnl
dnl  This file is part of GUILE-WWW
dnl
dnl  GUILE-WWW is free software; you can redistribute it and/or modify it
dnl  under the terms of the GNU General Public License as published by
dnl  the Free Software Foundation; either version 2, or (at your
dnl  option) any later version.
dnl
dnl  GUILE-WWW is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  General Public License for more details.
dnl
dnl  You should have received a copy of the GNU General Public License
dnl  along with GUILE; see the file COPYING.  If not, write to the
dnl  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
dnl  Boston, MA 02111-1307, USA.

dnl This is an adaptation of "guile-tools autofrisk" output.
AC_DEFUN([AC_GUILE_WWW_MODULES_AOK],[
  GUILE_MODULE_REQUIRED(ice-9 regex)
  GUILE_MODULE_REQUIRED(srfi srfi-13)
  GUILE_CHECK_ICE9_OPTARGS(need_optargs_kludge)
])

dnl This enables dependency tracking machinery so that "make" automatically
dnl updates doc/guile-www.info whenever any *.scm file changes.
AC_DEFUN([AC_GUILE_WWW_TWERP_PREP],[
  AC_CONFIG_COMMANDS([twerp-prep],[
    if echo "$ac_cs_version" | grep -q options.*--enable-maintainer-mode ; then
      if guile-tools list | grep -q twerp2texi ; then
        guile-tools prep-Ptexi doc/Makefile guile-www
      else
        AC_MSG_WARN([[automatic doc rebuild not supported; you need to]])
        AC_MSG_WARN([[edit doc/guile-www.texi manually if source changes]])
      fi
    fi
  ])
])

dnl acinclude.m4 ends here
