dnl Some -*-autoconf-*- macros for Guile-WWW.
dnl
dnl	Copyright (C) 2004,2005,2006 Free Software Foundation, Inc.
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
dnl  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
dnl  Boston, MA  02110-1301  USA

dnl Determine if `make-shared-substring' is available; if not, set
dnl shell var $1 to "no" and mark it for substitution, as by AC_SUBST.
AC_DEFUN([AC_GUILE_WWW_HAVE_MAKE_SHARED_SUBSTRING],[
  AC_MSG_CHECKING([if Guile has make-shared-substring])
  GUILE_CHECK([$1],[make-shared-substring])
  if test 0 = $$1 ; then $1=yes ; else $1=no ; fi
  AC_MSG_RESULT([$$1])
  AC_SUBST([$1])
])

dnl Add configure option `--disable-shsub'.
dnl $1 is a shell var to set to "no" if `make-shared-substring'
dnl should be replaced by `substring'.  If Guile doesn't provide
dnl `make-shared-substring', this will be set to "no" regardless
dnl of the configure option.
AC_DEFUN([AC_GUILE_WWW_DISABLE_SHARED_SUBSTRING],[
  AC_GUILE_WWW_HAVE_MAKE_SHARED_SUBSTRING([$1])
  if test no = $$1 ; then : ; else
    AC_ARG_ENABLE([shsub],
      AS_HELP_STRING([--disable-shsub],
                     [replace "make-shared-substring" with "substring"
                      (default: replace only if not available)]),
      [test no = $enableval -o no = $$1 && $1=no])
  fi
])

dnl Encapsulate Guile core-capabilities checks.
AC_DEFUN([AC_GUILE_WWW_CORE_AOK],[
  AC_GUILE_WWW_DISABLE_SHARED_SUBSTRING([use_shsub])
])

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

dnl Find a command to reduce footprint of an installed scheme module.
dnl The command must take a single arg, the filename to process, and
dnl write its output to stdout.  Set shell var $1 to that command (which
dnl may contain spaces) and mark it for substitution, as by AC_SUBST.
AC_DEFUN([AC_GUILE_WWW_PROG_PUNIFY],[
  AC_MSG_CHECKING([punify command])
  if guile-tools | grep -q punify ; then
    if guile-tools punify --newline-after-top-level-form /dev/null 2>/dev/null
    then $1='guile-tools punify --newline-after-top-level-form'
    else $1='guile-tools punify'
    fi
  else
    $1='sed -e s/^;.*//g;/./,/^$/!d'
  fi
  AC_MSG_RESULT([$$1])
  AC_SUBST([$1])
])

dnl Encapsulate installation methodology
AC_DEFUN([AC_GUILE_WWW_INST_METH_AOK],[
  GUILE_SITE_DIR
  AC_GUILE_WWW_PROG_PUNIFY(PUNIFY)
])

dnl acinclude.m4 ends here
