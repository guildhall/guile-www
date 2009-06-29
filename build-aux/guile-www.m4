dnl guile-www.m4 --- Some -*-autoconf-*- macros for Guile-WWW
dnl
dnl Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.
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

dnl Encapsulate Guile core-capabilities checks.
AC_DEFUN([AC_GUILE_WWW_CORE_AOK],[
])

dnl This is an adaptation of "guile-tools autofrisk" output.
AC_DEFUN([AC_GUILE_WWW_MODULES_AOK],[
  GUILE_MODULE_REQUIRED(ice-9 and-let-star)
  GUILE_MODULE_REQUIRED(ice-9 rdelim)
  GUILE_MODULE_REQUIRED(ice-9 regex)
  GUILE_MODULE_REQUIRED(ice-9 rw)
  GUILE_MODULE_REQUIRED(srfi srfi-13)
  GUILE_MODULE_REQUIRED(srfi srfi-14)
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

dnl Idiomatic syntactic sugar
AC_DEFUN([AC_GUILE_WWW_CONFIG_SCRIPT],[
  AC_CONFIG_FILES([$1],[chmod +x $1])
])

dnl guile-www.m4 ends here
