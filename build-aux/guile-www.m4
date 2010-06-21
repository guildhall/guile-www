dnl guile-www.m4 --- Some -*-autoconf-*- macros for Guile-WWW
dnl
dnl Copyright (C) 2009 Thien-Thi Nguyen
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

dnl Encapsulate installation methodology
AC_DEFUN([AC_GUILE_WWW_INST_METH_AOK],[
  GUILE_SITE_DIR
])

dnl Idiomatic syntactic sugar
AC_DEFUN([AC_GUILE_WWW_CONFIG_SCRIPT],[
  AC_CONFIG_FILES([$1],[chmod +x $1])
])

dnl guile-www.m4 ends here
