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

dnl This is from not yet released guile.m4 GUILE_MODULE_CHECK.
dnl Delete it after guile-1.4.1.98 install.
AC_DEFUN([AC_GUILE_WWW_MODULE_CHECK],
         [AC_MSG_CHECKING([if $2 $4])
	  GUILE_CHECK($1,[(use-modules $2) (exit ((lambda () $3)))])
	  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
          AC_MSG_RESULT($$1)
         ])

dnl This is from not yet released guile.m4 GUILE_CHECK_ICE9_OPTARGS.
dnl Delete it after guile-1.4.1.98 install.
AC_DEFUN([AC_GUILE_WWW_CHECK_ICE9_OPTARGS],[
  GUILE_MODULE_AVAILABLE($1, (ice-9 optargs-kw))
  if test "$$1" = yes ; then
    $1=no
  else
    AC_GUILE_WWW_MODULE_CHECK($1, (ice-9 optargs),
      [(= 2 ((lambda* (a #:optional b) b) 4 2))],
      [acts like (ice-9 optargs-kw)])
  fi
  AC_SUBST($1)
])

dnl This is an adaptation of "guile-tools autofrisk" output.
AC_DEFUN([AC_GUILE_WWW_MODULES_AOK],[
  GUILE_MODULE_REQUIRED(ice-9 regex)
  dnl Replace with GUILE_CHECK_ICE9_OPTARGS after guile-1.4.1.98 install.
  AC_GUILE_WWW_CHECK_ICE9_OPTARGS(need_optargs_kludge)
])

dnl acinclude.m4 ends here
