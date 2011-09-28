## snuggle.m4 (serial 5)
##
## Copyright (C) 2011 Thien-Thi Nguyen
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with this program; see the file COPYING.  If not,
## write to the Free Software Foundation, Inc., 51 Franklin
## Street, Fifth Floor, Boston, MA 02110-1301 USA

# SNUGGLE_SET_SOFIXFLAGS
#
# Set shell var @code{SOFIXFLAGS} and mark it for
# substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([SNUGGLE_SET_SOFIXFLAGS],[
AC_REQUIRE([AC_CANONICAL_HOST])
AC_CACHE_CHECK([sofix flags],[host_cv_sofixflags],
[AS_CASE([$host_os],
  [linux-gnu],[host_cv_sofixflags=no-la,no-symlinks],
  [host_cv_sofixflags=ln-s-lib])])
SOFIXFLAGS="$host_cv_sofixflags"
AC_SUBST([SOFIXFLAGS])
])

# SNUGGLE_PROGS -- set paths to Guile interpreter, config and tool programs
#
# Usage: SNUGGLE_PROGS
#
# Look for programs @code{guile}, @code{guile-config} and
# @code{guile-tools}, and set variables @var{GUILE}, @var{GUILE_CONFIG} and
# @var{GUILE_TOOLS}, to their paths, respectively.  If either of the first two
# is not found, signal error.
#
# Mark the variables for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([SNUGGLE_PROGS],[
AC_PATH_PROG([GUILE],[guile])
AS_IF([test x = x"$GUILE"],
      [AC_MSG_ERROR([guile required but not found])])
AC_PATH_PROG([GUILE_CONFIG],[guile-config])
AS_IF([test x = x"$GUILE_CONFIG"],
      [AC_MSG_ERROR([guile-config required but not found])])
AC_PATH_PROG([GUILE_TOOLS],[guile-tools])
AC_SUBST([GUILE])
AC_SUBST([GUILE_CONFIG])
AC_SUBST([GUILE_TOOLS])
])

# GUILE_FLAGS -- set flags for compiling and linking with Guile
#
# Usage: GUILE_FLAGS
#
# This macro runs the @code{guile-config} script, installed with Guile, to
# find out where Guile's header files and libraries are installed.  It sets
# two variables, @var{GUILE_CFLAGS} and @var{GUILE_LDFLAGS}.
#
# @var{GUILE_CFLAGS}: flags to pass to a C or C++ compiler to build code that
# uses Guile header files.  This is almost always just a @code{-I} flag.
#
# @var{GUILE_LDFLAGS}: flags to pass to the linker to link a program against
# Guile.  This includes @code{-lguile} for the Guile library itself, any
# libraries that Guile itself requires (like -lqthreads), and so on.  It may
# also include a @code{-L} flag to tell the compiler where to find the
# libraries.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([SNUGGLE_FLAGS],[
AC_REQUIRE([SNUGGLE_PROGS])dnl
AC_CACHE_CHECK([libguile compile flags],[snuggle_cv_compile],
[snuggle_cv_compile=`$GUILE_CONFIG compile`])
AC_CACHE_CHECK([libguile link flags],[snuggle_cv_link],
[snuggle_cv_link=`$GUILE_CONFIG link`])
GUILE_CFLAGS="$snuggle_cv_compile"
GUILE_LDFLAGS="$snuggle_cv_link"
AC_SUBST([GUILE_CFLAGS])
AC_SUBST([GUILE_LDFLAGS])
])

dnl SNUGGLE_GUILE_LIBSITE_DIR(CACHE-VAR-PREFIX)
dnl
dnl Use Guile-BAUX program ‘re-prefixed-site-dirs’ to set shell-variable
dnl CACHE-VAR-PREFIX_cv_minstroot, which is subsequently also copied to
dnl var ‘GUILE_LIBSITE’, marked for substitution, as by ‘AC_SUBST’.
dnl
AC_DEFUN([SNUGGLE_GUILE_LIBSITE_DIR],[
AC_REQUIRE([SNUGGLE_PROGS])
AC_CACHE_CHECK([module installation root dir],[$1_cv_minstroot],[
saved_GUILE_LOAD_PATH="$GUILE_LOAD_PATH"
GUILE_LOAD_PATH=
eval `GUILE="$GUILE" \
      $ac_aux_dir/guile-baux/gbaux-do \
      re-prefixed-site-dirs "$GUILE_CONFIG" $1`
GUILE_LOAD_PATH="$saved_GUILE_LOAD_PATH"
AS_UNSET([saved_GUILE_LOAD_PATH])
])
GUILE_LIBSITE="$][$1_cv_minstroot"
AC_SUBST([GUILE_LIBSITE])
])])

dnl SNUGGLE_GUILE_TOOLS_EXISTSP(CACHE-VAR,PROGRAM)
dnl
dnl Check if "guile-tools" lists PROGRAM.  If so, set
dnl shell variable CACHE-VAR to "yes", otherwise "no".
dnl
AC_DEFUN([SNUGGLE_GUILE_TOOLS_EXISTSP],[
AC_REQUIRE([SNUGGLE_PROGS])
AC_CACHE_CHECK([for "guile-tools $2"],[$1],
[AS_IF([$GUILE_TOOLS | grep "^$2$" 1>/dev/null 2>&1],[$1=yes],[$1=no])])
])])

# SNUGGLE_CHECK -- evaluate Guile Scheme code and set a variable
#
# Usage: SNUGGLE_CHECK(var,check)
#
# Set @var{var} to @code{yes} or @code{no} depending on the return
# value of having @code{$GUILE -c} evaluate @var{check}.
#
# @var{check} is one or more Guile Scheme expression, the last of
# which should return either 0 or non-@code{#f} to indicate success.
# Non-0 number or @code{#f} indicates failure.
# This is conventionally achieved by wrapping the last expression in
# @code{exit}.  For example, @code{(foo) (bar) (exit (baz))}.
#
# Avoid using the character @samp{#} (hash) since that can confuse
# Autoconf.  You can use @samp{@%:@} (at-percent-colon-at), instead.
# @xref{Quadrigraphs,,,autoconf}.
#
AC_DEFUN([SNUGGLE_CHECK],[
AC_REQUIRE([SNUGGLE_PROGS])
AS_IF([$GUILE -c "$2" > /dev/null 2>&1],
      [AS_VAR_SET([$1],[yes])],
      [AS_VAR_SET([$1],[no])])
])

# SNUGGLE_MODULE_CHECK -- check feature of a Guile Scheme module
#
# Usage: SNUGGLE_MODULE_CHECK(var,module,featuretest,description)
#
# Set @var{var} based on whether or not @var{module} supports @var{featuretest}.
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols w/o paresn, like: @code{ice-9 common-list}.
# @var{featuretest} is an expression acceptable to GUILE_CHECK, q.v.
# @var{description} is a present-tense verb phrase (passed to AC_MSG_CHECKING).
#
AC_DEFUN([SNUGGLE_MODULE_CHECK],[
AS_VAR_PUSHDEF([cv],[guile_cv_$1])dnl
AC_CACHE_CHECK([if ($2) $4],[cv],
[SNUGGLE_CHECK([cv],[(use-modules ($2)) (exit ((lambda () $3)))])])
AS_VAR_SET([$1],[$cv])
AS_VAR_POPDEF([cv])dnl
])

# SNUGGLE_MODULE_AVAILABLE -- check availability of a Guile Scheme module
#
# Usage: SNUGGLE_MODULE_AVAILABLE(module-name)
#
# @var{module-name} is a list of symbols, without surrounding parens,
# like: @code{ice-9 common-list}.  This sets the shell variable
# have_mod_@var{module-name} to @code{yes} or @code{no}.
#
AC_DEFUN([SNUGGLE_MODULE_AVAILABLE],[
AS_VAR_PUSHDEF([var],[have_mod_$1])dnl
SNUGGLE_MODULE_CHECK(var,[$1],[0],[is available])
AS_VAR_POPDEF([var])dnl
])

# SNUGGLE_CHECK_ICE9_OPTARGS -- use (ice-9 optargs) for (ice-9 optargs-kw)?
#
# Usage: SNUGGLE_CHECK_ICE9_OPTARGS(var)
#
# Check if module @code{(ice-9 optargs-kw)} is available.  If so, set
# shell var @var{var} to "no" (see why below).  Otherwise, check if
# module @code{(ice-9 optargs)} acts like @code{(ice-9 optargs-kw)}.
# If so, set @var{var} to "yes", otherwise set it to "no".
#
# Some versions of Guile provide a module @code{(ice-9 optargs)} that
# acts like @code{(ice-9 optargs-kw)} (and subsequently omit the latter,
# instead of providing both).  Code that uses @code{(ice-9 optargs-kw)}
# solely can be textually kludged to load @code{(ice-9 optargs)} in
# these situations if @var{var} has value "yes" (and you @code{AC_SUBST})
# it.  Here is a Makefile.am fragment that demonstrates the technique:
#
# @example
# install-data-hook:
#         if test "$(need_optargs_kludge)" = yes ; then \
#            sed s/optargs-kw/optargs/ foo.scm > TMP ; \
#            mv TMP foo.scm ; \
#         fi
# @end example
#
# In this example, @var{var} is @code{need_optargs_kludge}.  If it turns
# out @code{(ice-9 optargs-kw)} is available, @code{need_optargs_kludge}
# would have value "no", and the kludge would neither be required nor
# applied.
#
AC_DEFUN([SNUGGLE_CHECK_ICE9_OPTARGS],[
SNUGGLE_MODULE_AVAILABLE([ice-9 optargs-kw])
AS_IF([test xyes = x"$have_mod_ice_9_optargs_kw"],
[$1=no],
[SNUGGLE_MODULE_CHECK([$1],[ice-9 optargs],
  [(= 2 ((lambda* (a @%:@:optional b) b) 4 2))],
  [acts like (ice-9 optargs-kw)])])
])

# SNUGGLE_CHECK_META_SWITCH_MINUS_E_STRING(CACHE-VAR)
#
# Check if meta-switch invocation can handle -e STRING.
# If so, set CACHE-VAR to "yes", otherwise "no".
#
AC_DEFUN([SNUGGLE_CHECK_META_SWITCH_MINUS_E_STRING],[
AC_REQUIRE([SNUGGLE_PROGS])
AC_CACHE_CHECK([if meta-switch parsing handles -e STRING],[$1],[
cat > conftest-dwms <<EOF
@%:@! $GUILE \\
-e "(a b c)" -s
!@%:@
 (define-module (a b c) @%:@:export (main))
 (define (main args) (exit @%:@t))
EOF
chmod +x conftest-dwms
AS_IF([./conftest-dwms 1>conftest-dwms.out 2>conftest-dwms.err],
      [$1=yes],
      [$1=no])
])])

# SNUGGLE_GUILE_USER_PROVIDES(var,name)
#
# Check if module @code{(guile-user)} provides @var{name}.
# If so, set @var{var} to "yes", otherwise "no".
# The value is cached in @code{guile_cv_@var{var}}.
#
AC_DEFUN([SNUGGLE_GUILE_USER_PROVIDES],
[SNUGGLE_MODULE_CHECK([$1],[guile-user],[$2],[provides ‘$2’])])

## snuggle.m4 ends here
