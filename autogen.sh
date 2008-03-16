#!/bin/sh
#
# usage: sh -x autogen.sh
#
# tested with:
# - GNU Automake 1.9.6
# - GNU Autoconf 2.61
# - Guile 1.4.1.114 (see HACKING)

[ -f configure.in ] || {
  echo "autogen.sh: run this command only at the top of a source tree."
  exit 1
}

aclocal -I `guile-config info datadir`/aclocal
autoconf

# automake is not so smooth handling generated .texi
texi='doc/guile-www.texi'
if [ ! -f $texi ] ; then
    echo '@setfilename guile-www.info' > $texi
    echo '@include version.texi' >> $texi
    touch -r configure.in $texi
fi

automake --add-missing

if grep -q "Version 2" COPYING ; then
    v3='../.common/GPLv3'
    if [ -f $v3 ]
    then ln -sf $v3 COPYING
    else echo WARNING: COPYING is v2
    fi
fi

# autogen.sh ends here
