#!/bin/sh
#
# usage: sh -x autogen.sh
#
# tested with:
# - GNU Automake 1.9.6
# - GNU Autoconf 2.60
# - Guile 1.4.1.107 (see HACKING)

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
    touch -m 01010000 $texi
fi

automake --add-missing

# autogen.sh ends here
