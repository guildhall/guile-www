#!/bin/sh
#
# usage: sh -x autogen.sh
#
# tested with:
# - autoconf (GNU Autoconf) 2.65
# - automake (GNU automake) 1.11
# - Guile 1.4.1.118

[ -f configure.ac ] || {
  echo "autogen.sh: run this command only at the top of a source tree."
  exit 1
}

ln -sf $(guile-config info datadir)/aclocal/guile.m4 build-aux/guile.m4
aclocal -I build-aux
autoconf

# automake is not so smooth handling generated .texi
texi='doc/guile-www.texi'
if [ ! -f $texi ] ; then
    echo '@setfilename guile-www.info' > $texi
    echo '@include version.texi' >> $texi
    touch -r configure.ac $texi
fi

automake --add-missing

# autogen.sh ends here
