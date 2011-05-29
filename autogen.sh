#!/bin/sh
#
# usage: sh -x autogen.sh
#
# tested with:
# autoconf (GNU Autoconf) 2.68
# automake (GNU automake) 1.11.1
# guile (Unofficial Guile) 1.4.1.122
# guile-baux-tool (Guile-BAUX) 20110527.1157.3b3268a

[ -f configure.ac ] || {
  echo "autogen.sh: run this command only at the top of a source tree."
  exit 1
}

guile-baux-tool import common punify tsar tsin gbaux-do

# automake is not so smooth handling generated .texi
texi='doc/guile-www.texi'
if [ ! -f $texi ] ; then
    echo '@setfilename guile-www.info' > $texi
    echo '@include version.texi' >> $texi
    touch -r configure.ac $texi
fi

autoreconf --verbose --install --symlink -Wall

# autogen.sh ends here
