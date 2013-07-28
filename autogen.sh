#!/bin/sh
#
# usage: sh -x autogen.sh
#
# tested with:
# autoconf (GNU Autoconf) 2.69
# automake (GNU automake) 1.13.4
# guile (GNU Guile) 1.8.7
# guile-baux-tool (Guile-BAUX) 20130705.0751.4969fb4

[ -f configure.ac ] || {
  echo "autogen.sh: run this command only at the top of a source tree."
  exit 1
}

guile-baux-tool snuggle m4 build-aux
guile-baux-tool import re-prefixed-site-dirs \
    common punify tsar tsin gbaux-do

# automake is not so smooth handling generated .texi
texi='doc/guile-www.texi'
if [ ! -f $texi ] ; then
    echo '@setfilename guile-www.info' > $texi
    echo '@include version.texi' >> $texi
    touch -r configure.ac $texi
fi

autoreconf --verbose --install --symlink -Wall

# These override what ‘autoreconf --install’ creates.
# Another way is to use gnulib's config/srclist-update.
actually ()
{
    gnulib-tool --verbose --copy-file $1 $2
}
actually doc/INSTALL.UTF-8 INSTALL
actually doc/fdl.texi

# We aren't really interested in the backup files.
rm -f INSTALL~

# autogen.sh ends here
