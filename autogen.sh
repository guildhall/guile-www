#!/bin/sh

[ -f configure.in ] || {
  echo "autogen.sh: run this command only at the top of a source tree."
  exit 1
}

aclocal
autoconf
automake --add-missing --verbose
