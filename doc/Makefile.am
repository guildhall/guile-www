## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 2001,2003,2004 Free Software Foundation, Inc.
##
##   This file is part of GUILE-WWW.
##
##   GUILE-WWW is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as
##   published by the Free Software Foundation; either version 2, or
##   (at your option) any later version.
##
##   GUILE-WWW is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
##   You should have received a copy of the GNU General Public
##   License along with GUILE-PG; see the file COPYING.  If not, write
##   to the Free Software Foundation, Inc., 59 Temple Place, Suite
##   330, Boston, MA 02111-1307 USA

info_TEXINFOS = guile-www.texi
guile_www_TEXINFOS = version.texi

BUILT_SOURCES = doc-index-prep
doc-index-prep:
if MAINTAINER_MODE
	test -h www || ln -s $(top_srcdir) www
	( cd .. ; $(MAKE) .doc-index )
else
	@echo "nothing to do for $@"
endif

guile-www.texi: guile-www.twerp

twerp2texi = guile-tools twerp2texi

.twerp.texi:
	$(twerp2texi) -I ../.doc-index -o $@ $<

CLEANFILES = guile-www.info guile-www.dvi guile-www_toc.html guile-www.html

EXTRA_DIST = guile-www.twerp

## Makefile.am ends here
