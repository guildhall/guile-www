## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2001, 2003, 2004 Free Software Foundation, Inc.
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
##   License along with GUILE; see the file COPYING.  If not, write
##   to the Free Software Foundation, Inc., 59 Temple Place, Suite
##   330, Boston, MA 02111-1307 USA

AUTOMAKE_OPTIONS = gnu

SUBDIRS = doc

gwwwdir = $(GUILE_SITE)/www
gwww_DATA = cgi.scm http.scm main.scm url.scm

install-data-hook:
	if [ "$(need_optargs_kludge)" = yes ] ; then		\
	  sed s/optargs-kw/optargs/ $(srcdir)/cgi.scm > TMP ;	\
	  $(INSTALL_DATA) TMP $(gwwwdir)/cgi.scm ;		\
	  rm -f TMP ; fi
	if $(GUILE_TOOLS) | grep -q make-module-catalog ; then	\
	   $(GUILE_TOOLS) make-module-catalog $(GUILE_SITE) ; fi

if MAINTAINER_MODE

SUFFIXES = .scm .doc
.scm.doc:
	$(top_srcdir)/doc-snarf -D -o $@ $<

DOT_DOC_FILES = cgi.doc http.doc main.doc url.doc
.doc-index: $(DOT_DOC_FILES)
	guile-tools make-twerp2texi-index -o $@ $(DOT_DOC_FILES)

CLEANFILES = *.doc

endif

EXTRA_DIST = $(gwww_DATA) contrib examples doc-snarf

dist-hook:
	rm -rf `find $(distdir)/contrib -name CVS`
	rm -rf `find $(distdir)/examples -name CVS`

## Makefile.am ends here
