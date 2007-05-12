## Process this file with automake to produce Makefile.in.
##
##  Copyright (C) 2001, 2003, 2004, 2005, 2006,
##     2007 Free Software Foundation, Inc.
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
##   to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA  02110-1301  USA

AUTOMAKE_OPTIONS = gnu

SUBDIRS = doc examples

wwwdir = $(GUILE_SITE)/www
basic = cgi.scm http.scm main.scm url.scm url-coding.scm utcsec.scm

dw = $(DESTDIR)$(wwwdir)

install-data-local:
	$(mkinstalldirs) $(dw) $(dw)/server-utils $(dw)/data
	files="`cd $(srcdir) ; ls $(basic) server-utils/*.scm data/*.scm`" ; \
	for f in $$files ; do rm -f $(dw)/$$f ; done ; \
	$(SHELL) ./module-install $(dw) $$files

install-data-hook:
	rm -f $(dw)/METAINFO
	if $(GUILE_TOOLS) | grep -q make-module-catalog ; then	\
	   $(GUILE_TOOLS) make-module-catalog $(GUILE_SITE) ; fi

if MAINTAINER_MODE

SUFFIXES = .scm .doc
.scm.doc:
	$(top_srcdir)/doc-snarf -D -o $@ $<

DOT_DOC_FILES = cgi.doc http.doc main.doc url.doc url-coding.doc utcsec.doc \
		server-utils/big-dishing-loop.doc \
		server-utils/parse-request.doc \
		server-utils/filesystem.doc \
		server-utils/cgi-prep.doc \
		server-utils/cookies.doc \
		server-utils/answer.doc \
		server-utils/log.doc

.doc-index: $(DOT_DOC_FILES)
	guile-tools make-twerp2texi-index -o $@ $(DOT_DOC_FILES)

CLEANFILES = *.doc

cvs-state-summary:
	cd $(srcdir) ; \
	ttn-do display-cvs-state-summary . doc $(asisdirs)

endif ## MAINTAINER_MODE

asisdirs = server-utils data contrib

EXTRA_DIST = $(basic) $(asisdirs) doc-snarf module-install.in

dist-hook:
	for d in $(asisdirs) ; do			\
	  rm -rf `find $(distdir)/$$d -name CVS`	\
		 `find $(distdir)/$$d -name '*.doc'` ;	\
	done

## Makefile.am ends here
