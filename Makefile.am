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
gwww_DATA = cgi.scm http.scm main.scm url.scm url-coding.scm

install-data-hook:
	$(mkinstalldirs) $(gwwwdir)/server-utils
	for f in $(srcdir)/server-utils/*.scm ; do g=`basename $$f` ;	\
	  $(INSTALL_DATA) $$f $(gwwwdir)/server-utils/$$g ; done
	$(mkinstalldirs) $(gwwwdir)/data
	for f in $(srcdir)/data/*.scm ; do g=`basename $$f` ;	\
	  $(INSTALL_DATA) $$f $(gwwwdir)/data/$$g ; done
	if [ "$(need_optargs_kludge)" = yes ] ; then		\
	  sed s/optargs-kw/optargs/ $(srcdir)/cgi.scm > TMP ;	\
	  $(INSTALL_DATA) TMP $(gwwwdir)/cgi.scm ;		\
	  rm -f TMP ; fi
	cd $(gwwwdir) ;						\
	for f in *.scm server-utils/*.scm data/*.scm ; do	\
	  echo ';;; Copyright (C) 2004 FSF' > TMP ;		\
	  $(GUILE_TOOLS) punify $$f >> TMP ; mv TMP $$f ; done
	if $(GUILE_TOOLS) | grep -q make-module-catalog ; then	\
	   $(GUILE_TOOLS) make-module-catalog $(GUILE_SITE) ; fi

if MAINTAINER_MODE

SUFFIXES = .scm .doc
.scm.doc:
	$(top_srcdir)/doc-snarf -D -o $@ $<

DOT_DOC_FILES = cgi.doc http.doc main.doc url.doc url-coding.doc \
		server-utils/parse-request.doc \
		server-utils/filesystem.doc \
		server-utils/answer.doc

.doc-index: $(DOT_DOC_FILES)
	guile-tools make-twerp2texi-index -o $@ $(DOT_DOC_FILES)

CLEANFILES = *.doc

endif

asisdirs = server-utils data contrib examples

EXTRA_DIST = $(gwww_DATA) $(asisdirs) doc-snarf

dist-hook:
	for d in $(asisdirs) ; do			\
	  rm -rf `find $(distdir)/$$d -name CVS`	\
		 `find $(distdir)/$$d -name '*.doc'` ;	\
	done

## Makefile.am ends here
