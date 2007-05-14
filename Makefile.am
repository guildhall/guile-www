## Process this file with automake to produce Makefile.in.
##
##  Copyright (C) 2001, 2003, 2004, 2005, 2006,
##     2007 Free Software Foundation, Inc.
##
##   This file is part of Guile-WWW.
##
##   Guile-WWW is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as
##   published by the Free Software Foundation; either version 2, or
##   (at your option) any later version.
##
##   Guile-WWW is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
##   You should have received a copy of the GNU General Public
##   License along with Guile-WWW; see the file COPYING.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA  02110-1301  USA

AUTOMAKE_OPTIONS = gnu

SUBDIRS = server-utils data doc examples

wwwdir = $(GUILE_SITE)/www
www_DATA =					\
 cgi						\
 http						\
 main						\
 url						\
 url-coding					\
 utcsec

include $(top_srcdir)/build-aux/common.mk

install-data-hook:
	rm -f $(DESTDIR)$(wwwdir)/METAINFO
	if $(GUILE_TOOLS) | grep -q make-module-catalog ; then	\
	   $(GUILE_TOOLS) make-module-catalog $(GUILE_SITE) ; fi

if MAINTAINER_MODE

cvs-state-summary:
	cd $(srcdir) ; \
	ttn-do display-cvs-state-summary . $(SUBDIRS) $(asisdirs)

endif ## MAINTAINER_MODE

asisdirs = contrib

EXTRA_DIST += $(asisdirs)

dist-hook:
	for d in $(asisdirs) ; do			\
	  rm -rf `find $(distdir)/$$d -name CVS`	\
		 `find $(distdir)/$$d -name '*.doc'` ;	\
	done

## Makefile.am ends here
