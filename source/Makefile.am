## Makefile.am
##
## Copyright (C) 2007, 2008, 2009, 2011, 2012 Thien-Thi Nguyen
##
## This file is part of Guile-WWW.
##
## Guile-WWW is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3, or
## (at your option) any later version.
##
## Guile-WWW is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with Guile-WWW; see the file COPYING.  If not,
## write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA  02110-1301  USA

SUBDIRS = server-utils data

wwwdir = $(GUILE_LIBSITE)/www
www_DATA = $(top_leaves)

include $(top_srcdir)/build-aux/comp.mk

EXTRA_DIST += ChangeLog-data ChangeLog-server-utils
EXTRA_DIST += binary-ports.scm-low
EXTRA_DIST += binary-ports.scm-mid

if NEED_BINPORTS
www_DATA += binary-ports
endif

update-site-module-catalog:
if HAVE_MKPIMMC
	$(GUILE_TOOLS) make-module-catalog \
	  -t '$(DESTDIR)' '$(DESTDIR)$(GUILE_LIBSITE)'
endif

install-data-hook:
	$(MAKE) $(AM_MAKEFLAGS) update-site-module-catalog

uninstall-hook:
	rm -rf '$(DESTDIR)$(wwwdir)'
	$(MAKE) $(AM_MAKEFLAGS) update-site-module-catalog

dist-hook:
	rm -f $(distdir)/binary-ports.scm

## Makefile.am ends here
