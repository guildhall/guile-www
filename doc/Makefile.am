## Makefile.am
##
## Copyright (C) 2007, 2008, 2009, 2010, 2011 Thien-Thi Nguyen
## Copyright (C) 2001, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
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

info_TEXINFOS = guile-www.texi
guile_www_TEXINFOS = version.texi

include $(top_srcdir)/build-aux/common.mk

allstems =			\
 $(top_leaves)			\
 $(data_leaves:%=data/%)	\
 $(su_leaves:%=server-utils/%)

snippets: $(allstems:%=$(top_srcdir)/source/%.scm)
	$(gx) tsar -f $@ -c utf-8 update $?

$(info_TEXINFOS): snippets

.texh.texi:
	$(gx) tsin -f snippets -c utf-8 $<
	if [ '.' = '$(srcdir)' ] ; then : ; else mv $@ $(srcdir)/$@ ; fi

CLEANFILES = guile-www.info guile-www.dvi guile-www_toc.html guile-www.html

EXTRA_DIST = guile-www.texh guile-www.html guile-www.pdf snippets

## Makefile.am ends here
