## comp.mk
##
## Copyright (C) 2010, 2011, 2012 Thien-Thi Nguyen
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
## You should have received a copy of the GNU General Public License
## along with Guile-WWW.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/common.mk

scheme = $(www_DATA:%=%.scm)

.scm:
	$(gx) mm -o $@ $(MMFLAGS) $<

EXTRA_DIST = $(scheme)

CLEANFILES = $(www_DATA)

SUFFIXES = .scm

CLEANFILES += .stamp-all
.stamp-all: $(www_DATA)
	@touch $@

## comp.mk ends here
