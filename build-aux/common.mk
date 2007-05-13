# common.mk
#
# Copyright (C) 2007 Thien-Thi Nguyen
#
# This file is part of Guile-WWW.
#
# Guile-WWW is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version.
#
# Guile-WWW is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with GUILE; see the file COPYING.  If not, write
# to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA  02110-1301  USA

%: %.scm
	$(top_builddir)/build-aux/module-compile -o $@ $<

if MAINTAINER_MODE

SUFFIXES = .scm .doc
.scm.doc:
	$(GUILE_TOOLS) doc-snarf -D -o $@ $<

DOT_DOC_FILES = $(www_DATA:%=%.doc)

CLEANFILES = $(www_DATA) $(DOT_DOC_FILES)

.doc-index: $(DOT_DOC_FILES)
	$(GUILE_TOOLS) make-twerp2texi-index -o $@ $(DOT_DOC_FILES)

endif ## MAINTAINER_MODE

# common.mk ends here
