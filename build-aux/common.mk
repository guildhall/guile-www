## common.mk
##
## Copyright (C) 2007, 2008, 2009, 2011 Thien-Thi Nguyen
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

# Guile-BAUX support.

gx = $(top_srcdir)/build-aux/guile-baux/gbaux-do

# Automake doesn't do wildcards (info "(automake) Wildcards"),
# so we list all the filename stems here, separated by directory.
# This way, each source/**/Makefile.am can work "locally", while
# doc/Makefile.am can assemble filenames prefixing parent-dir.

top_leaves =		\
 cgi			\
 http			\
 main			\
 url			\
 url-coding		\
 utcsec

data_leaves =		\
 mime-types		\
 http-status

su_leaves =		\
 answer			\
 big-dishing-loop	\
 cgi-prep		\
 cookies		\
 filesystem		\
 form-2-form		\
 log			\
 modlisp		\
 parse-request

## common.mk ends here
