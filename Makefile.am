## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2001 Free Software Foundation, Inc.
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

gwwwdir = $(datadir)/guile/www
gwww_DATA = cgi.scm http.scm main.scm url.scm

EXTRA_DIST = $(gwww_DATA) contrib examples

dist-hook:
	rm -rf `find $(distdir)/contrib -name CVS`
