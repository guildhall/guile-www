## Makefile.am
##
## Copyright (C) 2012 Thien-Thi Nguyen
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

include $(top_srcdir)/build-aux/common.mk

TESTS_ENVIRONMENT = ./v

XFAIL_TESTS =

TESTS =
TESTS += x000

# Unfortunately, this list must track the count of items listed
# in ../build-aux/common.mk (i.e., be manually maintained).
single = \
 x001 x002 x003 x004 x005 x006 x007 x008 x009 x010 \
 x011 x012 x013 x014 x015 x016 x017 x018 x019
TESTS += $(single)

explicit = \
 x100 \
 x200 \
 x300

TESTS += $(explicit)

EXTRA_DIST = $(explicit)

all-modules.list:
	@echo Creating $@ ... ;						\
	n=0 ;								\
	z () {								\
	  h="$$1" ; shift ;						\
	  for m ; do							\
	    n=`expr 1 + $$n` ;						\
	    printf '((%s %s) #:prefix z%d-)\n' "$$h" "$$m" $$n ;	\
	  done ;							\
	} ;								\
	{ z 'www' $(top_leaves) ;					\
	  z 'www server-utils' $(su_leaves) ;				\
	  z 'www data' $(data_leaves) ;					\
	} > $@

check_DATA = .fake

## This hair is so that we can do, e.g.:
##  touch ../source/**/foo.scm && make check
bsrc = $(top_builddir)/source
witnesses = \
$(bsrc)/.stamp-all \
$(bsrc)/data/.stamp-all \
$(bsrc)/server-utils/.stamp-all

.PHONY: $(witnesses)

$(witnesses):
	@cd `dirname $@` && $(MAKE) .stamp-all

.stamp-all: $(witnesses)
	@touch -r `ls -t $(witnesses) | head -n 1` $@

.fake: .stamp-all
	@echo Doing .fake install ... ;		\
	top=`pwd`/.fake ;			\
	rm -rf $$top ;				\
	cd ../source ;				\
	$(MAKE) $(AM_MAKEFLAGS) install		\
	  FOR_MAKE_CHECK=1			\
	  prefix="$$top"

x000: all-modules.list
	@printf "(use-modules `cat $<`)\n(exit #t)\n" > $@

$(single): all-modules.list
	@n=`echo $@ | sed 's/x0*//'` ; \
	sed -e "$${n}!d" -e 's/.*/(use-modules &)\n(exit #t)/' $< > $@

clean-local:
	-rm -rf all-modules.list .fake x000 $(single)

## Makefile.am ends here
