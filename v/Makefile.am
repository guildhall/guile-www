## Makefile.am
##
## Copyright (C) 2012, 2013, 2014 Thien-Thi Nguyen
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

TESTS_ENVIRONMENT = ./v

XFAIL_TESTS =

TESTS =
TESTS += x000

single =
include singles
TESTS += $(single)

explicit = \
 x100 \
 x110 \
 x120 \
 x200 \
 x210 \
 x300 \
 x310

TESTS += $(explicit)

EXTRA_DIST = $(explicit) common x310.d
EXTRA_DIST += singles singles-update.mk

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

DISTCLEANFILES = testing-locale

check_DATA = .fake testing-locale

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
	sed -e "$${n}!d" -e 's/.*/(use-modules &) (exit #t)/' $< > $@

# Try to find the first .UTF-8 locale; default to C.UTF-8 on failure.
testing-locale:
	locale -a > $@T
	sed -n -e '/[.]UTF-8$$/{' -e p -e q -e '}' $@T > $@
	test -s $@ || echo C.UTF-8 > $@
	rm -f $@T

clean-local:
	-rm -rf all-modules.list .fake x000 $(single)

installcheck-local:
	$(MAKE) check prefix='$(prefix)'

## Makefile.am ends here
