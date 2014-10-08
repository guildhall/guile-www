## singles-update.mk
##
## Copyright (C) 2014 Thien-Thi Nguyen
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

## Hint: (find-file "../autogen.sh")

from = ../build-aux/common.mk

include $(from)

singles: $(from)
	count=`echo $(top_leaves) $(data_leaves) $(su_leaves) | wc -w` ; \
	printf 'single += x%03d\n' `seq 1 $$count` > $@

## singles-update.mk ends here
