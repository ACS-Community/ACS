# doxymacs_parser Makefile
# Copyright (C) 2001 Ryan T. Sammartino
# <ryants at shaw dot ca>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# Doxymacs homepage: http://doxymacs.sourceforge.net/
#
# $Id: Makefile,v 1.26 2002/02/27 10:32:27 vltsccm Exp $

# Double check that these settings make sense for your system.
# (Sorry about not having a configure script... just seems like overkill
# for such a small program)
#
# doxymacs_parser requires libxml version 2.3.4 or greater, which can be
# obtained from http://www.libxml.org/.

CC		= 	gcc

CFLAGS		= 	-O2 -g -Wall -Werror \
			-fexpensive-optimizations -fomit-frame-pointer \
			`xml2-config --cflags`

LDFLAGS		=	`xml2-config --libs`

INSTALL_DIR	=	$(HOME)/bin


# No user serviceable parts below this line.

all: doxymacs_parser

doxymacs_parser: doxymacs_parser.o
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)

.PHONY: install
install: doxymacs_parser
	if [ ! -d $(INSTALL_DIR) ]; then mkdir -p $(INSTALL_DIR) ; fi
	cp -f doxymacs_parser $(INSTALL_DIR)/doxymacs_parser

.PHONY: clean
clean:
	$(RM) doxymacs_parser.o doxymacs_parser
