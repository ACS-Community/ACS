/*
    nls.h - NLS system interface
    Copyright (c) 2000  Frodo Looijaard <frodol@dds.nl>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* This file is a small hack to keep both the stand-alone package and
 * the util-linux versions happy.
 * In the stand-alone version, we define NOT_UTIL_LINUX and use our own
 * nls.h definitions; in the util-linux version, we use the global
 * util-linux nls definitions
 */

#ifdef NOT_UTIL_LINUX

#ifndef GETOPT_NLS
#define GETOPT_NLS

#define PACKAGE "getopt"

#ifndef WITHOUT_GETTEXT
#include <libintl.h>
#define _(Text) gettext (Text)
#else /* def WITHOUT_GETTEXT */
#define _(Text) (Text)
#undef bindtextdomain
#define bindtextdomain(Domain,Directory) /* empty */
#undef textdomain
#define textdomain(Domain) /* empty */
#endif /* ndef WITHOUT_GETTEXT */

#endif /* def GETOPT_NLS */

#else /* not NOT_UTIL_LINUX */
#include "../lib/nls.h"
#endif /* NOT_UTIL_LINUX */
