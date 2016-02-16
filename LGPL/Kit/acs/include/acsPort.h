/*************************************************************************
* E.S.O. - ALMA project
*
* "@(#) $Id: acsPort.h.Linux,v 1.2 2009/12/21 13:11:50 psivera Exp $" 
*
* Port.h for Linux
*
* who        when       what
* --------  ----------  ----------------------------------------------
* eallaert  2014-10-10  copied from Kit/vlt/vltPort.h.Linux and acs-ified
*
*/

/*
#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2014
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************
*/

/************************************************************************
*  acsPort.h - Include file to mask differences between platforms.
*              This file should be included in all source files.
*              It relies on macro definitions preceeding the
*              inclusion of this file.
*                
*  REMARK: This file belongs to the "ACS/LGPL/Kit/acs" module.
*------------------------------------------------------------------------
*/

#ifndef ACSPORT_H
#define ACSPORT_H

#ifdef __GNUC__
#define ATTRIBUTE_UNUSED __attribute__((__unused__))
#else
#define ATTRIBUTE_UNUSED
#endif


/*
* When it is used, acsPort.h MUST be the very first file included
* in ANSI ".c" files.
* Cause a syntax error if we detect that any other include file has been
* included before acsPort.h in an ANSI ".c" file.
*/
#if defined(__STDC__) && \
                         (defined(_H_STANDARDS) || \
                          defined(_SYS_STDSYMS_INCLUDED) || \
                          defined(_STANDARDS_H_))
#    error "acsPort.h MUST BE THE VERY FIRST FILE INCLUDED IN ANSI '.c' FILES"
#endif

/*
 * This file is used also by some VxWorks code.
 * To be compatible with existing code, SUN_COMP is defined for both
 * gcc and cc68k, but the following definitiond do not influence cc68k
 */
 
#define LINUX

/* 
 * at present, SELECT is defined in the code using it. It should be done here
 * for all. May be in the next release.
 */

/*
 * Adjust name-space information.
 */
#if defined(_ALL_SOURCE)
#    undef _POSIX_C_SOURCE
#endif

#if defined(_XOPEN_SOURCE)
#    undef _POSIX_C_SOURCE
#endif
  
/* for Thomas Ebert re. ipc.h AH 29/09/99. BGI:Modified in _GNU_SOURCE 06/10/99 */
#ifndef __cplusplus
#define _GNU_SOURCE
#endif

/* For CCS_Lite, just like on Solaris. */
#ifndef MAKE_VXWORKS
#include <stddef.h>
#include <sys/types.h>
#include <sys/time.h>
 
#if !defined(timercmp)
    struct timeval
        {
        long      tv_sec;         /* seconds */
        long      tv_usec;        /* and microseconds */
        };
#define crTIMEVAL_TIMEZONE_DEFINED
#endif
#endif /* MAKE_VXWORKS */

#endif /*!ACSPORT_H*/
