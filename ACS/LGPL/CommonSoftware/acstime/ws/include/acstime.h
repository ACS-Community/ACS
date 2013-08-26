#ifndef ACSTIME_H
#define ACSTIME_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acstime.h,v 1.7 2011/10/28 15:12:04 hsommer Exp $"
*/
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file acstime.h
 *  This is just provided as a convenience to the developer (combining all the 
 *  ACS C++ time system functionality into one header).
 */
#include <acstimeDevIOTime.h>
#include <acstimeTimeUtil.h>
#include <acstimeEpochHelper.h>
#include <acstimeDurationHelper.h>
#include <acstimeProfiler.h>
#include "acstimeS.h"

// GCH: Commented out implementation include files.
//      They should not be used by clients.
// #include <acstimeTimerImpl.h>
// #include <acstimeClockImpl.h>
////////////////////////////////////////////////////////////////////////
#endif
