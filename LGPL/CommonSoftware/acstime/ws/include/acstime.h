/* @(#) $Id: acstime.h,v 1.6 2004/09/09 22:15:35 dfugate Exp $
 *
 * Copyright (C) 2001
 * Associated Universities, Inc. Washington DC, USA.
 *
 * Produced for the ALMA project
 *
 * This library is free software; you can redistribute it and/or modify it it 
 * under the terms of the GNU Library General Public License as published by 
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
 * License for more details.
 *
 * You should have received a copy of the GNU Library General Public License 
 * along with this library; if not, write to the Free Software Foundation, 
 * Inc., 675 Massachusetts Ave, Cambridge, MA, 02139, USA.
 *
 * Correspondence concerning ALMA should be addressed as follows:
 * Internet email: alma-sw-admin@nrao.edu
 */
////////////////////////////////////////////////////////////////////////
#ifndef ACSTIME_H
#define ACSTIME_H
////////////////////////////////////////////////////////////////////////
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
