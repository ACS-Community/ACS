#ifndef baci_H
#define baci_H

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baci.h,v 1.108 2007/06/12 08:02:23 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bgustafs 2001-07-12 changed name of parameter in setCompletion
* msekoran  2001/03/04 modified
*/

/** 
 * @file 
 * Header file BACI.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include "acsutil.h"
#include "baciExport.h"

#include "acsutilTimeStamp.h"
#include "baciThread.h"
#include "baciTypes.h"
#include "baciValue.h"
#include "baciCORBA.h"
#include "baciRegistrar.h"

#include "logging.h"

#include "baciError.h"
#include "acserr.h"
#include "ACSErrTypeMonitor.h"
#include "ACSErrTypeOK.h"
#include <baciErrTypeProperty.h>

#include <algorithm>
#include <map>
#include <vector>
#include <deque>

#include "baciBACIAction.h"
#include "baciBACICallback.h"
#include "baciBACIMonitor.h"
#include "baciBACIProperty.h"
#include "baciBACIComponent.h"

#include "archiveeventsArchiveSupplier.h"

namespace baci {

/**
 * ACS_ARCHIVE is a macro used to log something to the archive. As far as I know,
 * it currently does not actually send anything to the real ALMA archive.
 * @param device Some device. Presumably this is a component's name. String.
 * @param param Name of the property. String.
 * @param type Type of the value to be archived???
 * @param value Value to be archived. Most simple CORBA types should work fine for this
 * as sendEvent is templated. I.e., value could be "joe", 33ULL, 3.14, etc
 */ 
#define ACS_ARCHIVE(device, param, type, value) \
ArchiveSupplierSingleton::Instance().sendEvent(0, getTimeStamp(), device, param, value);

/**
 * ACS_ARCHIVE_PRIORITY is a macro used to log something to the archive
 * with a given priority. As far as I know,
 * it currently does not actually send anything to the real ALMA archive.
 * @param device Some device. Presumably this is a component's name. String.
 * @param param Name of the property. String.
 * @param type Type of the value to be archived???
 * @param value BACIValue object to be archived.
 * @param prio Priority at which the value is logged.
 */ 
#define ACS_ARCHIVE_PRIORITY(device, param, type, value, prio) \
{ \
CORBA::Any any; \
value.getAny(any); \
ArchiveSupplierSingleton::Instance().send_event(prio, getTimeStamp(), device, param, any, "");  \
}


/* ------------------------------------------------------------------------ */

 }; 

#endif /* baci_H */ 


