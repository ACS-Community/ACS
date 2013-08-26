#ifndef logging_ACSStructuredPushSupplierBin_H
#define logging_ACSStructuredPushSupplierBin_H
/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * "@(#) $Id: loggingACSStructuredPushSupplierBin.h,v 1.2 2007/05/28 06:23:39 cparedes Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2001-06-17  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <loggingACSStructuredPushSupplier.h>

/**
 * NOTE: This class contains quite a bit of duplicated code that could be reduced if it inherited
 * from the basencSupplier class - however, due to build order dependency issues, this is not possible.
 */
class ACSStructuredPushSupplierBin : public ACSStructuredPushSupplier 
{
  public:
    // = Initialization and Termination code
    //ACSStructuredPushSupplierBin (void);
    // Constructor.
    virtual void send_event (const CosNotification::StructuredEvent& event);
    // Send one event.
   
};

#endif
