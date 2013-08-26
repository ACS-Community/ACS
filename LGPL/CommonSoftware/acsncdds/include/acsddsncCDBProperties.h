#ifndef acsnc_cdb_properties_H
#define acsnc_cdb_properties_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: acsddsncCDBProperties.h,v 1.4 2010/02/26 18:13:38 utfsm Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* cmaureir  2010-02-04  created
*/

/** @file acsncddsCDBProperties.h
 *  Header file for classes/functions designed to extract a channel's 
 *  quality of service and admin properties from the ACS CDB.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include <dds/DdsDcpsInfrastructureC.h>
#include <cdbDALC.h>
#include <loggingACEMACROS.h>
#include <iostream>
#include <string>
#include <map>

namespace ddsnc {

    class CDBProperties {

      public:

	static CDB::DAL_ptr
	getCDB();

	static bool 
	cdbChannelConfigExists(CORBA::String_var channelName);

	static DDS::QosPolicyCountSeq
	getCDBQoSProps(CORBA::String_var channelName);

    };
};


#endif
