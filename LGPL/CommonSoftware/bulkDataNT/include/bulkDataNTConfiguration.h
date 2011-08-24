#ifndef _BULK_DATA_NT_CONFIGURATION_H_
#define _BULK_DATA_NT_CONFIGURATION_H_

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
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
* "@(#) $Id: bulkDataNTConfiguration.h,v 1.10 2011/08/24 14:13:59 bjeram Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* rtobar    011-08-01   created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <map>
#include <string>

namespace AcsBulkdata
{

///DDS configuration class at this moment ii is commm for all classe but can be later split
// ... if needed
class DDSConfiguration
{
	friend class BulkDataNTDDS;
	friend class BulkDataNTDDSSubscriber;
	friend class BulkDataNTDDSPublisher;
public:
	DDSConfiguration();
protected:
	std::string libraryQos;  /// QoS configuration library
	std::string profileQos;  /// QoS configuration profile in the library that should be used
};


/// common class for Sender and Receiver configuration
class StreamConfiguration : public DDSConfiguration
{
	friend class BulkDataNTStream;
	friend class BulkDataConfigurationParser;
public:
	StreamConfiguration();

protected:
	std::string urlProfileQoS;  // here we can specify RTI DDS QoS as a URL/string str://
	// ... it is read just when we create stream, but it contains profiles for all flows
	unsigned int DDSLogVerbosity; // log level for RTI DDS, the type should be NDDS_Config_LogVerbosity
};


/****************************************/
/* Sender-side configuration structures */
/****************************************/

/** A Sender flow configuration */
class  SenderFlowConfiguration : public DDSConfiguration
{
public:
	SenderFlowConfiguration();
};

/** A Sender stream configuration. It consists in a seres
 *  of sender flow configurations */
class SenderStreamConfiguration : public StreamConfiguration
{
public:
	SenderStreamConfiguration();

	//	std::map<const char *, BulkDataNTSenderFlowConfiguration> flows;
};


/******************************************/
/* Receiver-side configuration structures */
/******************************************/

/** Specifies the kind of a receiver. This information is propagated
 *  to the senders so they can take decision based on the importance
 *  of the receiver, in case of any problem. */
enum ReceiverType {
	NORMAL    = 0x1,
	IMPORTANT = 0x2,
	CRITICAL  = 0x4
};

/** A Receiver flow configuration */
class ReceiverFlowConfiguration : public DDSConfiguration
{
public:
	ReceiverFlowConfiguration();
};

/** A Receiver stream configuration. It consists in a seres
 *  of receiver flow configurations, and also of the receiver type */
class ReceiverStreamConfiguration : public StreamConfiguration
{
public:

	ReceiverStreamConfiguration();

protected:
	ReceiverType type;
	//	std::map<const char *, BulkDataNTReceiverFlowConfiguration> flows;
};

};

#endif /*!_H*/
