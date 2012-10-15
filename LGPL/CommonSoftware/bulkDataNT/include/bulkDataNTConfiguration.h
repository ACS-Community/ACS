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
* "@(#) $Id: bulkDataNTConfiguration.h,v 1.38 2012/10/15 09:58:10 bjeram Exp $"
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

/**
 * Should return true if the value of ENABLE_BULKDATA_NT is set to:
 * 1, y, Y or true (ignorecase)
 * @return true or false
 */
bool isBulkDataNTEnabled();

///DDS configuration class at this moment it is common for all configuration classes
//  ... but can be later split if needed
class DDSConfiguration
{
	friend class BulkDataNTDDS;
	friend class BulkDataNTStream;
	friend class BulkDataNTDDSSubscriber;
	friend class BulkDataNTDDSPublisher;
	friend class BulkDataConfigurationParser;
public:
	DDSConfiguration();

	/**
	 * Default qos_library to use
	 */
	static const char* const DEFAULT_LIBRARY;

	/**
	 * Default qos_profile to use for sender streams
	 */
	static const char* const DEFAULT_SENDER_STREAM_PROFILE;

	/**
	 * Default qos_profile to use for sender flows
	 */
	static const char* const DEFAULT_SENDER_FLOW_PROFILE;

	/**
	 * Default qos_profile to use for receiver streams
	 */
	static const char* const DEFAULT_RECEIVER_STREAM_PROFILE;

	/**
	 * Default qos_profile to use for receiver flows
	 */
	static const char* const DEFAULT_RECEIVER_FLOW_PROFILE;

	/**
	 * Default qos_profile to use when we create QoS using API
	 */
	static const char* const DEFAULT_API_CREATE_PROFILE;

	/// BulkData NT debug level. The value is read from env. variable BULKDATA_NT_DEBUG
	/// now it effects the whole BD running in a single process, but
	/// if needed can be later re-factor that can be set per stream/flow
	static short debugLevel;

	std::string getStringProfileQoS() { return stringProfileQoS; }

protected:
	/**
	 * It tries to read BULKDATA_NT_DEBUG and if it is there set the value of debugLevel
	 */
	static void setDebugLevelFromEnvVar();

	/**
	 * sets DDSLogVerbosity depend on the debug level
	 */
	static void setDDSLogVerbosity();

	std::string libraryQos;  /// QoS configuration library
	std::string profileQos;  /// QoS configuration profile in the library that should be used

	/**
	 * Sets configuration
	 * @param cfg
	 * @param defaultProfile
	 */
	void setStringProfileQoS(char* cfg, const char *defaultProfile);

	 /**
	  * * Sets configuration with profile name
	  * @param profileName
	  * @param cfg
	  * @param defaultProfile
	  */
	void setStringProfileQoS(char* profileName, char* cfg, const char *defaultProfile);

	std::string stringProfileQoS;  /// her goes DDS QoS Profile

	// QoS that follow can be hardcoded, but is more flexible in this way.
	static bool ignoreUserProfileQoS; //when true USER_QOS_PROFILES.xml in current folder would not be loaded
	static bool ignoreEnvironmentProfileQoS; //when true NDDS_QOS_PROFILES will be ignored

	static unsigned int DDSLogVerbosity; // log level for RTI DDS, the type should be NDDS_Config_LogVerbosity

	static const char* const DEFAULT_QoS_FILE;
	static std::string urlProfileQoS;   // here we specify where it should be looked for default values = DEFAULT_QoS_FILE
	void fillUrlProfileQoS(const char* envVar, const char *dilim="");
};//DDSConfiguration


/// common class for Sender and Receiver configuration
class StreamConfiguration : public DDSConfiguration
{
	friend class BulkDataNTStream;
	friend class BulkDataConfigurationParser;
public:
	StreamConfiguration();

	bool isParticipantPerStream() const {	return participantPerStream;	}
	void setParticipantPerStream(bool participantPerStream) {	this->participantPerStream = participantPerStream;	}

    static bool DEFAULT_PARTICIPANT_PER_STREAM;
protected:
	bool participantPerStream;  ///It flags if we have a participant per stream or just a single participant
};//StreamConfiguration


/****************************************/
/* Sender-side configuration structures */
/****************************************/

/** A Sender flow configuration */
class  SenderFlowConfiguration : public DDSConfiguration
{
public:
	SenderFlowConfiguration();

	/**
	 * Sets DDS QoS directly using XML
	 * @param cfg (same value that can be given to DDSReceiverFlowQoS elemtn in CDB
	 */
	void setDDSSenderFlowQoS(char *cfg);

	/**
	 * Sets DDS QoS directly using XML + profile name (if we need to have it unique)
	 * @param profileName
	 * @param cfg
	 */
	void setDDSSenderFlowQoS(char *profileName, char* cfg);


	double getACKsTimeout() const;
    double getSendFrameTimeout() const;
    void setACKsTimeout(double acKsTimeout);
    void setSendFrameTimeout(double frameTimeout);

    static double DEFAULT_SENDFRAME_TIMEOUT;
    static double DEFAULT_ACKs_TIMEOUT;
protected:
    double sendFrameTimeout;
	double ACKsTimeout;
};

/** A Sender stream configuration. It consists in a seres
 *  of sender flow configurations */
class SenderStreamConfiguration : public StreamConfiguration
{
public:
	SenderStreamConfiguration();

	/**
	 * Sets DDS QoS directly using XML
	 * @param cfg (same value that can be given to DDSReceiverFlowQoS elemtn in CDB
	 */
	void setDDSSenderStreamQoS(char *cfg);

	/**
	 * Sets DDS QoS directly using XML + profile name (if we need to have it unique)
	 * @param profileName
	 * @param cfg
	 */
	void setDDSSenderStreamQoS(char *profileName, char* cfg);


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

	/**
	 * Sets DDS QoS directly using XML
	 * @param cfg (same value that can be given to DDSReceiverFlowQoS elemtn in CDB
	 */
	void setDDSReceiverFlowQoS(char *cfg);

	/**
	 * Sets DDS QoS directly using XML + profile name (if we need to have it unique)
	 * @param profileName
	 * @param cfg
	 */
	void setDDSReceiverFlowQoS(char *profileName, char* cfg);

	double getCbReceiveProcessTimeout() const;
    void setCbReceiveProcessTimeout(double cbReceiveProcessTimeout);

    double getCbReceiveAvgProcessTimeout() const;
    void setCbReceiveAvgProcessTimeout(double cbReceiveAvgProcessTimeout);

    bool isEnableMulticast() const;
    void setEnableMulticast(bool enableMulticast);

    std::string getMulticastAddress() const;
    void setMulticastAddress(std::string multicastAddress);

    unsigned short getUnicastPort() const;
    void setUnicastPort(unsigned short);

    /// default multicast address for receiver flow
    static const char* const DEFAULT_MULTICAST_ADDRESS;

    /// default cbReceiveProcessTimeout value
    static double DEFAULT_CBRECEIVE_PROCESS_TIMEOUT;

    /// default cbReceiveAvgProcessTimeout value
    static double DEFAULT_CBRECEIVE_AVG_PROCESS_TIMEOUT;

    static bool DEFAULT_ENABLE_MULTICAST; /// default enableMulticast value

    static unsigned short DEFAULT_UNICAST_PORT;  /// default unicvast port; 0 means that DDS will choose the port

protected:
    double cbReceiveProcessTimeout; /// how long should max take execution of cbReceive
    double cbReceiveAvgProcessTimeout; /// how long should in avergae take execution of cbReceive
    bool enableMulticast; /// is multicast enabled, otherwise unicast
    std::string multicastAddress; /// multicast address, used only if multicast is anbel (enableMulticast==true)
    unsigned short unicastPort; /// unicast address, used only if multicast is disabled (enableMulticast==false). If it is 0 the DDS will coose the port
};

/** A Receiver stream configuration. It consists in a seres
 *  of receiver flow configurations, and also of the receiver type */
class ReceiverStreamConfiguration : public StreamConfiguration
{
public:

	ReceiverStreamConfiguration();

	/**
	 * Sets DDS QoS directly using XML
	 * @param cfg (same value that can be given to DDSReceiverFlowQoS elemtn in CDB
	 */
	void setDDSReceiverStreamQoS(char *cfg);

	/**
	 * Sets DDS QoS directly using XML + profile name (if we need to have it unique)
	 * @param profileName
	 * @param cfg
	 */
	void setDDSReceiverStreamQoS(char *profileName, char* cfg);

	bool isUseIncrementUnicastPort() const { return useIncrementUnicastPort;	}
	void setUseIncrementUnicastPort(bool usePort) {	this->useIncrementUnicastPort = usePort;}

	unsigned short getBaseUnicastPort() const {	return baseUnicastPort;	}
	void setBaseUnicastPort(unsigned short port) { this->baseUnicastPort = port;	}

	static bool DEFAULT_USE_INCREMENT_UNICAST_PORT; /// default use increment

	static unsigned short DEFAULT_BASE_UNICAST_PORT;  /// default base unicast port
protected:
	ReceiverType type;
	/// base port for unicast, if it is configured to use incremental port number for flows in the stream (useIncramentUnicastPort=true) ...
	/// ... #ReceiverFlowConfiguration::unicastPort takes precedence if define (!=0)
	unsigned short baseUnicastPort;
	bool useIncrementUnicastPort; /// if increment port numbers should be used for flows in the stream. The port is calculated as increment to the #baseUnicasPort
};//ReceiverStreamConfiguration

};

#endif /*!_H*/
