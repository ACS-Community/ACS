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
*
* who       when      what
* --------  --------  ----------------------------------------------
* pcolomer 2015-03-19
*/
#include <iostream>
#include <vector>
#include <orbsvcs/CosNamingC.h>
#include <Profile.h>
#include <Stub.h>
#include <PortableServer.h>
#include <Endpoint.h>
#include <logging.h>
#include <acsutilPorts.h>
#include <acscommonC.h>

namespace ns {
	namespace helpers {


		/**
		 * @return true when str has the suffix passed as input parameter. Otherwise return false
		 */
		bool hasSuffix(const std::string &str, const std::string &suffix)
		{
			return str.size() >= suffix.size() &&
				str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0;
		}

		struct NSEntryInfo {
			std::string name;
			std::string kind;
			std::string port;
			std::string endpoint;
		};
		typedef std::vector<NSEntryInfo> NSEntryInfoVector;

		class Filter {
		public:
			Filter() {}
			virtual ~Filter() {}
			virtual bool select(const NSEntryInfo &entryInfo) = 0;
			NSEntryInfoVector m_entries;
		};

		class FilterByEndpoint : public Filter {
		public:
			FilterByEndpoint(const std::string &endpoint) : Filter(), m_endpoint(endpoint) {}
			virtual ~FilterByEndpoint() {}
			virtual bool select(const NSEntryInfo &entryInfo) {
				if(entryInfo.endpoint == m_endpoint)
				{
					m_entries.push_back(entryInfo);
					return true;
				}
				return false;
			}
			std::string m_endpoint;
		};

		class FilterByName : public Filter {
		public:
			FilterByName(const std::string &name) : Filter(), m_name(name) {}
			virtual ~FilterByName() {}
			virtual bool select(const NSEntryInfo &entryInfo)
			{
				if(entryInfo.name == m_name)
				{
					m_entries.push_back(entryInfo);
					return true;
				}
				return false;
			}
			std::string m_name;
		};

		class FilterByNamePort : public Filter {
		public:
			FilterByNamePort(const std::string &name,const std::string &port)
				: Filter(), m_name(name), m_port(port) {}
			virtual ~FilterByNamePort() {}
			virtual bool select(const NSEntryInfo &entryInfo)
			{
				//if(m_name.compare(entryInfo.name) == 0 && m_port.compare(entryInfo.port) == 0)
				if(entryInfo.name == m_name && entryInfo.port == m_port)
				{
					m_entries.push_back(entryInfo);
					return true;
				}
				return false;
			}
			std::string m_name;
			std::string m_port;
		};

		class FilterByKindEndpoint : public Filter {
		public:
			FilterByKindEndpoint(const std::string &kind,const std::string &endpoint)
				: Filter(), m_kind(kind), m_endpoint(endpoint) {}
			virtual ~FilterByKindEndpoint() {}
			virtual bool select(const NSEntryInfo &entryInfo)
			{
				if(entryInfo.kind == m_kind && entryInfo.endpoint == m_endpoint)
				{
					m_entries.push_back(entryInfo);
					return true;
				}
				return false;
			}
			std::string m_kind;
			std::string m_endpoint;
		};


		class FilterByKindEndpointPort : public Filter {
		public:
			FilterByKindEndpointPort(const std::string &kind,const std::string &endpoint,const std::string &port)
				: Filter(), m_kind(kind), m_endpoint(endpoint), m_port(port) {}
			virtual ~FilterByKindEndpointPort() {}
			virtual bool select(const NSEntryInfo &entryInfo)
			{
				if(entryInfo.kind == m_kind && entryInfo.endpoint == m_endpoint && entryInfo.port == m_port)
				{
					m_entries.push_back(entryInfo);
					return true;
				}
				return false;
			}
			std::string m_kind;
			std::string m_endpoint;
			std::string m_port;
		};

		/**
		 * Filter by kind, endpoint, and suffix name
		 */
		class FilterByKindEndpointSuffixName : public Filter {
		public:
			FilterByKindEndpointSuffixName(const std::string &kind,const std::string &endpoint,const std::string &suffix)
				: Filter(), m_kind(kind), m_endpoint(endpoint), m_suffix(suffix) {}
			virtual ~FilterByKindEndpointSuffixName() {}
			virtual bool select(const NSEntryInfo &entryInfo)
			{
				if(entryInfo.kind == m_kind && entryInfo.endpoint == m_endpoint 
				&& hasSuffix(entryInfo.name, m_suffix))
				{
					m_entries.push_back(entryInfo);
					return true;
				}
				return false;
			}
			std::string m_kind;
			std::string m_endpoint;
			std::string m_suffix;
		};
		typedef std::vector<Filter*> PFilterVector;
	}
}

/**
 *
 */
void getEndpoint(CORBA::Object_ptr obj,std::string &endpoint,std::string &port)
{
	TAO_Stub *stub = obj->_stubobj ();
	if (!stub)
	{
		ACS_SHORT_LOG((LM_ERROR, "Invalid Stub"));
		return;
	}

	TAO_Profile* profile = stub->profile_in_use ();
	if (!profile)
	{
		ACS_SHORT_LOG((LM_ERROR, "Invalid Profile"));
		return;
	}

	TAO_Endpoint* endp = profile->endpoint ();
	if (!endp)
	{
		ACS_SHORT_LOG((LM_ERROR, "Invalid Endpoint"));
		return;
	}

	// Display Endpoint
	char buf[256]= {'\0'};
	if (endp->addr_to_string (buf, sizeof(buf)-1u) < 0)
	{
		ACS_SHORT_LOG((LM_ERROR, "Endpoint too long"));
		ACE_OS::strcpy( buf, "{Endpoint too long}" );
	}

	endpoint = std::string(buf);
	size_t pos = endpoint.find_last_of(":");
	if(pos != std::string::npos)
	{
		port = endpoint.substr(pos + 1);
		endpoint = endpoint.substr(0,pos);
	} else {
		port = "";
	}
}

/**
 *
 */
void analyzeList(const CosNaming::NamingContext_ptr nc,
              const CosNaming::BindingList &bl,
			  ns::helpers::PFilterVector &filters)
{
	for (CORBA::ULong i = 0;i < bl.length ();++i)
	{
		CosNaming::Name Name;
		Name.length (1);
		Name[0].id = CORBA::string_dup (bl[i].binding_name[0].id);
		Name[0].kind = CORBA::string_dup (bl[i].binding_name[0].kind);

		CORBA::Object_var obj = nc->resolve (Name);

	    // If this is not a context node
        if (bl[i].binding_type != CosNaming::ncontext)
        {
        	if (CORBA::is_nil (obj.in()))
        	{
        		ACS_SHORT_LOG((LM_ERROR, "Nil Object: %s [%s]",Name[0].id.in(), Name[0].kind.in()));
        	} else {
        		std::string endpoint, port;
        		getEndpoint(obj.in(), endpoint, port);
        		for(ns::helpers::PFilterVector::iterator it = filters.begin();
        				it != filters.end();++it)
        		{
        			ns::helpers::NSEntryInfo entryInfo;
        			entryInfo.name = Name[0].id;
        			entryInfo.kind = Name[0].kind;
        			entryInfo.port = port;
        			entryInfo.endpoint = endpoint;
        			(*it)->select(entryInfo);
        		}
        	}
        }
	}
}

/**
 *
 */
void analyzeNamingEntries(const CosNaming::NamingContext_ptr nc,
                CORBA::ULong max_count,
				ns::helpers::PFilterVector &filters)
{
	CosNaming::BindingIterator_var it;
	CosNaming::BindingList_var bl;

	nc->list (max_count, bl, it);

	analyzeList(nc, bl.in(), filters);

		if (!CORBA::is_nil (it.in ()))
		{
			CORBA::Boolean more;

			do
			{
				more = it->next_n(max_count, bl);
				analyzeList(nc, bl.in(), filters);
			} while (more);

			it->destroy ();
		}
	}

	static const std::string APP_ID = "acsncDelChannelsInNameS";

	static const std::string KIND_CHANNEL = "channels";
	static const std::string KIND_NC_SUPPORT = "NCSupport"; // Notify Channels are registered twice in the Naming Service in order to allow the subscribers to reconnect to them (ICT-4730)

	static const std::string PARAM_IOR_NS = "--name_service";
	static const std::string PARAM_NOTIFY_SERVICE_NAME = "--notify_service";
	static const std::string PARAM_BASEPORT = "--baseport";

	static const std::string ALARM_NOTIFY_CHANNEL = "AlarmNotifyEventChannelFactory";
	static const std::string ARCHIVE_NOTIFY_CHANNEL = "ArchiveNotifyEventChannelFactory";
	static const std::string LOOGING_NOTIFY_CHANNEL = "LoggingNotifyEventChannelFactory";
	static const std::string DEFAULT_NOTIFY_CHANNEL = "NotifyEventChannelFactory";

	void showHelp(const std::string &name)
	{
		std::cout << std::endl;
	std::cout << "\t> " << name << " OPTIONS" << std::endl;
	std::cout << "\tWhere OPTIONS can be:" << std::endl;
	std::cout << "\t" << PARAM_IOR_NS << "  IOR of the Naming Service" << std::endl;
	std::cout << "\t" << PARAM_NOTIFY_SERVICE_NAME << "  Notify Service name or IP:PORT" << std::endl;
	std::cout << "\t" << PARAM_BASEPORT << "  Base port, an integer ranging from 0-9 that's only needed when the Notify Service passed is the name. When the baseport is not set, takes the value from ACS_INSTANCE" << std::endl;
	std::cout << "\t" << "-h, -help, --help  Displays this information" << std::endl;
	std::cout << std::endl;
}

/**
 * Parse command line arguments
 */
void getParams(int argc,char *argv[],
		std::string &iorNamingService,std::string &notifyServiceName,int32_t &baseport)
{
	bool nsNameSet = false;
	bool baseportSet = false;
	iorNamingService = "";
	notifyServiceName = "";
	baseport = -1;

	for(int32_t i = 1;i < argc;++i)
	{
		std::string param(argv[i]);

		if(param == PARAM_IOR_NS)
		{
			if(i + 1 < argc)
			{
				iorNamingService = std::string(argv[i+1]);
			}
		} else if(param == PARAM_NOTIFY_SERVICE_NAME) {
			if(i + 1 < argc)
			{
				notifyServiceName = std::string(argv[i+1]);
				nsNameSet = true;
			}
		} else if(param == PARAM_BASEPORT) {
			if(i + 1 < argc)
			{
				std::string strBaseport = std::string(argv[i+1]);
				baseportSet = true;
				try {
					baseport = atoi(strBaseport.c_str());
				} catch(...) {}
			}
		} else if(param == "-h" || param == "--help" || param == "-help") {
			showHelp(APP_ID);
			exit(0);
		}
	}

	if(iorNamingService.empty())
	{
		ACS_SHORT_LOG((LM_ERROR, "IOR of the Naming Service is not set"));
		showHelp(APP_ID);
		exit(1);
	}

	if(false == nsNameSet)
	{
		ACS_SHORT_LOG((LM_ERROR, "Notify Service is not set"));
		showHelp(APP_ID);
		exit(1);
	}

	if(true == baseportSet && (baseport < 0 || 10 < baseport))
	{
		ACS_SHORT_LOG((LM_ERROR, "Wrong Baseport. Must be an integer between 0-10"));
		showHelp(APP_ID);
		exit(1);
	}
}

/**
 * Main
 */
int main (int argc, char *argv[])
{
	int err = 0;
	CORBA::ORB_var orb;

	ns::helpers::PFilterVector fNotifyService;
	ns::helpers::PFilterVector fNotifyChannels;

	try {
		// Initialize the ORB.
		orb = CORBA::ORB_init(argc, argv);

		// Get command line input arguments
		bool show = false;
		std::string iorNamingService, notifyServiceName;
		int32_t baseport;
		getParams(argc,argv,iorNamingService,notifyServiceName,baseport);

		// Get the logger
		ACS_CHECK_LOGGER;
		getNamedLogger("acsncDelChannelsInNameS");

		// Set the ACS_INSTANCE value as baseport if baseport is not passed
		if(baseport < 0)
		{
			std::string acsInstance = std::getenv("ACS_INSTANCE");
			baseport = atoi(acsInstance.c_str());
		}

		// baseport wrong defined so we exit
		if(baseport < 0 && baseport > 9)
		{
			ACS_SHORT_LOG((LM_ERROR, "Unable to set baseport"));
			exit(1);
		}

		//Get reference to Root POA
		CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
		PortableServer::POA_var poa = PortableServer::POA::_narrow(obj.in());

		// Activate POA Manager
		PortableServer::POAManager_var mgr = poa->the_POAManager();
		mgr->activate();

		// Resolve the Naming Service.
		CORBA::Object_var naming_obj = orb->string_to_object(iorNamingService.c_str());

		//CosNaming::NamingContext_var root =
		//		CosNaming::NamingContext::_narrow(naming_obj.in());
		CosNaming::NamingContextExt_var root =
				CosNaming::NamingContextExt::_narrow(naming_obj.in());

		// Naming service cannot be retrieved. Exit with error.
		if(CORBA::is_nil(root.in()))
		{
			++err;
			ACS_SHORT_LOG((LM_ERROR, "Nil Naming Context reference"));
			try {
				orb->destroy();
			} catch(const CORBA::Exception &ex) {
				ACS_SHORT_LOG((LM_ERROR, "CORBA exception while destroying ORB: %s", ex._info().c_str()));
				++err;
			}
			return err;
		}

		std::string ip;
		std::string port;

		size_t pos = notifyServiceName.find_first_of(":");
		if(pos != std::string::npos)
		{
			ip = notifyServiceName.substr(0, pos);
			port = notifyServiceName.substr(pos + 1);
		} else {
			// Alarm Notify Service
			if(notifyServiceName == ALARM_NOTIFY_CHANNEL || notifyServiceName == "Alarm")
			{
				notifyServiceName = ALARM_NOTIFY_CHANNEL;
				port = ACSPorts::getNotifyServicePort(baseport, notifyServiceName.c_str());
			
			// Archive Notify Service
			} else if(notifyServiceName == ARCHIVE_NOTIFY_CHANNEL || notifyServiceName == "Archive") {
				notifyServiceName = ARCHIVE_NOTIFY_CHANNEL;
				port = ACSPorts::getNotifyServicePort(baseport, notifyServiceName.c_str());

			// Logging Notify Service
			} else if(notifyServiceName == LOOGING_NOTIFY_CHANNEL || notifyServiceName == "Logging") {
				notifyServiceName = LOOGING_NOTIFY_CHANNEL;
				port = ACSPorts::getNotifyServicePort(baseport, notifyServiceName.c_str());

			// Default Notify Service
			} else if(notifyServiceName == DEFAULT_NOTIFY_CHANNEL || notifyServiceName == "Notify" || notifyServiceName == "") {
				notifyServiceName = DEFAULT_NOTIFY_CHANNEL;
				port = ACSPorts::getNotifyServicePort(baseport, notifyServiceName.c_str());

			// Named Notify Service
			} else {
				size_t posSuff = notifyServiceName.find(DEFAULT_NOTIFY_CHANNEL);
				if(std::string::npos == posSuff) {
					posSuff = notifyServiceName.size();
					notifyServiceName = notifyServiceName + DEFAULT_NOTIFY_CHANNEL;
				}
				std::string nsNamePort = notifyServiceName.substr(0,posSuff);
				port = ACSPorts::getNotifyServicePort(baseport, nsNamePort.c_str());
			}

			// ACSPorts returns port number and a null character in the end so we get only the number
			port = port.substr(0,port.size() - 1);

			ACS_SHORT_LOG((LM_DEBUG, "Looking for service '%s' using port %s from baseport %d",
					notifyServiceName.c_str(), port.c_str(), baseport));

			// Create fitlers to look for the Noitfy Service
			fNotifyService.push_back(new ns::helpers::FilterByNamePort(notifyServiceName, port));

			// Look for the Notify Service
			analyzeNamingEntries(root.in(), 100, fNotifyService);

			// Found one or more Notify Services
			if(fNotifyService.front()->m_entries.size() > 0)
			{
				ip = fNotifyService.front()->m_entries.front().endpoint;	
			}
		}

		// Found one or more Notify Services
		if(false == ip.empty() && false == port.empty())
		{
			// Alarm Noitfy Service will create channels using a specific suffix:
			// NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR + ACS_NC_DOMAIN_ALARMSYSTEM
			if(notifyServiceName == ALARM_NOTIFY_CHANNEL)
			{
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_CHANNEL, ip, port));
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_NC_SUPPORT, ip, port));
			
			// Archive Notify Service has channels using a specific domain and port:
			// NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR + ACS_NC_DOMAIN_ARCHIVING 
			} else if(notifyServiceName == ARCHIVE_NOTIFY_CHANNEL) {
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_CHANNEL, ip, port));
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_NC_SUPPORT, ip, port));

			// Logging Notify Service has channels using a specific domain and port:
			// NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR + ACS_NC_DOMAIN_LOGGING 
			} else if(notifyServiceName == LOOGING_NOTIFY_CHANNEL) {
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_CHANNEL, ip, port));
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_NC_SUPPORT, ip, port));

			// Default Notify Service has channels using a specific port:
			// NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR + NAMESERVICE_BINDING_NC_DOMAIN_DEFAULT 
			} else if(notifyServiceName == DEFAULT_NOTIFY_CHANNEL) {
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_CHANNEL, ip, port));
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_NC_SUPPORT, ip, port));

			// Channels owned by other kinds of Notify Services will be deleted according to the port
			} else {
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_CHANNEL, ip, port));
				fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpointPort(KIND_NC_SUPPORT, ip, port));
			}

//			fNotifyChannels.push_back(new ns::helpers::FilterByKindEndpoint(KIND_CHANNEL, endpoint));
			analyzeNamingEntries(root.in(), 100, fNotifyChannels);

			// Check if it found entries
            bool foundEntries = false;
            for(ns::helpers::PFilterVector::const_iterator it = fNotifyChannels.begin(); 
                it != fNotifyChannels.end() && false == foundEntries; ++it)
            {
                if((*it)->m_entries.size() > 0)
                {
                    foundEntries = true;
                }
            }

			if(true == foundEntries)
			{
                for(ns::helpers::PFilterVector::const_iterator itf = fNotifyChannels.begin(); 
                    itf != fNotifyChannels.end(); ++itf)
                {
                    for(ns::helpers::NSEntryInfoVector::iterator it = (*itf)->m_entries.begin();
                            it != (*itf)->m_entries.end();++it)
                    {
                        if(show == true)
                        {
                            ACS_SHORT_LOG((LM_INFO, "Channel %s to be deleted in the Naming Service", it->name.c_str()));
                        } else {
                            CosNaming::Name name;
                            name.length (1);
                            name[0].id = CORBA::string_dup (it->name.c_str());
                            name[0].kind = CORBA::string_dup (it->kind.c_str());
                            CORBA::Object_var objToDel = root->resolve(name);
                            root->unbind(name);
                            ACS_SHORT_LOG((LM_DEBUG, "Channel %s[%s:%s] deleted in the Naming Service", 
                                    it->name.c_str(),it->endpoint.c_str(),it->port.c_str()));
                        }
                    }
                }
			} else {
				ACS_SHORT_LOG((LM_DEBUG, "Name Service without channel entries in the endpoint %s:%s", ip.c_str(), port.c_str()));
			}
		} else {
			ACS_SHORT_LOG((LM_DEBUG, "Notify Service '%s' not found!", notifyServiceName.c_str()));
		}


	} catch(const CORBA::Exception &ex) {
		ACS_SHORT_LOG((LM_ERROR, "CORBA exception: %s", ex._info().c_str()));
		++err;
	}

	try {
		orb->destroy();
	} catch(const CORBA::Exception &ex) {
		ACS_SHORT_LOG((LM_ERROR, "CORBA exception while destroying ORB: %s", ex._info().c_str()));
		++err;
	}


	// Delete filters used
	for(ns::helpers::PFilterVector::iterator it = fNotifyService.begin();it != fNotifyService.end();++it)
	{
		delete *it;
		*it = NULL;
	}
	for(ns::helpers::PFilterVector::iterator it = fNotifyChannels.begin();it != fNotifyChannels.end();++it)
	{
		delete *it;
		*it = NULL;
	}
	fNotifyService.clear();
	fNotifyChannels.clear();

    return err;
}
