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
* "@(#) $Id: bulkDataNTConfigurationParser.i,v 1.5 2011/11/02 18:07:21 rtobar Exp $"
*
* who       when        what
* --------  --------    ----------------------------------------------
* rtobar    2011-09-02  created
*/

#include "ACS_BD_Errors.h"

template<class CfgS, class StreamConfigT, class FlowConfigT>
void BulkDataConfigurationParser::populateConfiguration(map<string, CfgS> &configMap) {

	// Create the stream configuration objects from the collected information
	try {
		map<char*, set<char *> >::iterator mit;
		set<char *>::iterator sit;
		for(mit = m_entities.begin(); mit != m_entities.end(); mit++) {

			StreamConfigT streamCfg;

			// Check if we have any profile for this stream of any of its flows
			if( m_profiles.find(mit->first) != m_profiles.end() ) {

				// Check if there is a profile particularly for this stream
				map<string, string> profiles = m_profiles[mit->first];
				if( profiles.find(mit->first) != profiles.end() ) {
					streamCfg.libraryQos    = DYNAMIC_LIBRARY_NAME;
					streamCfg.profileQos    = mit->first;
				}
				else
					printf("Stream '%s' doesn't have it's own QoS setting, will use the default lib/profile\n", mit->first);

				// Regardless, create the str:// URI so the DDS participant factory gets it
				streamCfg.stringProfileQoS = getStrURIforStream(mit->first);

//				printf("Profile %s is: ==== %s ====\n", mit->first, streamCfg.urlProfileQoS.c_str());
			}
			else
				printf("Stream '%s' has no QoS settings, will use the default lib/profile\n", mit->first);

			// Create flow configuration objects for this stream
			map<string, FlowConfigT> flowConfigs;
			for(sit = mit->second.begin(); sit != mit->second.end(); sit++) {

				FlowConfigT flowCfg;

				// Check if we have a profile for this stream/flow combination
				string profileName(mit->first);
				profileName.append("#");
				profileName.append(*sit);
				if( m_profiles.find(mit->first)              != m_profiles.end() &&
				    m_profiles[mit->first].find(profileName) != m_profiles[mit->first].end() ) {
					flowCfg.libraryQos    = DYNAMIC_LIBRARY_NAME;
					flowCfg.profileQos    = profileName;
				}
				else
					printf("Flow '%s' doesn't have it's own QoS setting, will use the default lib/profile\n", profileName.c_str());

				flowConfigs[*sit] = flowCfg;
			}

			CfgS streamConfigStruct;
			streamConfigStruct.streamCfg = streamCfg;
			streamConfigStruct.flowsCfgMap = flowConfigs;
			configMap[mit->first] = streamConfigStruct;
		}
	} catch(StreamCreateProblemExImpl &ex) {
		throw ex;
	} catch(FlowCreateProblemExImpl &ex) {
		throw ex;
	}

}
