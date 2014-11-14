/*
 * CorbaNotifyUtils.cpp
 *
 *  Created on: Oct 1, 2014
 *      Author: almamgr
 */

#include "CorbaNotifyUtils.h"
#include <ace/Log_Msg.h>

#include <vector>

typedef std::vector<std::string> StrVec;

void CorbaNotifyUtils::getORBOptions(const std::string &appName,const std::string &orbOptions,
		int &orbArgc,ACE_TCHAR*** orbArgv)
{
	StrVec ORBOptionsVec;
	size_t ini = orbOptions.find_first_not_of(" \t");
	size_t end;

	while(ini != std::string::npos)
	{
		end = orbOptions.find_first_of(" \t", ini + 1);
		if(end != std::string::npos)
		{
			ORBOptionsVec.push_back(orbOptions.substr(ini, end - ini));
			ini = orbOptions.find_first_not_of(" \t", end);
		} else {
			ORBOptionsVec.push_back(orbOptions.substr(ini));
			ini = std::string::npos;
		}
	}

	int i = 1;
	orbArgc = ORBOptionsVec.size() + 1;
	*orbArgv = new ACE_TCHAR*[orbArgc];
	(*orbArgv)[0] = new ACE_TCHAR[appName.size() + 1];
	strncpy((*orbArgv)[0], appName.c_str(), appName.size());
	(*orbArgv)[0][appName.size()] = '\0';
	for(StrVec::const_iterator it = ORBOptionsVec.begin();
			it != ORBOptionsVec.end(); ++it, ++i)
	{
		(*orbArgv)[i] = new ACE_TCHAR[it->size() + 1];
		strncpy((*orbArgv)[i], it->c_str(), it->size());
		(*orbArgv)[i][it->size()] = '\0';
		ACE_DEBUG((LM_INFO, "%T ORB option: %s\n", (*orbArgv)[i]));
	}
}

void CorbaNotifyUtils::delORBOptions(int &orbArgc,ACE_TCHAR*** orbArgv)
{
	for(int i = 0;i < orbArgc;++i)
	{
		delete (*orbArgv)[i];
	}
	delete [] *orbArgv;
	*orbArgv = NULL;
}

