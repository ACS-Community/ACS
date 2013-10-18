/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2013-05-22  created 
*/

#include <OS.h>
#include <iostream>
#include "acsDaemonUtils.h"
#include "acsutilTimeStamp.h"


AcsDaemonUtils::AcsDaemonUtils():
	m_logDirectory("~/.acs/commandcenter/")
{
	int ret=initLogDirectory();
	// In case of error ret is not 0
	if (ret!=0) {
		std::cout<<"ERROR initializing the folder to write logs: ";
		switch (ret) {
		case 1: {
			std::cout<<"ACSDATA does not exist or can't write in ACSDATA"<<std::endl;
			break;
		}
		case 2: {
			std::cout<<"error creating ACSDATA/logs"<<std::endl;
			break;
			}
		case 3: {
			std::cout<<"Can't write ACSDATA/logs"<<std::endl;
			break;
			}
		case 4: {
			std::cout<<"error getting the host name for HOST environment variable"<<std::endl;
			break;
			}
		case 5: {
			std::cout<<"Error creating ACSDATA/logs/<HOST>"<<std::endl;
			break;
			}
		case 6: {
			std::cout<<"Can't write ACSDATA/logs/<HOST>"<<std::endl;
			break;
			}
		default: {
			std::cout<<"Unknown error code "<<ret<<std::endl;
			break;
		}
		}
		std::cout<<"AcsDaemonUtils built. Log directory is: "<<m_logDirectory<<std::endl;
	}
}

bool AcsDaemonUtils::makeFolder(std::string path)
{
	if (ACE_OS::mkdir(path.c_str())!=0) {
		return false;
	}
	// Folder created: try to set permissions
	std::string chmod("chmod 775 ");
	chmod=chmod+path;
	if (ACE_OS::system(chmod.c_str())!=0) {
		std::cout<<getTimestamp()<<" WARNING: Error setting permissions of "<<path<<std::endl;
	}
	return true;
}

int AcsDaemonUtils::initLogDirectory()
{
	 //get the directory name to store the logs stdout
	char* acsdata = ACE_OS::getenv("ACSDATA");
	if (ACE_OS::access(acsdata,F_OK|W_OK)!=0) {
		// ACSDATA does not exist or can't write!
		return 1;
	}

	std::string logDirectory = std::string(acsdata) + std::string("/logs/");

	// Check if ACSDATA/logs exists
	if (ACE_OS::access(logDirectory.c_str(),F_OK)!=0) {
		// try to create the folder
		if (!makeFolder(logDirectory.c_str())) {
			return 2;
		}
	}
	// Check if ACSDATA/logs is writable
	if (ACE_OS::access(logDirectory.c_str(),W_OK)!=0) {
		return 3;
	}

	// Set the logDirectory to ACSDATA/logs/<HOST> from HOST env. var.
	std::string hostFolder = logDirectory;
	char * host = ACE_OS::getenv("HOST");
	if(host != NULL)
	{
		hostFolder = logDirectory + std::string(host) + std::string("/");
	} else {
		return 4; // HOST env. var. not set
	}

	// Check if ACSDATA/logs/<HOST> exists
	if (ACE_OS::access(hostFolder.c_str(),F_OK)!=0) {
		// try to create the folder
		if (!makeFolder(hostFolder.c_str())!=0) {
			return 5;
		}
	}
	// Check if ACSDATA/logs/<HOST> is writable
	if (ACE_OS::access(hostFolder.c_str(),W_OK)!=0) {
		return 6;
	}

	// Everything went well: use ACSDATA/logs/<HOST> as a base for logging
	m_logDirectory=hostFolder;
	return 0;

}

std::string AcsDaemonUtils::getLogDirectoryForContainer(std::string containerName)
{
	if (containerName.rfind('/')==std::string::npos)
	{
		// Non hierarchical container: the logs go in the standard log folder
		return getLogDirectory();
	}

	// For hierarchical container like CONTROL/ACC/testContainer the logs go in
	// $ACSDATA/logs/<HOST>/CONTROL/ACC so we have to remove the container name
	// from the passed parameter containerName
	std::string name=containerName;
	if (containerName[containerName.length()-1]=='/') {
		// Remove the trailing '/'
		name=containerName.substr(0,containerName.length()-1);
	}
	std::string contName=name.substr(0,name.rfind('/')+1);

	// Does the folder already exist?
	// If it is the case then the method returns immediately
	std::string temp(getLogDirectory()+contName);
	if (ACE_OS::access(temp.c_str(),F_OK)==0) {
		return temp;
	}

	// containerName can be hierarchical i.e. with multiple "/"
	// In that case, we must create all the intermediate folders, one for each "/"
	// in containerName
	temp=getLogDirectory();
	for (unsigned int i=0; i<contName.length(); ++i)
	{
		temp+=contName[i];
		if (contName[i]=='/') {
			std::cout<<"Checking folder "<<temp<<std::endl;
			// A new folder must be created if not exists
			if (ACE_OS::access(temp.c_str(),F_OK)!=0) {
				std::cout<<"\tCreating folder "<<temp<<std::endl;
				if (!makeFolder(temp)) {
					std::cout<<getTimestamp()<<" ERROR building log folder for container "<<containerName;
					std::cout<<" falling back to "<<getLogDirectory()<<std::endl;
					return getLogDirectory();
				}
			}
		}
	}
	return temp;
}

std::string AcsDaemonUtils::getTimestamp()
{
	std::string timeStamp(getStringifiedTimeStamp().c_str());

	if( timeStamp.find(":") != std::string::npos)
		timeStamp.replace(timeStamp.find(":"),1,".");
	if( timeStamp.find(":") != std::string::npos )
		timeStamp.replace(timeStamp.find(":"),1,".");
	if( timeStamp.find("T") != std::string::npos)
		timeStamp.replace(timeStamp.find("T"),1,"_");

	return timeStamp;
}

std::string AcsDaemonUtils::getSimpleContainerName(std::string containerName) {
	if (containerName.rfind('/')==std::string::npos) {
		// Not hierarchical container name
		return containerName;
	}
	// The container is hierarchical
	return containerName.substr(containerName.rfind('/')+1);
}

/*___oOo___*/
