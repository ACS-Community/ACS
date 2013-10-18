#ifndef ACSDAEMONS_UTILS_H
#define ACSDAEMONS_UTILS_H
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
* acaproni  2013-05-22  created
*/

/************************************************************************
 * A utility class for logging from the daemons.
 *
 * The log directory defaults to "~/.acs/commandcenter/" but it is redefined
 * if ACSDATA env. variable is defined. In that case the folder name
 * is $ACSDATA/logs/host/.
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>

class AcsDaemonUtils {
private:
	/**
	 * The directory to write the logs.
	 */
	std::string m_logDirectory;

public:

	/**
	 * Constructor
	 */
	AcsDaemonUtils();

	/**
	 * Get the directory to write the log files into.
	 *
	 * @return The returned path is usually $ACSDATA/logs/<HOST>/
	 */
	std::string getLogDirectory() { return m_logDirectory; }

	/**
	 * The folder for the logs of a container is usually $ACSDATA/logs/<HOST>.
	 * If the container is hierarchical i.e it has a name like CONTROL/ACC/containerName
	 * then the logs go in $ACSDATA/logs/<HOST>/CONTROL/ACC/
	 * <P>
	 * If the folder does not exist, this method tries to build the folder
	 * setting the permissions too.
	 *<P>
	 * In case of error building the folder, it returns  m_logDirectory.
	 *
	 * @param containerName The name of the container
	 * @return The returned path is $ACSDATA/logs/<HOST>/containerName
	 *         or $ACSDATA/logs/<HOST>/AAA/BBB/... if the container name is hierarchical
	 */
	std::string getLogDirectoryForContainer(std::string containerName);

	/**
	 * Build a return a well formatted timestamp
	 */
	std::string getTimestamp();

	/**
	 * Return the container name without the hierarchical part, that is:
	 * - the name if the container if it is not hierarchical
	 * - the last word after '\' if it is hierarchical
	 *
	 * @return the name of the container
	 */
	std::string getSimpleContainerName(std::string containerName);

	/**
	 * Check if the process has permission to write in the log folder
	 * where the log folder is returned by getLogDirectory()
	 *
	 * @return 0 means that the folder is writable
	 */
	int checkWritePermission();

	/**
		 * Check if the process has permission to write in the log folder
		 * for the passed container name.
		 * The folder for the logs of container is built by
		 * AcsDaemonUtils#getLogDirectoryForContainer(...)
		 *
		 * @return 0 means that the folder is writable
		 */
		int checkWritePermissionForContainer(std::string containerName);

private:

	/**
	 * Init the log directory, m_logDirectory, from the ACSDATA environment variable, if defined.
	 * <P>
	 * If the folder does not exist, then initLogDirectory tries to create .
	 * <BR>Note that this is the base folder for the logs i.e. ACSDATA/logs/host.
	 *
	 * @return	0 if the folder exists and is writable
	 * 			1 if ACSDATA does not exist or can't write in ACSDATA
	 * 			2 error creating ACSDATA/logs
	 * 			3 Can't write ACSDATA/logs
	 * 			4 error getting the host name for HOST environment variable
	 * 			5 Error creating ACSDATA/logs/<HOST>
	 * 			6 Can't write ACSDATA/logs/<HOST>
	 */
	int initLogDirectory();

	/**
	 * Make the folder with the passed path.
	 *
	 * @param path The path of the folder to create
	 * @return true If the folder has been successfully created
	 *         false otherwise
	 */
	bool makeFolder(std::string path);

	/**
	 * Check if the process has permission to write in the
	 * passed log folder.
	 *
	 * The test is done by creating a temporary file as this should be
	 * the most portable way.
	 */
	int checkWritePermission(std::string folder);

};

#endif /*!ACSDAEMONS_UTILS_H*/
