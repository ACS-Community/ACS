/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2006 
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
* "@(#) $Id: acsstartupIrFeed.cpp,v 1.2 2006/05/05 21:04:50 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2006-03-24  created
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static char *rcsId="@(#) $Id: acsstartupIrFeed.cpp,v 1.2 2006/05/05 21:04:50 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <iostream>
#include <list>
#include <acsutilPorts.h>
#include <dirent.h>
#include <fstream>


using std::string;
using std::list;
using std::cout;

/**
 * Helper function adds all IDL files found within dirName
 * to files list.
 */
void addIdlFiles(std::string dirName,
		 list<string> & files)
{
    //open the directory
    DIR* dir = opendir(dirName.c_str());

    //sanity check
    if (dir==0)
	{
	return;
	}

    //traverse the directory looking for files that end in ".idl"
    struct dirent * file = readdir(dir);
    while(file!=0)
	{
	string t_file = file->d_name;

	//good, found a match
	if(t_file.rfind(".idl") != string::npos)
	    {
	    files.push_back(t_file);
	    }
	
	file = readdir(dir);
	}

    closedir(dir);
}

int main(int argc, char *argv[])
{
    list<string> idlFiles;
    
    std::string aceRoot     = getenv("ACE_ROOT");
    std::string idlPath     = getenv("IDL_PATH");
    std::string outFile     = argv[1];
    std::string processId   = argv[2];
    
    std::ofstream file(outFile.c_str());


    ///Split idlPath after the first $ACE_ROOT

    //find first reference to ACE_ROOT
    std::string::size_type aceRootIndex = idlPath.find(aceRoot);

    //chop off everything after and including ACE_ROOT
    idlPath = idlPath.substr(0, aceRootIndex);
    
    //get rid of all the nasty "-I"s
    while(true)
	{
	std::string::size_type includeIndex = idlPath.find("-I");
	//sanity check
	if (includeIndex == string::npos)
	    {
	    break;
	    }

	idlPath.erase(includeIndex, 2);
	}

    //cycle through each area of IDL path
    while(idlPath.length()!=0)
	{
	//find the next whitespace to...
	std::string::size_type wsIndex = idlPath.find(" ");
	//sanity check
	if (wsIndex == string::npos)
	    {
	    break;
	    }

	//...get the next IDL directory
	string idlDir = idlPath.substr(0, wsIndex);
	
	//examine the IDL directory adding all IDLs found
	addIdlFiles(idlDir, idlFiles);

	
	//shrink the idlPath
	idlPath.erase(0, wsIndex + 1);
	}
    
    //remove duplicates
    idlFiles.unique();

    file << "#ifndef acsIrfeedDyn_" << processId <<"_idl" << std::endl;
    file << "#define acsIrfeedDyn_" << processId <<"_idl" << std::endl;
    file << "/*******************************************************************************" << std::endl;
    file << "* ALMA Project" << std::endl;
    file << "*" << std::endl;
    file << "* This file is dynamically created by acsIrfeed to load" << std::endl;
    file << "* in the Orbacus Interface Repository" << std::endl;
    file << "* the complete set of IDL interfaces" << std::endl;
    file << "*" << std::endl;
    file << "*/" << std::endl;
    file << "" << std::endl;
    file << "#define local" << std::endl;
    file << "#include <CosPropertyService.idl>" << std::endl;
    file << "#undef local" << std::endl;
    file << "" << std::endl;
    file << "#include <acserr.idl>" << std::endl;
    file << "" << std::endl;

    while (idlFiles.empty() == false)
	{
	file << "#include <" << idlFiles.front() << ">" << std::endl;
	idlFiles.pop_front();
	}

    file << "#endif" << std::endl;
    
    return 0;

}








