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
* "@(#) $Id: acsstartupIrFeed.cpp,v 1.10 2012/03/29 15:58:28 hsommer Exp $"
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

static char *rcsId="@(#) $Id: acsstartupIrFeed.cpp,v 1.10 2012/03/29 15:58:28 hsommer Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <iostream>
#include <list>
#include <map>
#include <utility>
#include <acsutilPorts.h>
#include <dirent.h>
#include <fstream>


using std::string;
using std::list;
using std::map;
using std::multimap;
using std::pair;
using std::cout;

/**
 * Helper function adds all IDL files found within dirName
 * to files list.
 */
void addIdlFiles(std::string dirName,
		 map<string, string> & files,
		 map<string, int> & seenFiles)
{
    map<string, int>::iterator it;
    pair<map<string, int>::iterator, bool> ret;

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
    int pos = t_file.rfind(".idl");
    if(pos != (signed int)string::npos )
	{
        int endPos = t_file.size() - 4;
	    if( pos == endPos){
	        if (t_file != "ACSIRSentinel.idl") {
		    ret = seenFiles.insert(pair<string, int>(t_file, 0));
	            if (ret.second == true) 
					files.insert(pair<string, string>(t_file, dirName));
		}
        } 
    }
	
	file = readdir(dir);
	}

    closedir(dir);
}

int checkIncludeGuards(string idl, string idlPath, multimap<string, string> & includeGuards)
{
	string line, temp;
	size_t offset;
	if(idl == "ACSIRSentinel.idl")
		return 0;
	ifstream ifs((idlPath+string("/")+idl).c_str());
	if(ifs.fail()) {
		std::cout << "IDL file:\t'" << idl << "' was not found in '" << idlPath << "'" << std::endl;
		return 1;
	}
	while(std::getline(ifs,line)) {
		if((offset = line.find("#ifndef ")) != string::npos) {
			temp = line.substr(offset+8);
			while((offset = temp.find(" ")) != string::npos)
				temp.erase(offset,1);
			while((offset = temp.find("\t")) != string::npos)
				temp.erase(offset,1);
			pair<multimap<string, string>::iterator, multimap<string,string>::iterator> key = includeGuards.equal_range(temp);
			if(key.first != key.second) {
				std::cout << "IDL file:\t'" << idl << "' has include guard '" << key.first->first << "' already used in file(s): " << std::endl;
				for(multimap<string, string>::iterator it = key.first; it != key.second; it++)
					std::cout << "\t\t'" << it->second << "'" << std::endl;
				includeGuards.insert(pair<string, string>(temp, idl));
				return 2;
			}
			includeGuards.insert(pair<string, string>(temp, idl));
			return 0;
		}
	}
	std::cout << "IDL file:\t'" << idl << "' has no include guard" << std::endl;
	return 3;
}

int main(int argc, char *argv[])
{
    map<string, string> idlFiles;
    map<string, int> seenIdlFiles;
    multimap<string, string> idlIncludeGuards;
    
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
    if (argc == 3){
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
        addIdlFiles(idlDir, idlFiles, seenIdlFiles);

        
        //shrink the idlPath
        idlPath.erase(0, wsIndex + 1);
        }
    }
 
    idlFiles.insert(pair<string, string>("ACSIRSentinel.idl","."));

    file << "#ifndef acsIrfeedDyn_" << processId <<"_idl" << std::endl;
    file << "#define acsIrfeedDyn_" << processId <<"_idl" << std::endl;
    file << "/*******************************************************************************" << std::endl;
    file << "* ALMA Project" << std::endl;
    file << "*" << std::endl;
    file << "* This file is dynamically created by acsIrfeed to load" << std::endl;
    file << "* in the TAO (or mico) Interface Repository" << std::endl;
    file << "* the complete set of IDL interfaces" << std::endl;
    file << "*" << std::endl;
    file << "*/" << std::endl;
    file << "" << std::endl;
    file << "#define local" << std::endl;
    file << "#include <CosProperty.idl>" << std::endl;
    file << "#undef local" << std::endl;
    file << "" << std::endl;
    file << "#include <acserr.idl>" << std::endl;
    file << "" << std::endl;

	for(map<string, string>::iterator it = idlFiles.begin(); it != idlFiles.end(); it++) {
		checkIncludeGuards(it->first, it->second, idlIncludeGuards);
		file << "#include <" << it->first << ">" << std::endl;
	}

    for(int i=3 ; i < argc ;  i++)
        file << "#include <" << argv[i] << ">" << std::endl;  

    file << "#endif" << std::endl;
    
    return 0;

}








