/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2006 
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
* "@(#) $Id$"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2006-07-12  created 
*/

#include "vltPort.h"

#include <iostream>

#include "ConfigPropertyGetter.h"
#include <acsutilORBHelper.h>
#include <maciHelper.h>
#include <cdb.h>
#include <cdbDALaccess.h>

// The following defines, remeber what we'are parsing
#define NO_TAG		0
#define PROP_TAG	1

typedef struct {
	// The list of the properties
	std::list<Property>* props;
	// The name of the property
	std::string pName;
	// The tag we're parsing (see defines above)
    short actualTag;
} ParserStruct;

ConfigPropertyGetter::ConfigPropertyGetter():
	m_corbaInitialized(false), m_properties(NULL) {
	m_corbaInitialized=initializeCorbaServices();
}

ConfigPropertyGetter::~ConfigPropertyGetter() {
	m_properties->clear();
	m_properties=NULL;
}

bool ConfigPropertyGetter::initializeCorbaServices() {
	std::cout << "ConfigPropertyGetter::initializeCorbaServices()\n";
	
	// Get the ORB    
    CORBA::ORB_ptr orb = ORBHelper::getORB();
    if (CORBA::is_nil(orb)) {
    	std::cout << "Error getting ORB\n";
    	return false;
    }
    
    maci::Manager_ptr manager = maci::MACIHelper::resolveManager(orb, 0, 0, 0, 0);
    if (CORBA::is_nil(manager)) {
    	std::cout << "Error getting Manager\n";
    	return false;
    }
    
    CDB::DAL_var cdbDAL;
	CORBA::ULong status;
	CORBA::Object_var cdb = manager->get_service(0, "CDB", true, status);
	if (CORBA::is_nil(cdb.in()))
	{
		return false;
	}
	
	cdbDAL = CDB::DAL::_narrow(cdb.in());
	
	DALaccess::forceDAL(cdbDAL.in());
	
	// Get the DAO
	m_dao = cdbDAL->get_DAO("Alarms/AlarmSystemConfiguration");
	if (m_dao.size()>0) {
		parseDAO();
	}
	return true;
}

void ConfigPropertyGetter::parseDAO() {
	if (m_dao.size()==0) {
		return;
	}
	XML_Parser p = XML_ParserCreate(NULL);
    if (! p)
        {
        return ;
        }

    //Connect to the parser the handler for the end and the start of a tag
    XML_SetElementHandler(p, start_hndl, end_hndl);

	m_properties = new std::list<Property>();
	ParserStruct commonData;
	commonData.props=m_properties;
	commonData.actualTag=NO_TAG;
	
    // Connect the char handler
    XML_SetCharacterDataHandler(p,char_hndl);

    XML_SetUserData(p,&commonData);

    // We have all the xml in the string so we parse all the document
    // with just one call
    if (XML_Parse(p,m_dao.c_str(),m_dao.size(),TRUE)==0)
        {
		return;
        }
        // Release the memory used by the parser
    XML_ParserFree(p);
    m_properties = commonData.props;
}

std::string ConfigPropertyGetter::getProperty(std::string propName) {
	if (m_properties==NULL || propName.size()==0) {
		return "";
	} 
	return "";
}

void ConfigPropertyGetter::start_hndl(void *data, const XML_Char *el, const XML_Char **attr) {
	ParserStruct* ps = (ParserStruct*)data;
	if (strcmp(el,"configuration-property")==0) {
		ps->actualTag=PROP_TAG;
        ps->pName=(char*)attr[1];
	}
}

void ConfigPropertyGetter::end_hndl(void *data, const XML_Char *el) {
	ParserStruct* ps = (ParserStruct*)data;
	ps->actualTag=NO_TAG;
}

void ConfigPropertyGetter::char_hndl(void *data, const XML_Char *s, int len) {
	ParserStruct* ps = (ParserStruct*)data;
	
	if (ps->actualTag==PROP_TAG) {
		Property p;
		p.key=ps->pName;
		for (int t=0; t<len; t++) {
			p.value+=s[t];
		}
		ps->props->push_back(p);
	}
}

