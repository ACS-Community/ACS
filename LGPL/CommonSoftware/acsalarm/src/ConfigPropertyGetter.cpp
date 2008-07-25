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
#include <cdb.h>
#include <cdbDALaccess.h>
#include <cdbErrType.h>

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

ConfigPropertyGetter::ConfigPropertyGetter(maci::Manager_ptr manager):m_properties(NULL) 
{
	m_dao = getDAO(manager);
	if (m_dao.size()>0) {
		parseDAO();
	}
}

ConfigPropertyGetter::~ConfigPropertyGetter() {
	if (m_properties!=NULL) {
		m_properties->clear();
		delete m_properties;
		m_properties=NULL;
	}
}

std::string ConfigPropertyGetter::getDAO(maci::Manager_ptr manager) {
    CDB::DAL_var cdbDAL;
    CORBA::Object_var cdb;

    try
	{
	cdb = manager->get_service(0, "CDB", true);
	if (CORBA::is_nil(cdb.in()))
	    {
	    throw acsErrTypeAlarmSourceFactory::ErrorGettingDALExImpl(__FILE__,__LINE__,"ConfigPropertyGetter::getDAO");
	    }
	}
    catch(maciErrType::CannotGetComponentEx &ex)
	{
	throw acsErrTypeAlarmSourceFactory::ErrorGettingDALExImpl(ex, __FILE__,__LINE__,"ConfigPropertyGetter::getDAO");
	}
    catch(maciErrType::ComponentNotAlreadyActivatedEx &ex)
	{
	throw acsErrTypeAlarmSourceFactory::ErrorGettingDALExImpl(ex, __FILE__,__LINE__,"ConfigPropertyGetter::getDAO");
	}
	  catch(maciErrType::ComponentConfigurationNotFoundEx &ex)
	{
	throw acsErrTypeAlarmSourceFactory::ErrorGettingDALExImpl(ex, __FILE__,__LINE__,"ConfigPropertyGetter::getDAO");
	}
    catch(CORBA::Exception &ex)
	{
	throw acsErrTypeAlarmSourceFactory::ErrorGettingDALExImpl(__FILE__,__LINE__,"ConfigPropertyGetter::getDAO");
	}
    catch(...)
	{
	throw acsErrTypeAlarmSourceFactory::ErrorGettingDALExImpl(__FILE__,__LINE__,"ConfigPropertyGetter::getDAO");
	}//try-catch
    
    cdbDAL = CDB::DAL::_narrow(cdb.in());
    
    cdb::DALaccess::forceDAL(cdbDAL.in());
    
    // Get the DAO
    try {
        char * retVal = cdbDAL->get_DAO("Alarms/Administrative/AlarmSystemConfiguration");
        std::string retString(retVal);
        CORBA::string_free(retVal);
        return retString;
    } catch (cdbErrType::CDBRecordDoesNotExistEx) {
        return "";
    }
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
	std::list<Property>::iterator iter;
	for (iter=m_properties->begin(); iter!=m_properties->end(); iter++) {
		Property p = (*iter);
		if (p.key==propName) {
			return p.value;
		}
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

