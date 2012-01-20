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
 * "@(#) $Id: cdbDAOProxy.cpp,v 1.9 2012/01/20 23:18:16 tstaig Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2005/09/08  created
 */

#include "cdbDAOProxy.h"

#include <expat.h>
#include <logging.h>

#include <ace/SString.h>
#include <sstream>
#include <ace/Tokenizer_T.h>

#include "cdbErrType.h"
#include <ACSErrTypeCommon.h>
#include <ACSErrTypeCORBA.h>

using namespace cdb;
using namespace std;

// string table
#define MSG_NO_MEMORY   "No memory to create parser object!"
#define MSG_PARSE_ERROR "Parse error at line %d:%s"

//----------------------------------------------------
// CXMLTreeNode
//----------------------------------------------------

CXMLTreeNode::CXMLTreeNode(CXMLTreeNode *pParent)
{
    m_parent = pParent;
}

CXMLTreeNode::~CXMLTreeNode()
{
    MapStringToNode::const_iterator iter = m_subNodesMap.begin();
    while(iter != m_subNodesMap.end()) {
    delete iter->second;
    ++iter;
    }
}

void CXMLTreeNode::getAttributeNames(string &names)
{
    // just to be safe, clear current value
    names.erase(names.begin(), names.end());  // using erase because clear not supported on VxWorks

    // add elements
    MapStringToNode::const_iterator iter = m_subNodesMap.begin();
    while (iter != m_subNodesMap.end())
	{
	names.append(iter->first.c_str()).append(",");
	iter++;
	}

    // and attributes
    MapStringToString::const_iterator fldIter = m_fieldMap.begin();
    while (fldIter != m_fieldMap.end()) {
    // filter out namespace information
    if(ACE_OS::strncmp(fldIter->first.c_str(), "xmlns", 5) != 0) {
    names.append(fldIter->first.c_str()).append(",");
    }
    fldIter++;
    }

    // if not empty, truncate last comma
    if (names.length())
	names.resize(names.length() - 1);
}

//----------------------------------------------------
// DAOProxy (expat XML parser)
//----------------------------------------------------

void DAOProxy::start(void *data, const char *el, const char **attr)
{
    ((DAOProxy*)data)->Start(el, attr);
}

void DAOProxy::end(void *data, const char *el)
{
    ((DAOProxy*)data)->End(el);
}

void DAOProxy::Start(const char *el, const char **attr)
{
    static int elementID = 0;

    if (!m_rootNode)
	{
	m_rootNode = new CXMLTreeNode(0);
	m_rootNode->m_name = el;
	m_currNode = m_rootNode;
	}
    else
	{
	// we have to add this node
	const char* elementName = el;
	CXMLTreeNode* pNode = new CXMLTreeNode(m_currNode);

	// do we have map? check
	if (ACE_OS::strcmp(el, "_") == 0 && attr[0])
	    {
	    // take first element value as new name (fall-off)
	    elementName = attr[1];
	    // check for Name atribute and if found, take its value as name
	    for (int i = 0; attr[i]; i+=2)
		if (ACE_OS::strcmp(attr[i], "Name") == 0)
		    elementName = attr[i+1];
	    }

	// check if node with the name exists, prefix it if it does
	CXMLTreeNode::MapStringToNode::const_iterator iter = m_currNode->m_subNodesMap.find(elementName);
	if(iter != m_currNode->m_subNodesMap.end())
	    {
	    char newName[100];
	    elementID++;
	    ACE_OS::snprintf(newName, 100, "%s%d", elementName, elementID);
	    pNode->m_name = newName;
	    }
	else
	    {
	    pNode->m_name = elementName;
	    }

	// add to map
	m_currNode->m_subNodesMap[pNode->m_name.c_str()] = pNode;
	m_currNode = pNode;
	}

    // do we have array? check
    if(!m_inArray && ACE_OS::strcasecmp(el, "cdb:_") == 0)
	{
	m_inArray = true;
	m_arrayName = m_currNode->m_parent->m_name;
	}

    // add attributes (name, value pairs)
    for (int i = 0; attr[i]; i+=2)
	{
	if (m_inArray)
	    m_arrayContent.append(attr[i+1]).append(",");
	else
	    m_currNode->m_fieldMap[attr[i]] = attr[i+1];
	}

}

void DAOProxy::End(const char *el)
{
    // return one level up
    m_currNode = m_currNode->m_parent;

    // do we ended array
    if (m_inArray && ACE_OS::strcmp(m_arrayName.c_str(), el) == 0)
	{
	// if not empty, truncate last comma
	if (m_arrayContent.length())
	    m_arrayContent.resize(m_arrayContent.length() - 1);

	m_currNode->m_fieldMap[m_arrayName] = m_arrayContent;

	// clear temp vars
	m_inArray = false;
	m_arrayName.erase(m_arrayName.begin(), m_arrayName.end());  // using erase because clear not supported on VxWorks
	m_arrayContent.erase(m_arrayContent.begin(), m_arrayContent.end());  // using erase because clear not supported on VxWorks
	}
}

//----------------------------------------------------
// DAOProxy
//----------------------------------------------------

DAOProxy::DAOProxy(const char * nodeName, const char* xml) :
    m_nodeName(nodeName), m_destroyed(false), m_inArray(false), m_rootNode(0), m_currNode(0), m_dao(CDB::DAO::_nil())
{
    ACS_TRACE("cdb::DAOProxy::DAOProxy");

    // parse XML

    XML_Parser p = XML_ParserCreate(0);
    if(!p)
	{
	m_errorMessage = MSG_NO_MEMORY;
	throw ACSErrTypeCommon::MemoryFaultExImpl (__FILE__,__LINE__,"cdb::DAOProxy::DAOProxy");
	}

    XML_SetUserData(p, this);
    XML_SetElementHandler(p, start, end);

    if (!XML_Parse(p, xml, ACE_OS::strlen(xml), 1))
	{
	char errorMessage[512];
	ACE_OS::snprintf(errorMessage, 512, MSG_PARSE_ERROR,
			 XML_GetCurrentLineNumber(p),
			 XML_ErrorString(XML_GetErrorCode(p)));
	m_errorMessage = errorMessage;

	XML_ParserFree(p);

	throw cdbErrType::CDBXMLErrorExImpl (__FILE__,__LINE__,"cdb::DAOProxy::DAOProxy");
	}

    XML_ParserFree(p);
}

DAOProxy::DAOProxy(const char * nodeName, CDB::DAO_ptr dao) :
    m_nodeName(nodeName), m_destroyed(false), m_inArray(false), m_rootNode(0), m_currNode(0), m_dao(CDB::DAO::_duplicate(dao))
{
    ACS_TRACE("cdb::DAOProxy::DAOProxy");

    // initialization status
    if (m_dao.ptr() == CDB::DAO::_nil())
	throw ACSErrTypeCORBA::FailedToResolveServiceExImpl (__FILE__,__LINE__,"cdb::DAOProxy::DAOProxy");
}


DAOProxy::~DAOProxy()
{
    ACS_TRACE("cdb::DAOProxy::~DAOProxy");

    try
	{
	if (!m_destroyed)
	    destroy();
	}
    catch(...)
	{
	// noop
	}

    if (m_rootNode)
	{
	delete m_rootNode;
	m_rootNode = 0;
	}
}



void DAOProxy::get_field(const char* name, string &value)
{
    //ACS_TRACE("cdb::DAOProxy::get_field");

    CXMLTreeNode* pNode = m_rootNode;
    if (ACE_OS::strlen(name) == 0 || ACE_OS::strcmp(pNode->m_name.c_str(), name) == 0)
	{
	// return attributes
	pNode->getAttributeNames(value);
	return;
	}

    // tokenizer changes buffer, so copy is needed
    char* namecopy = ACE_OS::strdup(name);
    ACE_Tokenizer tokenizer(namecopy);
    tokenizer.delimiter_replace('/', 0);

    string fieldName = tokenizer.next();
    for (char* token = tokenizer.next(); token; token = tokenizer.next())
	{
	CXMLTreeNode::MapStringToNode::const_iterator iter = pNode->m_subNodesMap.find(fieldName);
	if(iter == pNode->m_subNodesMap.end())
	    {
	    // not found, add one level of hierarchy
	    fieldName.append("/").append(token);
	    }
	else
	    {
	    // found, transverse
	    pNode = iter->second;
	    fieldName = token;
	    }

	}

    ACE_OS::free(namecopy);

    // backward compatibility
    if (ACE_OS::strcmp(fieldName.c_str(), "_characteristics") == 0)
	{
	pNode->getAttributeNames(value);
	return;
	}
    else
	{
	CXMLTreeNode::MapStringToString::const_iterator iter = pNode->m_fieldMap.find(fieldName);
	if(iter != pNode->m_fieldMap.end())
	    {
	    // found
	    value = iter->second;
	    return;
	    }
	else
	    {
	    // we should try to get it as node
	    CXMLTreeNode::MapStringToNode::const_iterator iter = pNode->m_subNodesMap.find(fieldName);
	    if(iter != pNode->m_subNodesMap.end())
		{
		iter->second->getAttributeNames(value);
		return;
		}
	    else
		{
		ACS_LOG(LM_RUNTIME_CONTEXT, "cdb::DAOProxy::get_field",
			(LM_WARNING, "Field %s/%s does not exist.", m_nodeName.c_str(), name));
		//throw CDB::FieldDoesNotExist();
		throw cdbErrType::CDBFieldDoesNotExistExImpl (
			__FILE__, __LINE__,
			"DAOProxy::get_field");
		}
	    }
	}
}


bool DAOProxy::split(const string& str, VectorString& array)
{
    //ACS_TRACE("cdb::DAOProxy::split");

    // The string that will be added to the list next.
    string strCur; 
    // Tells us what kind of quote we are in.
    bool bQuote = 0;

    unsigned int iter = 0;
    unsigned int len = str.length();

    array.clear();

    while(iter < len)
	{
	// We got to a whitespace and we are not in a quote: push the currently
	// build substring at the end of the array.
	if(!bQuote && str[iter] == ',')
	    {
	    if(strCur.length()!=0)
		{
		array.push_back(strCur);
		strCur.erase(strCur.begin(), strCur.end());  // using erase because clear not supported on VxWorks
		}
	    }
	// Escape sequence.
	else if(str[iter] == '\\')      
	    {
	    ++iter;
	    // Whoops, escape ended before the new line.
	    if(iter == len) 
		{
		return false;
		}
	    switch(str[iter])
		{
		case 'n':
		    strCur += '\n';
		    break;
		case 'r':
		    strCur += '\r';
		    break;
		case ',':
		case '\\':
		case '\'':
		case '"':
		    // Treat next character verbatim, regardless what it may be.
		    strCur += str[iter];
		    break;
		default:
		    // An unrecognized escape!
		    return false;
		}
	    }
	// The quote ended.
	else if(bQuote && str[iter] == '"')
	    {
	    // Indicate that we are in the quote no longer.
	    bQuote = 0;
	    array.push_back(strCur);
	    strCur.erase(strCur.begin(), strCur.end());  // using erase because clear not supported on VxWorks
	    }
	// The quote begun.
	else if(str[iter] == '"')
	    {
	    if(strCur.length()!=0)
		{
		array.push_back(strCur);
		strCur.erase(strCur.begin(), strCur.end()); // using erase because clear not supported on VxWorks 
		}
	    bQuote = 1;
	    }
	else
	    {
	    // A regular character.
	    strCur += str[iter];
	    }
	++iter;
	}

    // Push the last string to the end of the array.
    if(strCur.length()!=0)
	{
	array.push_back(strCur);
	}

    return true;
}


CORBA::Long DAOProxy::get_long (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOProxy::get_long");

    // check remote mode
    if (m_dao.ptr() != CDB::DAO::_nil()) 
	return m_dao->get_long(propertyName);

    string value;
    try{
    get_field(propertyName, value);
    }catch(cdbErrType::CDBFieldDoesNotExistExImpl ex){
	throw ex.getCDBFieldDoesNotExistEx();
    }
    std::istringstream is(value.c_str());
    CORBA::Long val;
    (istream&) is >> val;
    if (!is)
	throw cdbErrType::WrongCDBDataTypeExImpl(
		__FILE__, __LINE__,
		"cdb::DAOProxy::get_long" ).getWrongCDBDataTypeEx();
	//throw CDB::WrongDataType();
    return val;
}

CORBA::Double DAOProxy::get_double (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOProxy::get_double");

    // check remote mode
    if (m_dao.ptr() != CDB::DAO::_nil()) 
	return m_dao->get_double(propertyName);

    string value;
    try{
    get_field(propertyName, value);
    }catch(cdbErrType::CDBFieldDoesNotExistExImpl ex){
	throw ex.getCDBFieldDoesNotExistEx();
    }
    std::istringstream is(value.c_str());
    CORBA::Double val;
    (istream&) is >> val;
    if (!is)
	throw cdbErrType::WrongCDBDataTypeExImpl(
		__FILE__, __LINE__,
		"cdb::DAOProxy::get_long" ).getWrongCDBDataTypeEx();
	//throw CDB::WrongDataType();
    return val;
}

char * DAOProxy::get_string (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOProxy::get_string");

    // check remote mode
    if (m_dao.ptr() != CDB::DAO::_nil()) 
	return m_dao->get_string(propertyName);

    return get_field_data(propertyName);
}

char * DAOProxy::get_field_data (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOProxy::get_field_data");

    // check remote mode
    if (m_dao.ptr() != CDB::DAO::_nil()) 
	return m_dao->get_field_data(propertyName);

    string value;
    try{
    get_field(propertyName, value);
    }catch(cdbErrType::CDBFieldDoesNotExistExImpl ex){
	throw ex.getCDBFieldDoesNotExistEx();
    }
    return CORBA::string_dup (value.c_str());
}

CDB::stringSeq* DAOProxy::get_string_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOProxy::get_string_seq");

    // check remote mode
    if (m_dao.ptr() != CDB::DAO::_nil()) 
	return m_dao->get_string_seq(propertyName);

    string value;
    try{
    get_field(propertyName, value);
    }catch(cdbErrType::CDBFieldDoesNotExistExImpl ex){
	throw ex.getCDBFieldDoesNotExistEx();
    }

    VectorString array;
    split(value.c_str(), array);

    CDB::stringSeq_var seq = new CDB::stringSeq();
    seq->length(array.size());

    CORBA::ULong i = 0;
    for (VectorString::const_iterator iter = array.begin();
	 iter != array.end(); iter++)
	{
	seq[i++] = CORBA::string_dup(iter->c_str());
	}

    return seq._retn();
}

CDB::longSeq * DAOProxy::get_long_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOProxy::get_long_seq");

    // check remote mode
    if (m_dao.ptr() != CDB::DAO::_nil()) 
	return m_dao->get_long_seq(propertyName);

    string value;
    try{
    get_field(propertyName, value);
    }catch(cdbErrType::CDBFieldDoesNotExistExImpl ex){
	throw ex.getCDBFieldDoesNotExistEx();
    }

    VectorString array;
    split(value.c_str(), array);

    CDB::longSeq_var seq = new CDB::longSeq();
    seq->length(array.size());

    CORBA::ULong i = 0;
    for (VectorString::const_iterator iter = array.begin();
	 iter != array.end(); iter++)
	{
	std::istringstream is(iter->c_str());
	CORBA::Long val;
	(istream&) is >> val;
	if (!is)
		throw cdbErrType::WrongCDBDataTypeExImpl(
		__FILE__, __LINE__,
		"cdb::DAOProxy::get_long" ).getWrongCDBDataTypeEx();
	//    throw CDB::WrongDataType();
	seq[i++] = val;
	}

    return seq._retn();
}


CDB::doubleSeq * DAOProxy::get_double_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOProxy::get_double_seq");

    // check remote mode
    if (m_dao.ptr() != CDB::DAO::_nil()) 
	return m_dao->get_double_seq(propertyName);

    string value;
    try{
    get_field(propertyName, value);
    }catch(cdbErrType::CDBFieldDoesNotExistExImpl ex){
	throw ex.getCDBFieldDoesNotExistEx();
    }

    VectorString array;
    split(value.c_str(), array);

    CDB::doubleSeq_var seq = new CDB::doubleSeq();
    seq->length(array.size());

    CORBA::ULong i = 0;
    for (VectorString::const_iterator iter = array.begin();
	 iter != array.end(); iter++)
	{
	std::istringstream is(iter->c_str());
	CORBA::Double val;
	(istream&) is >> val;
	if (!is)
	    throw cdbErrType::WrongCDBDataTypeExImpl(
		__FILE__, __LINE__,
		"cdb::DAOProxy::get_long" ).getWrongCDBDataTypeEx();
	seq[i++] = val;
	}

    return seq._retn();
}

void DAOProxy::destroy ()
{
    //ACS_TRACE("cdb::DAOProxy::destroy");

    if (m_destroyed)
	return;
    
    m_destroyed = true;
    
    // check remote mode
    if (m_dao.ptr() != CDB::DAO::_nil()) 
	return m_dao->destroy();

    // noop for local implementation
}

// -------------------------------------------------------
/*___oOo___*/

