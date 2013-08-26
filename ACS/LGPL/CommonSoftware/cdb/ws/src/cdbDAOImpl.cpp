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
* "@(#) $Id: cdbDAOImpl.cpp,v 1.41 2008/09/29 09:51:19 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* dvitas    2002/07/01  created
*/

#include <cdbDAOImpl.h>
#include <expat.h>
#include <logging.h>
#include <cdbErrType.h>
 
 using namespace cdbErrType;
 using namespace cdb;

/////////////////////////////////////////////////////////////////////////////
// CXMLTreeNode

// string table
#define MSG_NO_MEMORY   "No memory to create parser object!"
#define MSG_PARSE_ERROR "Parse error at line %d:%s"

CXMLTreeNode::CXMLTreeNode( CXMLTreeNode *pParent )
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

void CXMLTreeNode::getAttributeNames( String &names )
{
        names.clear();
	// add elements
        MapStringToNode::const_iterator iter = m_subNodesMap.begin();
        while(iter != m_subNodesMap.end()) {
                names = names + iter->first.c_str();
                names = names + ",";
                ++iter;
        }
	// and atributes
	MapStringToField::const_iterator fldIter = m_fieldMap.begin();
	while(fldIter != m_fieldMap.end()) {
		if(strncmp(fldIter->first.c_str(),"xmlns",5) == 0) {
			++fldIter;
			continue; // namespace information is not needed here
		}
		names = names + fldIter->first.c_str();
		names = names + ",";
		++fldIter;
	}
}

/////////////////////////////////////////////////////////////////////////////
// DAOImpl

void DAOImpl::start(void *data, const char *el, const char **attr)
{
	((DAOImpl*)data)->Start(el, attr);
}

void DAOImpl::end(void *data, const char *el)
{
	((DAOImpl*)data)->End(el);
}

void DAOImpl::Start(const char *el, const char **attr)
{
	//if( ACE_OS::strcmp(el, "Entity") == 0 ) {
	//	return;
	//}
        static int elementID = 0;
        String elementName;
	if( m_rootNode == NULL ) {
		m_rootNode = new CXMLTreeNode( NULL );
		m_rootNode->m_name = el;
		m_currNode = m_rootNode;
	}
	else { // we have to add this node
                elementName = el;
		int nameLen = ACE_OS::strlen(el);
                CXMLTreeNode* pNode = new CXMLTreeNode( m_currNode );
                if( (ACE_OS::strcasecmp(el, "_") == 0 || (nameLen > 2 && el[nameLen-2]==':' && el[nameLen-1]=='_')) && attr[0] != NULL ) 
                {
                        elementName = attr[1];
                }
                CXMLTreeNode::MapStringToNode::const_iterator iter = m_currNode->m_subNodesMap.find(elementName);
                if( iter != m_currNode->m_subNodesMap.end() ) 
                {
                        char newName[100];
                        elementID++;
                        ACE_OS::sprintf(newName,"%s%d", elementName.c_str(), elementID);
                        pNode->m_name = newName;
                }
                else 
                {
                        pNode->m_name = elementName;
                }
                m_currNode->m_subNodesMap[pNode->m_name.c_str()] = pNode;
                m_currNode = pNode;
	}

	int i;
	String fieldString, type, value;
	if( !m_inArray && ACE_OS::strcasecmp(el, "cdb:_") == 0 ) 
	{
		m_inArray = 1;
		m_arrayName = m_currNode->m_parent->m_name;
		m_arrayType = "StringArray";
	}

	for (i = 0; attr[i]; i += 2) {
		//printf(" %s %s='%s'\n", el, attr[i], attr[i + 1]);
	  if( m_inArray ) 
	  {
	 	 m_arrayContent = m_arrayContent + attr[i+1] + ",";
		}
    else {
			type = attr[i];
			value = attr[i+1];
			fieldString = "<String>" + value;
			  
			Field fld;
			fld.FromString(fieldString);
			m_currNode->m_fieldMap[type] = fld;
    }

	}
	// do we have an array
	if( !m_inArray && type.find("Array") != String::npos ) {
		m_inArray = 1;
		m_arrayName = el;
		m_arrayType = type;
		return;
	}
}

void DAOImpl::End(const char *el)
{
	m_currNode = m_currNode->m_parent; // others are siblings
	// do we ended array
	if(m_inArray && ACE_OS::strcmp(m_arrayName.c_str(), el) == 0 ) {
		
		m_arrayContent = "<" + m_arrayType + ">" + m_arrayContent;
		Field fld;
		fld.FromString(m_arrayContent);
		m_currNode->m_fieldMap[m_arrayName] = fld;

		// clear temp vars
		m_inArray = 0;
		m_arrayName.clear(1);
		m_arrayType.clear(1);
		m_arrayContent.clear(1);
	}
}

// Implementation skeleton constructor
DAOImpl::DAOImpl( const char* curl ) : m_xml( curl ), m_initialized(FALSE), m_inArray(0), m_remote(0)
{
	m_rootNode = NULL;
	m_currNode = NULL;

	XML_Parser p = XML_ParserCreate(NULL);
  if( !p ) {
    m_errorMessage = MSG_NO_MEMORY;
    return;
  }
	
	XML_SetUserData(p, this);
	XML_SetElementHandler(p, start, end);
	
	if (! XML_Parse(p, m_xml.c_str(), m_xml.length(), 1)) {
		char errorMessage[512];
		ACE_OS::sprintf(errorMessage, MSG_PARSE_ERROR,
			XML_GetCurrentLineNumber(p),
			XML_ErrorString(XML_GetErrorCode(p)));
		m_errorMessage = errorMessage;
		return;
	}
	XML_ParserFree( p );

	// initialization successful
	m_initialized = TRUE;
}

DAOImpl::DAOImpl( CDB::DAO_ptr  dao ) : m_initialized(TRUE), m_inArray(0), m_remote(1)
{ 
	m_rootNode = NULL;
	m_currNode = NULL;
	m_dao = CDB::DAO::_duplicate(dao);
}
  
// Implementation skeleton destructor
DAOImpl::~DAOImpl (void)
{
	if( m_rootNode ) {
		delete m_rootNode;
		m_rootNode = NULL;
	}
}

Boolean DAOImpl::get_field( const String &strFieldName, Field &fld )
{
	if( m_remote ) {
		
		try{
			CORBA::String_var filedData = m_dao->get_field_data( strFieldName.c_str() );
			
      			if(!fld.SetType(Field::tyString, 0)) return 0;
      			return fld.SetString(filedData.in());
		}catch(CORBA::Exception &ex){
			// be silent here
		}
		return false;
	}

	int pos1;
	String fieldName = strFieldName;
	String restOfName;
	CXMLTreeNode::MapStringToNode::iterator subIter;
	CXMLTreeNode* pNode = m_rootNode;
	if( strFieldName.length() == 0 || ACE_OS::strcmp(pNode->m_name.c_str(), strFieldName.c_str()) == 0 ) {
		String filedData;
		pNode->getAttributeNames( filedData );
		fld.SetType(Field::tyString, 0);
		fld.SetString(filedData.c_str());
		return TRUE;
	}
	pos1 = fieldName.find("/");
	while( pos1 != String::npos ) {
		restOfName = fieldName.substr(pos1+1); 
		String subName = fieldName.substr(0, pos1);
		subIter = pNode->m_subNodesMap.find( subName );
		if( subIter == pNode->m_subNodesMap.end() )
			break; // we do not find it but fall down to check in plain entries
		pNode = subIter->second;
		fieldName = restOfName;
		pos1 = fieldName.find("/");
	}
	if( ACE_OS::strcmp(fieldName.c_str(), "_characteristics") == 0 ) {
		ACE_CString all_names;
		MapStringToField::const_iterator iter = pNode->m_fieldMap.begin();
		for(;iter!=pNode->m_fieldMap.end(); iter++ ) {
			all_names = all_names + iter->first + ",";
		}
		fld.SetType( Field::tyString, 0 );
		fld.SetString( all_names.c_str() );
		return TRUE;
	}
	
	MapStringToField::iterator iter = pNode->m_fieldMap.find(fieldName);
	if(iter == pNode->m_fieldMap.end()) {
                subIter = pNode->m_subNodesMap.find( fieldName );
                if( subIter != pNode->m_subNodesMap.end() ) {
                        String filedData;
                        subIter->second->getAttributeNames( filedData );
                        fld.SetType(Field::tyString, 0);
                        fld.SetString(filedData.c_str());
                        return TRUE;
                }
		return FALSE;
	}
	fld = iter->second;
	/*	
	// DEBUG
	String fieldData;
	fld.ToString(fieldData);
	ACS_LOG(0, "cdb::DAOImpl", (LM_INFO, "DAO:'%s' returned '%s'=%s", m_name.c_str(), strFieldName.c_str(), fieldData.c_str()));
	//
	*/
	return TRUE;
}


CORBA::Long DAOImpl::get_long (
    const char * propertyName
    
  )
{
	if( m_remote )
	{
		return m_dao->get_long( propertyName );
	}
	Field fld;
	if( !get_field(propertyName, fld) )
	{
		throw CDBFieldDoesNotExistExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getCDBFieldDoesNotExistEx();
	}
	if(fld.GetType() != Field::tyLong)
	{
		throw WrongCDBDataTypeExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getWrongCDBDataTypeEx();
	}
	Long retVal;
	fld.GetLong(retVal);
	return retVal;
}
  
CORBA::Double DAOImpl::get_double (
    const char * propertyName
    
  )
{
	if( m_remote )
	{
		return m_dao->get_double( propertyName );
	}
	Field fld;
	if( !get_field(propertyName, fld) )
	{
		throw CDBFieldDoesNotExistExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getCDBFieldDoesNotExistEx();
	}
	if(fld.GetType() != Field::tyDouble)
	{
		throw WrongCDBDataTypeExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getWrongCDBDataTypeEx();
	}
	Double retVal;
	fld.GetDouble(retVal);
	return retVal;
}
  
char * DAOImpl::get_string (
    const char * propertyName
    
  )
  {
  	if( m_remote )
	{
		return m_dao->get_string( propertyName );
	}
	Field fld;
	if( !get_field(propertyName, fld) )
	{
		throw CDBFieldDoesNotExistExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getCDBFieldDoesNotExistEx();
		//ACE_THROW_RETURN( CDB::FieldDoesNotExist(), 0 );
	}
	if(fld.GetType() != Field::tyString)
	{
		throw WrongCDBDataTypeExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getWrongCDBDataTypeEx();
		//ACE_THROW_RETURN( CDB::WrongDataType(), 0 );
	}
	cdb::String str;
	fld.GetString(str);
	return const_cast<char*>(str.c_str());
  }

char * DAOImpl::get_field_data (
    const char * propertyName
    
  )
{
	Field fld;
	if( !get_field(propertyName, fld) )
	{
		throw CDBFieldDoesNotExistExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getCDBFieldDoesNotExistEx();
	}
	String fieldData;
	fld.ToString(fieldData);
	return CORBA::string_dup (fieldData.c_str());
}
  
CDB::stringSeq* DAOImpl::get_string_seq (
    const char * propertyName
    
  )
{
	Field fld;
	if( !get_field(propertyName, fld) )
	{
		throw CDBFieldDoesNotExistExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getCDBFieldDoesNotExistEx();
	}
    
	if(fld.GetType() != Field::tyStringArray)
	{
		throw WrongCDBDataTypeExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getWrongCDBDataTypeEx();
	}

	// create return value
	CDB::stringSeq_var retSeq;
	ACE_NEW_THROW_EX (retSeq, CDB::stringSeq, CORBA::NO_MEMORY ());
	ACE_CHECK_RETURN (0);

	StringArray * ary = fld.GetStringArray();
	retSeq->length( ary->size() );
	CORBA::ULong n=0;
	for(StringArray::const_iterator aiter = ary->begin(); aiter != ary->end(); ++aiter) 
	{
		retSeq[n++] = CORBA::string_dup( aiter->c_str() );
	}
	
	return retSeq._retn();
	
}

::CDB::longSeq * DAOImpl::get_long_seq (
	const char * propertyName
	
	)
{
	Field fld;
    if( !get_field(propertyName, fld) )
    {
		throw CDBFieldDoesNotExistExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getCDBFieldDoesNotExistEx();
   }
  // I don't know why but it returns always an array of string
  if(fld.GetType() != Field::tyStringArray)
  {
		throw WrongCDBDataTypeExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getWrongCDBDataTypeEx();
   }
 // create return value
 CDB::longSeq_var retSeq;
 try
     {
     retSeq = new CDB::longSeq();
     }
 catch(...)
     {
     throw CORBA::NO_MEMORY ();
     }//try-catch

 StringArray * ary = fld.GetStringArray();
  retSeq->length( ary->size() );
 CORBA::ULong n=0;
  for(StringArray::const_iterator aiter = ary->begin(); aiter != ary->end(); ++aiter) 
   {
      //retSeq[n++] = *aiter; //CORBA::long_dup( *aiter); //->c_str() );
      long lng;
      int parsed=sscanf(aiter->c_str(),"%ld",&lng);
      if (parsed==1) {
         retSeq[n++]=lng;
      } else {
         // The string doesn't contain a long: throw an exception
	  throw WrongCDBDataTypeExImpl (
		__FILE__, __LINE__,
		"DAOImpl::get_long").getWrongCDBDataTypeEx();
      }
 }
  
   return retSeq._retn();
}


::CDB::doubleSeq * DAOImpl::get_double_seq (
	const char * propertyName
	
	)
{
	Field fld;
    if( !get_field(propertyName, fld) )
    {
	throw CDBFieldDoesNotExistExImpl (
		__FILE__, __LINE__,
		"DAOImpl::get_long").getCDBFieldDoesNotExistEx();
   }
  // I don't know why but it returns always an array of string
  if(fld.GetType() != Field::tyStringArray)
  {
	throw WrongCDBDataTypeExImpl (
		__FILE__, __LINE__,
		"DAOImpl::get_long").getWrongCDBDataTypeEx();
   }
 // create return value
 CDB::doubleSeq_var retSeq;
 try
     {
     retSeq = new CDB::doubleSeq();
     }
 catch(...)
     {
     throw CORBA::NO_MEMORY ();
     }//try-catch

 StringArray * ary = fld.GetStringArray();
  retSeq->length( ary->size() );
 CORBA::ULong n=0;
  for(StringArray::const_iterator aiter = ary->begin(); aiter != ary->end(); ++aiter) 
   {
      //retSeq[n++] = *aiter; //CORBA::long_dup( *aiter); //->c_str() );
      double dbl;
      int parsed=sscanf(aiter->c_str(),"%lf",&dbl);
      if (parsed==1) {
         retSeq[n++]=dbl;
      } else {
         // The string doesn't contain a long: throw an exception
		throw WrongCDBDataTypeExImpl (
			__FILE__, __LINE__,
			"DAOImpl::get_long").getWrongCDBDataTypeEx();
         
      }
 }
  
   return retSeq._retn();
}


/*___oOo___*/

