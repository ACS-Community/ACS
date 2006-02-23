#ifndef cdbDAOProxy_H_
#define cdbDAOProxy_H_

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
 *
 * "@(#) $Id: cdbDAOProxy.h,v 1.1 2005/09/20 09:12:41 msekoran Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2005/09/08  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>
#include <map>
#include <vector>

#include <cdbDALS.h>

using namespace std;

namespace cdb {

    /**
     * Internal class for XML tree handling.
     */
    class CXMLTreeNode
    {
      public:

	/**
	 * Constrcutor.
	 * @param pParent parent node.
	 */
	CXMLTreeNode( CXMLTreeNode *pParent );

	/**
	 * Destructor.
	 */
	~CXMLTreeNode();

	/**
	 * Get node attributes names (also subnodes names are added).
	 * @param names out parameter.
	 */
	void getAttributeNames(string &names);

	/// XML node name.
	string m_name;

	/// Node parent in the tree.
	CXMLTreeNode* m_parent;

	typedef map<string, CXMLTreeNode*> MapStringToNode;
	/// Children nodes map.
	MapStringToNode	m_subNodesMap;

	typedef map<string, string> MapStringToString;
	/// Node XML fields (attributes) map.
	MapStringToString m_fieldMap;

    };

    /**
     * Internal class for XML tree handling.
     */
    class DAOProxy : public virtual POA_CDB::DAO
    {
      public:

	/**
	 * Constrcutor (remote mode).
	 */
	DAOProxy(const char * nodeName, CDB::DAO_ptr dao);

	/**
	 * Constrcutor (local mode).
	 * @param xml XML represendation (to be parsed).
	 */
	DAOProxy(const char * nodeName, const char* xml);

	/**
	 * Destructor.
	 */
	virtual ~DAOProxy();


	/**
	 * Internal helper method.
	 * @param name attribute name.
	 * @param value out parameter.
	 */
	void get_field(const char* name, string &value)
	    throw (
		CDB::FieldDoesNotExist
		);
      
	typedef vector<string> VectorString;

	/**
	 * Split string str into several substrings, which are separated with
	 * commas. If quotes are used, then substrings are considered to be 
	 * enclosed in them. Quotes can be escaped so that they can be treated
	 * verbatim.
	 */
	static bool split(const string& str, VectorString& array);

	//----------------------------------------------------
	// CORBA interface
	//----------------------------------------------------

	virtual CORBA::Long get_long (const char * propertyName)
	    throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
		);

	virtual CORBA::Double get_double (const char * propertyName)
	    throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
		);

	virtual char * get_string (const char * propertyName )
	    throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
		);

	virtual char * get_field_data (const char * propertyName)
	    throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
		);

	virtual ::CDB::stringSeq * get_string_seq (const char * propertyName)
	    throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
		);

	virtual ::CDB::longSeq * get_long_seq (const char * propertyName)
	    throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
		);

	virtual ::CDB::doubleSeq * get_double_seq (const char * propertyName )
	    throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
		);

	virtual void destroy(
	    )
	    throw (
		CORBA::SystemException
		);

      public:

	/// Node name.
	string m_nodeName;

	/// Error message.
	string m_errorMessage;

      // expat (XML parser) internal operations
      protected:

	void	Start(const char *el, const char **attr);
	void	End(const char *el);

	static void start(void *data, const char *el, const char **attr);
	static void end(void *data, const char *el);

      // implementation
      protected:

	// destruction status flag.
	bool m_destroyed;

	// temporary variables for array handling
	bool m_inArray;
	string m_arrayName;
	string m_arrayContent;

	// XML tree handling
	CXMLTreeNode* m_rootNode;
	CXMLTreeNode* m_currNode;

	// CDB DAO (non-nil if remote)
	CDB::DAO_var m_dao;
    };

} /* namespace cdb */

#endif /* cdbDAOProxy_H_  */


// -------------------------------------------------------
/*___oOo___*/




