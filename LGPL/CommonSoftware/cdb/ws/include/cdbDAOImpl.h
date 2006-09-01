/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: cdbDAOImpl.h,v 1.27 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* dvitas    2002/07/01  created
*/

#ifndef cdbDAOImpl_H_
#define cdbDAOImpl_H_

#include <cdb.h>
 using namespace cdb;

/////////////////////////////////////////////////////////////////////////////
// internal class for XML tree handling

class CXMLTreeNode
{
public:
	CXMLTreeNode( CXMLTreeNode *pParent );
	~CXMLTreeNode();
        void getAttributeNames( String &names );

	CXMLTreeNode* m_parent; // parent in access tree
	typedef std::map<String, CXMLTreeNode*> MapStringToNode;
	//
	MapStringToNode				m_subNodesMap;
	MapStringToField m_fieldMap;
	String								m_name;
};

/////////////////////////////////////////////////////////////////////////////
//
#ifdef _DAO_SERVANT_BUILD
#include <cdbDALS.h>
class  DAOImpl : public virtual POA_CDB::DAO
#else
#include <cdbDALC.h>
class  DAOImpl
#endif
{
public:
  //Constructor 
  DAOImpl ( const char* curl );
  DAOImpl ( CDB::DAO_ptr  dao );

  //Destructor 
  virtual ~DAOImpl (void);
	virtual Boolean isInitialized() { return m_initialized; }

///////////////////////////////////////////
// scalars
virtual CORBA::Long get_long (
    const char * propertyName
    
  )
  throw (
    CORBA::SystemException,
    CDB::WrongDataType
  );

virtual CORBA::Double get_double (
    const char * propertyName
    
  )
  throw (
    CORBA::SystemException,
    CDB::WrongDataType
  );

virtual char * get_string (
    const char * propertyName
    
  )
  throw (
    CORBA::SystemException,
    CDB::WrongDataType
  );

virtual char * get_field_data (
    const char * propertyName
      
  )
  throw (
    CORBA::SystemException,
    CDB::WrongDataType,
    CDB::FieldDoesNotExist
  );

///////////////////////////////////////////
// sequences
virtual ::CDB::stringSeq * get_string_seq (
	const char * propertyName
	 
	)
	throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
	);

virtual ::CDB::longSeq * get_long_seq (
	const char * propertyName
	  
	)
	throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
	);

virtual ::CDB::doubleSeq * get_double_seq (
	const char * propertyName
	  
	)
	throw (
		CORBA::SystemException,
		CDB::WrongDataType,
		CDB::FieldDoesNotExist
	);

//
	Boolean get_field( const String &strFieldName, Field &fld );

public:
	ACE_CString	m_name; 
	ACE_CString	m_errorMessage;

// internal operations
protected:
	void	Start(const char *el, const char **attr);
	void	End(const char *el);

	static void start(void *data, const char *el, const char **attr);
	static void end(void *data, const char *el);

// implementation
protected:
	ACE_CString m_xml;
	Boolean			m_initialized;

	// temporary variables for array handling 
	int				m_inArray;
	String		m_arrayName;
	String		m_arrayType;
	String		m_arrayContent;

	// XTML tree handling
	CXMLTreeNode* m_rootNode;
	CXMLTreeNode* m_currNode;

	int						m_remote; // holding info about current location of this object
	CDB::DAO_var	m_dao;
};

#endif /* DAO_H_  */





