/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: cdbDAOImpl.h,v 1.31 2008/09/29 09:51:19 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* dvitas    2002/07/01  created
*/

#ifndef cdbDAOImpl_H_
#define cdbDAOImpl_H_

#include <cdb.h>

/////////////////////////////////////////////////////////////////////////////
// internal class for XML tree handling

class CXMLTreeNode
{
  public:
    CXMLTreeNode( CXMLTreeNode *pParent );
    ~CXMLTreeNode();
    void getAttributeNames( cdb::String &names );

    CXMLTreeNode* m_parent; // parent in access tree
    typedef std::map<cdb::String, CXMLTreeNode*> MapStringToNode;
    //
    MapStringToNode				m_subNodesMap;
    cdb::MapStringToField m_fieldMap;
    cdb::String								m_name;
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
    virtual cdb::Boolean isInitialized() { return m_initialized; }
    
    ///////////////////////////////////////////
    // scalars
    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    */
    virtual CORBA::Long get_long (const char * propertyName );
    
    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    */
    virtual CORBA::Double get_double (const char * propertyName);
    
    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    */
    virtual char * get_string (const char * propertyName );

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
	* @throw cdbErrType::CDBFieldDoesNotExistEx
    */
    virtual char * get_field_data (const char * propertyName );

    ///////////////////////////////////////////
    // sequences
    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
	* @throw cdbErrType::CDBFieldDoesNotExistEx
    */
    virtual ::CDB::stringSeq * get_string_seq (const char * propertyName );

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
	* @throw cdbErrType::CDBFieldDoesNotExistEx
    */
    virtual ::CDB::longSeq * get_long_seq (const char * propertyName );

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
	* @throw cdbErrType::CDBFieldDoesNotExistEx
    */
    virtual ::CDB::doubleSeq * get_double_seq (const char * propertyName );

    cdb::Boolean get_field( const cdb::String &strFieldName, cdb::Field &fld );

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
    cdb::Boolean			m_initialized;

    // temporary variables for array handling 
    int				m_inArray;
    cdb::String		m_arrayName;
    
    cdb::String		m_arrayType;
    cdb::String		m_arrayContent;

    // XTML tree handling
    CXMLTreeNode* m_rootNode;
    CXMLTreeNode* m_currNode;

    int			m_remote; // holding info about current location of this object
    CDB::DAO_var	m_dao;
};

#endif /* DAO_H_  */





