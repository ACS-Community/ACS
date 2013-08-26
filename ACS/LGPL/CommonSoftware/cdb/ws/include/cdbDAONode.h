#ifndef cdbDAONode_H_
#define cdbDAONode_H_

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
 * "@(#) $Id: cdbDAONode.h,v 1.7 2010/04/20 14:33:39 bjeram Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2005/09/11  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>
#include <map>

#include "cdbDAOProxy.h"
#include <cdbDALS.h>

namespace cdb {

    // forward declaration
    class DAONode;

    /**
     * DAL change listener implementation.
     */
    class DALChangeListenerImplementation :
	public virtual PortableServer::RefCountServantBase,
	public virtual POA_CDB::DALChangeListener,
	public virtual POA_ACS::OffShoot
    {
      public:

	/**
	 * Constrcutor.
	 * @param dal	DAL to be monitored.
	 * @param poa   POA to be used to activate CORBA object (SYSTEM_ID).
	 */
	DALChangeListenerImplementation(CDB::DAL_ptr, PortableServer::POA_ptr poa);

	/**
	 * Destructor (to be called only by POA, use destroy method instead).
	 */
	virtual ~DALChangeListenerImplementation();
	
	/**
	 * Register DAONode instance.
	 */
	void registerNode(DAONode *node);

	/**
	 * Unregister DAONode instance.
	 */
	void unregisterNode(DAONode *node);

	/**
	 * Destroy this object (via POA).
	 */
	void destroy();

	//----------------------------------------------------
	// CORBA DALChangeListenerImplementation interface
	//----------------------------------------------------

	virtual void object_changed (const char * curl);

      protected:

	/// CDB DAL.
	CDB::DAL_var m_dal;
	
	/// Change listener ID.
	long m_changeListenerID;

	typedef std::vector<DAONode*> VectorDAONode;
	typedef std::map<std::string, VectorDAONode> MapVectorDAONode;
	
	/// DAONode map.
	MapVectorDAONode nodeMap;

	/// CORBA POA.
	PortableServer::POA_var m_poa;
    };

    /**
     * DAL Access implementation.
     */
    class DAONode :
	public virtual POA_CDB::DAO
    {
      public:

	/**
	 * Constrcutor.
	 * @param nodeName	name of the CDB node (DAO) to be created.
	 * @param dal	CDB DAL.
	 * @param poa   POA to be used to activate CORBA object (SYSTEM_ID).
	 */
	DAONode(const char* nodeName, CDB::DAL_ptr dal, PortableServer::POA_ptr poa);

	/**
	 * Destructor.
	 */
	virtual ~DAONode();

	/**
	 * Read configuration.
	 */
	void readConfiguration();

	/**
	 * Create DAO using current DAL.
	 * @param nodeName	name of the CDB node (DAO) to be created.
	 */
	DAONode* createDAONode(const char * nodeName);
	
	/**
	 * Create child DAO.
	 * Prefixing all reuqests with child name and delegating it to this instance).
	 */
	DAONode* createChild(const char* childName);

	//----------------------------------------------------
	// CORBA DAO interface
	//----------------------------------------------------
    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual CORBA::Long get_long (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual CORBA::Double get_double (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual char * get_string (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual char * get_field_data (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual ::CDB::stringSeq * get_string_seq (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual ::CDB::longSeq * get_long_seq (const char * propertyName);
    
    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual ::CDB::doubleSeq * get_double_seq (const char * propertyName);

	/**
	 * template version of getter method, which is used mostly in baci
	 * It is specialized for baci (scalar) types
	 * @throw cdbErrType::WrongCDBDataTypeEx
	 * @throw cdbErrType::CDBFieldDoesNotExistEx
	 */
	template<class T>
	T getValue(const char * propertyName);

	virtual void destroy ();

      protected:

	/**
	 * Default constructor.
	 */
	DAONode();

	/**
	 * Connect.
	 * Sets the m_daoImpl.
	 * @param silent exception reporting flag, if <code>true</code> exception will be thrown if connect fails
	 */
	void connect(bool silent = true);

  	/// DAO CURL (name).
	std::string m_name;

	/// CDB DAL.
	CDB::DAL_var m_dal;

	/// CORBA POA.
	PortableServer::POA_var m_poa;

  	/// DAO implementation.
	DAOProxy* m_daoImpl;

	/// local or remote DAO flag
	bool m_remote;
	
	/// DAL change listener.
	DALChangeListenerImplementation* m_dalChangeListener;

	// used to access m_name field and connect method
	friend class DALChangeListenerImplementation;
    };

    /**
     * Child DAL Access implementation.
     */
    class DAOChildNode :
	public DAONode
    {
      public:

	/**
	 * Constrcutor.
	 * @param parent	parent DAONode node.
	 * @param childNamen	child name, prefix to propertyNames (concated string is then delegated to parent).
	 */
	DAOChildNode(DAONode * parent, const char* childName);

	/**
	 * Destructor.
	 */
	virtual ~DAOChildNode();
	
	/**
	 * Create DAO using current DAL.
	 * @param nodeName	name of the CDB node (DAO) to be created.
	 */
	DAONode* createDAONode(const char * nodeName);

	/**
	 * Create child DAO.
	 * Prefixing all reuqests with child name and delegating it to this instance).
	 */
	DAONode* createChild(const char* childName);

	//----------------------------------------------------
	// CORBA DAO interface
	//----------------------------------------------------

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual CORBA::Long get_long (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual CORBA::Double get_double (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual char * get_string (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual char * get_field_data (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual ::CDB::stringSeq * get_string_seq (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual ::CDB::longSeq * get_long_seq (const char * propertyName);

    /*
    * @throw cdbErrType::WrongCDBDataTypeEx
    * @throw cdbErrType::CDBFieldDoesNotExistEx
    */
	virtual ::CDB::doubleSeq * get_double_seq (const char * propertyName);

	/*
	 * @throw cdbErrType::WrongCDBDataTypeEx
	 * @throw cdbErrType::CDBFieldDoesNotExistEx
   */
	template<class T>
	T getValue(const char * propertyName)
	{
		return m_parent->getValue<T>(propertyName);
	}

	virtual void destroy ();

      protected:
	
        /// Parent instance (delegate).
	DAONode* m_parent;

	/// Child name (prefix).
        std::string m_childName;

	/// Prefix with hierarchy delimiter.
        std::string m_childNamePrefix;
    };

} /* namespace cdb */

#endif /* cdbDAONode_H_  */


// -------------------------------------------------------
/*___oOo___*/




