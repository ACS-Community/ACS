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
 * "@(#) $Id: cdbDAONode.cpp,v 1.16 2012/11/05 08:58:27 msekoran Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2005/09/11  created
 */

#include "cdbDAONode.h"
#include <logging.h>

#include "cdbErrType.h"
#include <ACSErrTypeCORBA.h>
#include <ACSErrTypeCommon.h>

using namespace cdb;
using namespace std;

namespace cdb
{

//----------------------------------------------------
// DALChangeListenerImplementation
//----------------------------------------------------

// TODO implement per DAL listener, not per component (usage of container container services and getDALChangeListener(CDB::DAL_ptr)) 
DALChangeListenerImplementation::DALChangeListenerImplementation(CDB::DAL_ptr dal, PortableServer::POA_ptr poa) :
    m_dal(CDB::DAL::_duplicate(dal)), m_poa(PortableServer::POA::_duplicate(poa))
{
    ACS_TRACE("cdb::DALChangeListenerImplementation::DALChangeListenerImplementation");

    if (m_poa.ptr() == PortableServer::POA::_nil())
	return;

    // activate the CORBA object (SYSTEM_ID -> activate_object)
    PortableServer::ObjectId_var oid = m_poa->activate_object(this);
    // create an object reference
    CORBA::Object_var obj = m_poa->id_to_reference(oid.in());
    // remove reference (POA will now destroy servant)
    this->_remove_ref();

    CDB::DALChangeListener_var m_changeListenerObj = CDB::DALChangeListener::_narrow(obj.in());
    
    if(m_dal.ptr() != CDB::DAL::_nil()) 
	m_changeListenerID = m_dal->add_change_listener(m_changeListenerObj.in());

}

DALChangeListenerImplementation::~DALChangeListenerImplementation()
{
    ACS_TRACE("cdb::DALChangeListenerImplementation::~DALChangeListenerImplementation");

    try
	{
	if(m_dal.ptr() != CDB::DAL::_nil()) 
	    {
	    m_dal->remove_change_listener(m_changeListenerID);
	    }
	}
    catch (...)
	{
	// don't worry server will handle itself
	}
}

void DALChangeListenerImplementation::destroy()
{
    ACS_TRACE("cdb::DALChangeListenerImplementation::destroy");

    // destroy: direct or via POA
    if (m_poa.ptr() == PortableServer::POA::_nil())
	{
	delete this;
	}
    else
	{
	// deactivate the servant
	PortableServer::ObjectId_var id = m_poa->servant_to_id(this);
	m_poa->deactivate_object(id.in());
	}
}
	
void DALChangeListenerImplementation::registerNode(DAONode *node)
{
    ACS_TRACE("cdb::DALChangeListenerImplementation::registerNode");

    if (nodeMap.find(node->m_name) == nodeMap.end())
	{
	m_dal->listen_for_changes(node->m_name.c_str(), m_changeListenerID);
	}

    nodeMap[node->m_name].push_back(node);
}

void DALChangeListenerImplementation::unregisterNode(DAONode *node)
{
    ACS_TRACE("cdb::DALChangeListenerImplementation::unregisterNode");
    
    if (nodeMap.find(node->m_name) != nodeMap.end())
	{
	VectorDAONode::iterator iter = nodeMap[node->m_name].begin();
	VectorDAONode::iterator end = nodeMap[node->m_name].end();
	while (iter != end)
	    {
	    if (*iter == node)
		{
		nodeMap[node->m_name].erase(iter);
		break;
		}
	    iter++;
	    }
	}

}

void DALChangeListenerImplementation::object_changed(const char * curl)
{
    ACS_TRACE("cdb::DALChangeListenerImplementation::object_changed");

    if (nodeMap.find(curl) != nodeMap.end())
	{
	VectorDAONode::iterator iter = nodeMap[curl].begin();
	VectorDAONode::iterator end = nodeMap[curl].end();
	while (iter != end)
	    {
	    try
		{
		(*iter)->connect();
		}
	    catch (...)
		{
		// noop
		}
	    iter++;
	    }
	}
}

//----------------------------------------------------
// DAONode
//----------------------------------------------------

DAONode::DAONode() :
    m_name(), m_dal(CDB::DAL::_nil()), m_daoImpl(0), m_remote(false), m_dalChangeListener(0)
{
}

DAONode::DAONode(const char* nodeName, CDB::DAL_ptr dal, PortableServer::POA_ptr poa) :
    m_name(nodeName), m_daoImpl(0), m_remote(false), m_dalChangeListener(0)
{
    ACS_TRACE("cdb::DAONode::DAONode");

    // no check on valid m_containerServices is done

    try
	{
	m_dal = CDB::DAL::_duplicate(dal);
	}
    catch (...)
	{
	throw ACSErrTypeCORBA::FailedToResolveServiceExImpl (__FILE__,__LINE__,"cdb::DAONode::DAONode");
	}

    if (m_dal.ptr() == CDB::DAL::_nil())
	throw ACSErrTypeCORBA::FailedToResolveServiceExImpl (__FILE__,__LINE__,"cdb::DAONode::DAONode");

    m_poa = PortableServer::POA::_duplicate(poa);

    // read config
    readConfiguration();

    // create change listener for DAL
    m_dalChangeListener = new DALChangeListenerImplementation(m_dal.in(), m_poa.in());
    m_dalChangeListener->registerNode(this);

    // connect
    try {
        connect(false);
    //Something went wrong we should unregister the claabak
    } catch (...) {
        m_dalChangeListener->unregisterNode(this);
        try {
            m_dalChangeListener->destroy();
        } catch(...) {
        }
        throw;
    }
}

DAONode::~DAONode()
{
    ACS_TRACE("cdb::DAONode::~DAONode");

    if (m_daoImpl)
	{
	delete m_daoImpl;
	}
    
    if (m_dalChangeListener)
	{
	m_dalChangeListener->unregisterNode(this);

	try
	    {
	    // since it is a servant, CORBA will destroy it
	    m_dalChangeListener->destroy();
	    }
	catch (...)
	    {
	    // noop (called just to be safe - so that memory will be freed)
	    }
	}
}

void DAONode::readConfiguration()
{
    ACS_TRACE("cdb::DAONode::readConfiguration");

    // TODO read config somehow...
    m_remote = false;
}

DAONode* DAONode::createDAONode(const char * nodeName)
{
    ACS_TRACE("cdb::DAONode::createDAONode");

    return new DAONode(nodeName, m_dal.in(), m_poa.in());
}
	
DAONode* DAONode::createChild(const char* childName)
{
    ACS_TRACE("cdb::DAONode::createChild");

    return new DAOChildNode(this, childName);

}

void DAONode::connect(bool silent)
{
    ACS_TRACE("cdb::DAONode::connect");

    DAOProxy* newDAOProxy = 0;

    // get new DAO implementation (reconnect)
    try
	{
	if (m_remote)
	    {
	    CDB::DAO_var dao = m_dal->get_DAO_Servant(m_name.c_str());
	    newDAOProxy = new DAOProxy(m_name.c_str(), dao.in());
	    }
	else
	    {
	    CORBA::String_var xml = m_dal->get_DAO(m_name.c_str());
	    newDAOProxy = new DAOProxy(m_name.c_str(), xml.in());
	    }
	}
    catch (cdbErrType::CDBRecordDoesNotExistEx &rde)
	{
	if (!silent)
	    {
	    cdbErrType::CDBRecordDoesNotExistExImpl rdeImpl(rde);
	    cdbErrType::CDBRecordDoesNotExistExImpl ex = 
		cdbErrType::CDBRecordDoesNotExistExImpl (rdeImpl, __FILE__, __LINE__,
							 "cdb::DAONode::connect");
	    ex.setCurl(rdeImpl.getCurl());
	    throw ex;
	    }
	}
    catch (cdbErrType::CDBXMLErrorEx &xee)
	{
	if (!silent)
	    {
	    cdbErrType::CDBXMLErrorExImpl xeeImpl(xee);
	    cdbErrType::CDBXMLErrorExImpl ex = 
		cdbErrType::CDBXMLErrorExImpl (xeeImpl, __FILE__, __LINE__,
							 "cdb::DAONode::connect");
	    ex.setCurl(xeeImpl.getCurl());
	    ex.setFilename(xeeImpl.getFilename());
	    ex.setNodename(xeeImpl.getNodename());
	    ex.setErrorString(xeeImpl.getErrorString());
	    throw ex;
	    }
	}
    // DAOProxy constructor throws CDBXMLErrorExImpl
    catch (cdbErrType::CDBXMLErrorExImpl &xee)
	{
	if (!silent)
	    {
	    cdbErrType::CDBXMLErrorExImpl xeeImpl(xee);
	    cdbErrType::CDBXMLErrorExImpl ex =
		cdbErrType::CDBXMLErrorExImpl (xeeImpl, __FILE__, __LINE__,
							 "cdb::DAONode::connect");
	    ex.setCurl(xeeImpl.getCurl());
	    ex.setFilename(xeeImpl.getFilename());
	    ex.setNodename(xeeImpl.getNodename());
	    ex.setErrorString(xeeImpl.getErrorString());
	    throw ex;
	    }
	}
    catch (ACSErr::ACSbaseExImpl& ex)
	{
	if (!silent)
	    {
	    throw ACSErrTypeCommon::CouldntCreateObjectExImpl (ex,__FILE__, __LINE__,"cdb::DAONode::connect");
	    }
	}
    catch (CORBA::SystemException& ex)
	{
	if (!silent)
	    {
	    ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx = 
		ACSErrTypeCommon::CORBAProblemExImpl (__FILE__, 
						      __LINE__,
						      "cdb::DAONode::connect");
	    corbaProblemEx.setMinor(ex.minor());
	    corbaProblemEx.setCompletionStatus(ex.completed());
	    corbaProblemEx.setInfo(ex._info().c_str());

	    throw corbaProblemEx;
	    }
	}
    catch (...)
	{
	if (!silent)
	    {
	    throw ACSErrTypeCommon::UnexpectedExceptionExImpl(__FILE__, __LINE__,"cdb::DAONode::connect");
	    }
	}
    
    // replace DAO implementations
    if (newDAOProxy)
	{
	DAOProxy* oldDAOProxy = m_daoImpl;
	m_daoImpl = newDAOProxy;
	if (oldDAOProxy)
	    delete oldDAOProxy;
	}
}




CORBA::Long DAONode::get_long (const char * propertyName)
{
    //ACS_TRACE("cdb::DAONode::get_long");

    if (m_daoImpl)
	return m_daoImpl->get_long(propertyName);
    else
	throw CORBA::NO_RESOURCES();
}

CORBA::Double DAONode::get_double (const char * propertyName)
{
    //ACS_TRACE("cdb::DAONode::get_double");

    if (m_daoImpl)
	return m_daoImpl->get_double(propertyName);
    else
	throw CORBA::NO_RESOURCES();
}

char * DAONode::get_string (const char * propertyName)
{
    //ACS_TRACE("cdb::DAONode::get_string");

    if (m_daoImpl)
	return m_daoImpl->get_string(propertyName);
    else
	throw CORBA::NO_RESOURCES();
}

char * DAONode::get_field_data (const char * propertyName)
{
    //ACS_TRACE("cdb::DAONode::get_field_data");

    if (m_daoImpl)
	return m_daoImpl->get_field_data(propertyName);
    else
	throw CORBA::NO_RESOURCES();
}

CDB::stringSeq* DAONode::get_string_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAONode::get_string_seq");

    if (m_daoImpl)
	return m_daoImpl->get_string_seq(propertyName);
    else
	throw CORBA::NO_RESOURCES();
}

CDB::longSeq * DAONode::get_long_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAONode::get_long_seq");

    if (m_daoImpl)
	return m_daoImpl->get_long_seq(propertyName);
    else
	throw CORBA::NO_RESOURCES();
}


CDB::doubleSeq * DAONode::get_double_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAONode::get_double_seq");

    if (m_daoImpl)
	return m_daoImpl->get_double_seq(propertyName);
    else
	throw CORBA::NO_RESOURCES();
}



// Specialization for double
template<>
CORBA::Double DAONode::getValue(const char * propertyName)
{
	return this->get_double( propertyName );
}
//we have to cast because from DAO we get just get_double
template<>
CORBA::Float DAONode::getValue(const char * propertyName)
{
	return static_cast<CORBA::Float>(this->get_double( propertyName ));
}

// Specialization for long
template<>
CORBA::Long DAONode::getValue(const char * propertyName)
{
	return get_long( propertyName );
}

template<>
ACS::uLong DAONode::getValue(const char * propertyName)
{
	// we can not use get_long to read xs:unisngedInt which its max value is larger than xs:int  we have to read string and cast it to unsigned long
	// see: COMP-4268
	ACS::uLong var;
	CORBA::String_var str = this->get_string( propertyName );
	std::istringstream is(str.in());
	(istream&) is >> var;
	if (!is) {
		ACS_LOG(LM_RUNTIME_CONTEXT, "DAONode::getValue<ACS::uLong>",
			(LM_ERROR, "Problem converting string value: %s to ACS::uLong!", str.in()));
		throw cdbErrType::WrongCDBDataTypeEx();
	}//if
	return var;
}

//we have to cast because from DAO we get just get_long
template<>
ACS::longLong DAONode::getValue(const char * propertyName)
{
	// until get_long is fixed that it can read also xs:long which is 64 bit we have to read string and cast it to long long
	ACS::longLong var;
	CORBA::String_var str = this->get_string( propertyName );
	std::istringstream is(str.in());
	(istream&) is >> var ;
		if (!is)
		{
			ACS_LOG(LM_RUNTIME_CONTEXT, "DAONode::getValue<long long>",
						  (LM_ERROR, "Problem converting string value: %s to long long!", str.in()));
			 throw cdbErrType::WrongCDBDataTypeEx();
		}//if

		return var;
}

//we have to cast because from DAO we get just get_long
template<>
ACS::uLongLong DAONode::getValue(const char * propertyName)
{
	// we can not use get_long to read xs:unisngedLong which is 64 bit we have to read string and cast it to unsigned long long
	// see: COMP-4268
		ACS::uLongLong var;
		CORBA::String_var str = this->get_string( propertyName );
		std::istringstream is(str.in());
		(istream&) is >> var ;
			if (!is)
			{
				ACS_LOG(LM_RUNTIME_CONTEXT, "DAONode::getValue<unsigned long long>",
							  (LM_ERROR, "Problem converting string value: %s to unsigned long long!", str.in()));
				 throw cdbErrType::WrongCDBDataTypeEx();
			}//if

			return var;
}

template<>
CORBA::Boolean DAONode::getValue(const char * propertyName)
{
	// we can not use get_long to read xs:unisngedInt which its max value is larger than xs:int  we have to read string and cast it to unsigned long
	// see: COMP-4268
	CORBA::Boolean var;
	CORBA::String_var str = this->get_string( propertyName );
	std::istringstream is(str.in());
	(istream&) is >> var;
	if (!is) {
		std::istringstream is2(str.in());
		(istream&) is2 >> std::boolalpha >> var;
		if(!is2) {
			ACS_LOG(LM_RUNTIME_CONTEXT, "DAONode::getValue<CORBA::Boolean>",
				(LM_ERROR, "Problem converting string value: %s to CORBA::Boolean!", str.in()));
			throw cdbErrType::WrongCDBDataTypeEx();
		}
	}//if
	return var;
}


void DAONode::destroy ()
{
    //ACS_TRACE("cdb::DAONode::destroy");

    if (m_daoImpl)
	return m_daoImpl->destroy();
    else
	throw CORBA::NO_RESOURCES();
}

//----------------------------------------------------
// DAOChildNode
//----------------------------------------------------

DAOChildNode::DAOChildNode(DAONode* parent, const char * childName) :
    m_parent(parent), m_childName(childName)
{
    ACS_TRACE("cdb::DAOChildNode::DAOChildNode");

    m_childNamePrefix = m_childName + "/";
}

DAOChildNode::~DAOChildNode()
{
    ACS_TRACE("cdb::DAOChildNode::~DAOChildNode");
}

DAONode* DAOChildNode::createDAONode(const char * nodeName)
{
    ACS_TRACE("cdb::DAOChildNode::createDAONode");

    return m_parent->createDAONode(nodeName);
}

DAONode* DAOChildNode::createChild(const char* childName)
{
    ACS_TRACE("cdb::DAOChildNode::createChild");

    string name = m_childNamePrefix + childName;
    return m_parent->createChild(name.c_str());
}


CORBA::Long DAOChildNode::get_long (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOChildNode::get_long");

    string name = m_childNamePrefix + propertyName;
    return m_parent->get_long(name.c_str());
}

CORBA::Double DAOChildNode::get_double (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOChildNode::get_double");

    string name = m_childNamePrefix + propertyName;
    return m_parent->get_double(name.c_str());
}

char * DAOChildNode::get_string (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOChildNode::get_string");

    string name = m_childNamePrefix + propertyName;
    return m_parent->get_string(name.c_str());
}

char * DAOChildNode::get_field_data (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOChildNode::get_field_data");

    string name = m_childNamePrefix + propertyName;
    return m_parent->get_field_data(name.c_str());
}

CDB::stringSeq* DAOChildNode::get_string_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOChildNode::get_string_seq");

    string name = m_childNamePrefix + propertyName;
    return m_parent->get_string_seq(name.c_str());
}

CDB::longSeq * DAOChildNode::get_long_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOChildNode::get_long_seq");

    string name = m_childNamePrefix + propertyName;
    return m_parent->get_long_seq(name.c_str());
}


CDB::doubleSeq * DAOChildNode::get_double_seq (const char * propertyName)
{
    //ACS_TRACE("cdb::DAOChildNode::get_double_seq");

    string name = m_childNamePrefix + propertyName;
    return m_parent->get_double_seq(name.c_str());
}

void DAOChildNode::destroy ()
{
    //ACS_TRACE("cdb::DAOChildNode::destroy");

    // noop (should not destroy parent)
}

};//namespace cdb













/*
int main(int argc, char** argv)
{

    LoggingProxy * logger = new LoggingProxy(0, 0, 31);
    if (logger)
    {
	LoggingProxy::init(logger);
	LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("main");
    }
 
    CORBA::ORB_var orb = CORBA::ORB_init(argc, argv, "TAO");
    PortableServer::POA_var poaRoot ;
    {
    
    CORBA::Object_var objRootPOA =
	orb->resolve_initial_references("RootPOA");
    
    
    poaRoot = PortableServer::POA::_narrow(objRootPOA.in());
	
      
    PortableServer::POAManager_var poaManager =
	poaRoot->the_POAManager();
	
    poaManager->activate();
      

    }









    CORBA::Object_var obj = orb->string_to_object("corbaloc::localhost:3012/CDB");

    if (CORBA::is_nil(obj.in()))
	{
	printf("Failed to resolve CDB ref.\n");
	return -1;
	}

    CDB::DAL_var dal = CDB::DAL::_narrow(obj.in());
    
    DAONode* dao = new DAONode("alma/PBEND_B_01", dal.in());


	{
	//      printf("Attributes: '%s'.\n--------------------\n", dao->get_field_data(""));
	for (int i = 1; i < argc; i++) {
	try
	    {
	    //	    printf("%s = '%d'.\n", argv[i], dao->get_long(argv[i]));
	    //      printf("%s = '%f'.\n", argv[i], dao->get_double(argv[i]));
	    
	    CDB::stringSeq_var seq = dao->get_string_seq(argv[i]);
	    printf("%s = '\n", argv[i]);
	    for (CORBA::ULong n = 0; n < seq->length(); n++)
		printf("%s\n", seq[n].in());
	    printf("'.\n");
	    
	    
	    CDB::longSeq_var lseq = dao->get_long_seq(argv[i]);
	    printf("%s = '\n", argv[i]);
	    for (CORBA::ULong n = 0; n < lseq->length(); n++)
		printf("%d\n", lseq[n]);
	    printf("'.\n");
	    
	    CDB::doubleSeq_var dseq = dao->get_double_seq(argv[i]);
	    printf("%s = '\n", argv[i]);
	    for (CORBA::ULong n = 0; n < dseq->length(); n++)
		printf("%f\n", dseq[n]);
	    printf("'.\n");
	    //	    printf("%s = '%s'.\n", argv[i], dao->get_field_data(argv[i]));
	    }
	catch (CDB::FieldDoesNotExist fde)
	    {
	    printf("%s = <field does not exist>.\n", argv[i]);
	    }
	catch (CDB::WrongDataType wdt)
	    {
	    CORBA::String_var val = dao->get_field_data(argv[i]);
	    printf("%s = <cannot format '%s'>.\n", argv[i], val.in());
	    }
	}      
	}

	DAONode* childDAO = dao->createChild("current");
	CORBA::Double val = childDAO->get_double("default_value");
	printf("current/default_value: %f\n", val);

	DAONode* childDAO2 = childDAO->createChild("invalid");
	try
	    {
	    childDAO2->get_double("default_value");
	    }
	catch (CDB::FieldDoesNotExist fde)
	    {
	    // ok
	    printf("OK\n");
	    }


	DAONode* dao2 = dao->createDAONode("alma/MOUNT1");

	val = dao2->get_double("cmdAz/default_value");
	printf("cmdAz/default_value: %f\n", val);


	try
	    {
	    dao->createDAONode("invalid");
	    }
	catch (ACSErr::ACSbaseExImpl& ex)
	    {
	    printf("ACSErr::ACSbaseExImpl exception - failed to create <invalid> DALNode.\n");
	    ex.log();
	    }
	catch (...)
	    {
	    printf("Unknown exception - failed to create <invalid> DALacces.\n");
	    }

	
	ACE_Time_Value t(15);
	orb->run(t);
	
	delete dao2;
	delete childDAO2;
	delete childDAO;
	delete dao;

	poaRoot->destroy(1, 1);
	orb->destroy();

 
    return 0;
}
*/

// -------------------------------------------------------
/*___oOo___*/

