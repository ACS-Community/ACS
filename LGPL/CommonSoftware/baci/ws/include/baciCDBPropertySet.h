#ifndef baciCDBPropertySet_h
#define baciCDBPropertySet_h

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciCDBPropertySet.h,v 1.96 2005/09/28 13:52:49 msekoran Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/05/24  created
*/

/** 
 * @file 
 * Header file BACI Property Set.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <baciExport.h>
#include <orbsvcs/CosPropertyServiceS.h>

#include <map>
#include "baciCharacteristicModelImpl.h"

NAMESPACE_BEGIN(baci);

/**
 * The PropertySet interface provides operations to define
 * and modify properties, list and get properties, and
 * delete properties.
 *
 * This implementation of PropertySet retrieves data from CDB.
 * <br>
 * By using the USE_DEFAULT_SERVANT policy, a POA will use a single servant to implement all of its objects.
 * This approach is useful when there is very little data associated with each object, 
 * so little that the data can be encoded in the Object Id.
 * <br>
 * <b><i><h3>NOTE: before using CDBPropertySet methods you must create singleton by calling CDBPropertySet::createInstance()</h3></i></b>
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */
class CDBPropertySet : public virtual POA_CosPropertyService::PropertySet
{
  public:
    
    /**
     * This constructor creates a singleton, it has to be called BEFORE using any of CDBPropertySet methods and
     * has to be called only once.
     * @param orb CORBA ORB reference
     * @param poa_manager POA Manager reference
     * @param root_poa root POA reference
     * @return CDBPropertySet reference to the CDBPropertySet singleton
     */
    static CDBPropertySet * createInstance (CORBA::ORB_ptr orb,
					    PortableServer::POAManager_ptr poa_manager,
					    PortableServer::POA_ptr root_poa);
    
    /**
     * Accessor to CDBPropertySet singleton.
     * @return CDBPropertySet reference to the CDBPropertySet singleton
     */
    static CDBPropertySet * getInstance()
	{
	    return instance_mp;
	}
    
  protected:
    
    /**
     * Protected constructor which actually creates a singleton.
     * @param orb CORBA ORB reference
     * @param poa_manager POA Manager reference
     * @param root_poa root POA reference
     * @return CDBPropertySet reference to the CDBPropertySet singleton
     * @see CDBPropertySet#createInstance
     */
    CDBPropertySet (CORBA::ORB_ptr orb,
		    PortableServer::POAManager_ptr poa_manager,
		    PortableServer::POA_ptr root_poa);
    
  public:
    /**
     * Destructor.
     */
    virtual ~CDBPropertySet (void);


    /**
     * Register characteristic model
     */
    void registerCharacteristicModel(const char * modelName, CharacteristicModelImpl* model);

    /**
     * Deregister characteristic model.
     */
    void deregisterCharacteristicModel(const char * modelName);

    
    /** 
     * Returns RepositoryId of this servant.
     * @return RepositoryId of this servant
     */
    const char * getRepositoryId ();
    
    /** 
     * Returns ObjectId of this servant.
     * @return ObjectId of this servant
     */
    char * getObjectId();
    
    /** 
     * Returns PropertySet reference (if neccessary also creates a reference to the CDBPropertySet default servant).
     * This methos creates objects without actually incarnating them with servants.
     * NODE: registerCharacteristicModel(propertyName, model) has to be called first!
     * @param propertyName name of the property
     * @return PropertySet reference
     */
    CosPropertyService::PropertySet * getPropertySet(const char * propertyName);
    
    /**
     * Support for defining and modifying properties.
     * Will modify or add a property to the PropertySet. If the
     * property already exists, then the property type is checked
     * before the value is overwritten. If the property does not
     * exist, then the property is added to the PropertySet.<br>
     * <b>NOTE: The implementation of CDBPropertySet acts only as accessor,
     * therefore this method throws CosPropertyService::ReadOnlyProperty() exception.</b>
     * @param property_name name of the property
     * @param property_value value of the property
     */
    virtual void define_property (const char * property_name,
				  const CORBA::Any & property_value)
	throw (CORBA::SystemException,
	       CosPropertyService::InvalidPropertyName,
	       CosPropertyService::ConflictingProperty,
	       CosPropertyService::UnsupportedTypeCode,
	       CosPropertyService::UnsupportedProperty,
	       CosPropertyService::ReadOnlyProperty);
    
    /**
     * Support for defining and modifying multiple properties at once.
     * Will modify or add a property to the PropertySet. If the
     * property already exists, then the property type is checked
     * before the value is overwritten. If the property does not
     * exist, then the property is added to the PropertySet.<br>
     * <b>NOTE: The implementation of CDBPropertySet acts only as accessor,
     * therefore this method throws CosPropertyService::ReadOnlyProperty() exception.</b>
     * @param nproperties list of properties (name-value pairs)
     */
    virtual void define_properties (const CosPropertyService::Properties & nproperties)
	throw (CORBA::SystemException,
	       CosPropertyService::MultipleExceptions);
    
    /**
     * Support for Getting Properties and their Names.
     * Returns the current number of properties associated with this
     * PropertySet.
     * @return number of properties
     */
    virtual CORBA::ULong get_number_of_properties ()
	throw (CORBA::SystemException);
    
    /**
     * Returns all of the property names currently defined in the
     * PropertySet. If the PropertySet contains more than how_many
     * property names, then the remaining property names are put
     * into the PropertyNamesIterator.
     * @param how_many upper bound of number of properties returned in <property_names> sequence
     * @param properties_names out sequence of property names
     * @param rest reference to iterator to the rest of property names
     */
    virtual void get_all_property_names (CORBA::ULong how_many,
					 CosPropertyService::PropertyNames_out property_names,
					 CosPropertyService::PropertyNamesIterator_out rest)
	throw (CORBA::SystemException);
    
    /**
     * Returns the value of a property in the PropertySet.
     * @param property_name name of the property
     */
    virtual CORBA::Any * get_property_value (const char * property_name)
	throw (CORBA::SystemException,
	       CosPropertyService::PropertyNotFound,
	       CosPropertyService::InvalidPropertyName);
    
    /**
     * Returns the values of the properties listed in
     * property_names.
     * @param property_names sequence of property names
     */
    virtual CORBA::Boolean get_properties (const CosPropertyService::PropertyNames & property_names,
					   CosPropertyService::Properties_out nproperties)
	throw (CORBA::SystemException);

    /**
     * Returns all of the property names currently defined in the
     * PropertySet. If the PropertySet contains more than how_many
     * property names, then the remaining property names are put
     * into the PropertyNamesIterator.
     * @param how_many upper bound of number of properties returned in <property_names> sequence
     * @param nproperties out sequence of Properties (name-valie pairs)
     * @param rest reference to iterator to the rest of Properties
     */
    virtual void get_all_properties (CORBA::ULong how_many,
				     CosPropertyService::Properties_out nproperties,
				     CosPropertyService::PropertiesIterator_out rest)
	throw (CORBA::SystemException);

    /**
     * Support for Deleting Properties.
     * Deletes the specified property if it exists from a
     * PropertySet.<br>
     * <b>NOTE: The implementation of CDBPropertySet acts only as accessor,
     * therefore this method throws CosPropertyService::FixedProperty() exception.</b>
     * @param property_name name of the property
     */
    virtual void delete_property (const char * property_name)
	throw (CORBA::SystemException,
	       CosPropertyService::PropertyNotFound,
	       CosPropertyService::InvalidPropertyName,
	       CosPropertyService::FixedProperty);
    
    /**
     * Support for Deleting Properties.
     * Deletes the properties defined in the property_names
     * parameter. This is a batch operation that returns the
     * MultipleExceptions exception if any delete failed.<br>
     * <b>NOTE: The implementation of CDBPropertySet acts only as accessor,
     * therefore this method throws list of CosPropertyService::FixedProperty() exceptions.</b>
     * @param property_names sequence of propery names
     */
    virtual void delete_properties (const CosPropertyService::PropertyNames & property_names)
	throw (CORBA::SystemException,
	       CosPropertyService::MultipleExceptions);
    
    /**
     * Validation of delete_properties. Applies to all properties.
     * <b>NOTE: The implementation of CDBPropertySet acts only as accessor,
     * therefore this method always returns false.
     * @return false 
     */
    virtual CORBA::Boolean delete_all_properties ()
	throw (CORBA::SystemException);
    
    /**
     * Support for Existence Check.
     * The is_property_defined operation returns true if the
     * property is defined in the PropertySet, and returns false
     * otherwise.
     * @param property_name name of the property
     */
    virtual CORBA::Boolean is_property_defined (const char * property_name)
	throw (CORBA::SystemException,
	       CosPropertyService::InvalidPropertyName);
    
  private:
    
    /// Instance to this object
    static CDBPropertySet * instance_mp;
    
    /// POA using DEFAULT_SERVANT policy
    PortableServer::POA_var poa_m;
    
    /// POA Current
    PortableServer::Current_var poaCurrent_m;

    typedef map<string, CharacteristicModelImpl*> CharacteristicModelImplMap;
    /// Map of characteristic models.
    CharacteristicModelImplMap modelMap;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const CDBPropertySet&);
    

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    CDBPropertySet(const CDBPropertySet&);  
};

NAMESPACE_END(baci);

#endif /* baciCDBPropertySet_h  */





