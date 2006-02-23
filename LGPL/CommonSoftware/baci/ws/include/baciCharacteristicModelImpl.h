#ifndef baciCharacteristicModelImpl_H
#define baciCharacteristicModelImpl_H

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
* "@(#) $Id: baciCharacteristicModelImpl.h,v 1.9 2006/01/12 21:31:12 gchiozzi Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/01/21  created

*/

/** 
 * @file 
 * Header file BACI Characteristic Model.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciS.h>

#include <acsContainerServices.h>
#include <lokiSmartPtr.h>
#include <cdbDAONode.h>
#include <cdbErrType.h>

NAMESPACE_BEGIN(baci);

class baci_EXPORT CharacteristicModelImpl : public virtual PortableServer::RefCountServantBase,
					    public virtual POA_ACS::CharacteristicModel
{
  public:

    /**
     * Constructor creating a model for given model name.
     *
     * TODO In case the CDB for this CharacteristicModel
     *      is not found (cdbErrType::CDBRecordDoesNotExistExImpl)
     *      we do not fail, but we continue assuming that
     *      we dealing with Components without properties and characteristics.
     *      Such Components should actually be ACSComponents and not
     *      CharacteristicComponents, so we write logs, but
     *      we try anyway to continue to be backward compatible.
     *      This will delay real problems with missing CDB instances
     *      for "true" CharacteristicComponents
     *      up to a later point when characteristics or properties
     *      will be accessed.
     *  
     *      CharacteristicComponents without CDB are deprecated 
     *      ans will be not allowed any more on a later release
     */
    CharacteristicModelImpl(const ACE_CString& model_name, ContainerServices* containerServices);

    /**
     * Constructor creating a child model (for properties).
     * TODO In case the CDB for this CharacteristicModel
     *      is not found (cdbErrType::CDBRecordDoesNotExistExImpl)
     *      we do not fail, but we continue assuming that
     *      we dealing with Components without properties and characteristics.
     *      Such Components should actually be ACSComponents and not
     *      CharacteristicComponents, so we write logs, but
     *      we try anyway to continue to be backward compatible.
     *      This will delay real problems with missing CDB instances
     *      for "true" CharacteristicComponents
     *      up to a later point when characteristics or properties
     *      will be accessed.
     *  
     *      CharacteristicComponents without CDB are deprecated 
     *      ans will be not allowed any more on a later release
     */
    CharacteristicModelImpl(const ACE_CString& child_name, CharacteristicModelImpl* parentModel);
    
    virtual ~CharacteristicModelImpl();

    const char* getModelName() const { return model_name_m.c_str(); };

    cdb::DAONode* getDAONode() const throw(cdbErrType::CDBRecordDoesNotExistExImpl);

    /* ------------------ [ CharacteristicModel interface ] ------------------ */

    virtual CosPropertyService::PropertySet_ptr get_all_characteristics ()
	throw (CORBA::SystemException);
    
    virtual CORBA::Any * get_characteristic_by_name (const char * name)
	throw (CORBA::SystemException,
	       ACS::NoSuchCharacteristic);
    
    virtual ACS::stringSeq * find_characteristic (const char * reg_exp)
	throw (CORBA::SystemException);

  protected:

    /**
     * Read characteristics from CDB
     * @return true on success, false on failure
     */
    virtual bool readCharacteristics();

    /// Name of the model.
    ACE_CString model_name_m;

    /// Smart pointer to DAO node.
    /// TODO this should become provate and only getDAONode should be allowed
    /// but we cannot do it now to allow backward compatibility.
    cdb::DAONode* m_daoNode_p;

    // TODO temp and not OK !!!
    static PortableServer::POA_ptr offShootPOA;

}; //class CharacteristicModelImpl

NAMESPACE_END(baci);

#endif





