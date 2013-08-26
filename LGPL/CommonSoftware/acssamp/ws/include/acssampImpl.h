#ifndef _ACSSAMP_IMPL_H
#define _ACSSAMP_IMPL_H
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
 * "@(#) $Id: acssampImpl.h,v 1.23 2008/10/07 06:41:54 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       07/04/03  created 
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciCharacteristicComponentImpl.h>
#include <ACSErrTypeCommon.h>

#include <acssampS.h>

#include <vector>
#include <list>

/**
 * The ACE NAMESPACE_USE() macro is equivalent to the use C++ directive,
 * but allows namespace support for compilers that do not have it 
 * natively (i.e., GCC 2.95 for VxWorks).
 */ 

/** @file acssampImpl.h
 *  Header file for the factory object, which allows to create 
 *  sampling objects.
 *  
 */

/** @class ACSSampImpl
 *  This class implements basically one method (initSampObj),
 *  which allows to create dynamically the so called "sampling
 *  objects". Each sampling object is connected to a specific
 *  property (e.g. LAMP1:brightness) and allows to sample its
 *  values with a user defined sampling frequency. Moreover the
 *  sampled value are not delivered immediately to the NC,
 *  but are buffered for an user-defined amount of time
 *  (in order to not waste network bandwidth). 
 *
 */


class ACSSampImpl: public virtual baci::CharacteristicComponentImpl,
		   public virtual POA_acssamp::Samp
{    
  public:
    
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other Components. 
     * @param name DO's name. This is also the name that will be used to find the
     * configuration data for the DO in the Configuration Database.
     */

    ACSSampImpl(const ACE_CString& name, maci::ContainerServices *containerServices);
  
    /**
     * Destructor
     */
    virtual ~ACSSampImpl();
  
    /**
     * initSampObj
     * Initialize and returns a reference to the newly created sampling object. 
     * @param name   name of the component, which property is to be sampled
     *               (e.g.<i>LAMP1 </i>)
     * @param property   name of the property to be sampled (e.g. <i>brightness </i>)
     * @param frequency  sampling frequency: period between two consecutive sampling
     *                      <i>(units are 100ns; e.g. 1000000 means 0.1 sec i.e. 10 samples per second) </i>
     * @param reportRate    number of second the process should buffer 
     *                      before actually sending (notifying) data
     *                      <i>(units are 100ns; e.g. 10000000 means collect data
     *                       for 1 second)</i>
     *
     *
     * @throw ACSErrTypeCommon::OutOfBoundsEx
     * @throw ACSErrTypeCommon::MemoryFaultEx
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @throw ACSErrTypeCommon::CouldntCreateObjectEx
     * @throw ACSErrTypeCommon::TypeNotSupportedEx
     * @throw ACSErrTypeCommon::CouldntAccessPropertyEx
     * @throw ACSErrTypeCommon::CouldntAccessComponentEx
     * @return SampObj_ptr  pointer to the newly created sampling object. It allows
     *                      to control the actual sampling (start/stop/pause etc.)
     *
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */

    virtual acssamp::SampObj_ptr initSampObj(const char *name,
					     const char *property,
					     ACS::TimeInterval frequency,
					     ACS::TimeInterval reportRate
	);


    /**
     * Internal method used to add all newly created sampling objects to an
     * internal list (containing a reference to the active objects). 
     * If objects are not correctly destroyed, this list is
     * used by the destructor
     * of the factory (this class) to clean-up everything in a consistent way.
     *
     * @param component_ref   pointer to a CORBA object
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */ 
    virtual void addComponenttoList(CORBA::Object_ptr component_ref);


    /**
     * Internal method used to remove a sampling object from the
     * internal list of the active (instantiated) objects.
     * This method is called automatically, whenever a previously created object is
     * destroyed. 
     *
     * @param component_ref   pointer to a CORBA object
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */ 
    virtual void removeComponentfromList(CORBA::Object_ptr component_ref);
  

  private:
  
    /**
     * component_list  is an internal list of the active sampling objects.
     */
    std::list<CORBA::Object_ptr> component_list;

    ACE_Recursive_Thread_Mutex m_samplingListMutex;
};



#endif /*!_ACSSAMP_IMPL__H*/









