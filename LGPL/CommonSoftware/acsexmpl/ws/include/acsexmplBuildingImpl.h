#ifndef acsexmplBuildingImpl_h
#define acsexmplBuildingImpl_h
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
* "@(#) $Id: acsexmplBuildingImpl.h,v 1.108 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use the SmartPropertyPointer for the property
* dave 2003-08-26 removed instances of "acsexmplDoor" as this interface is now defined in acsexmplBuilding.idl
* david 2002-07-02 added GNU License info
* david 2002-06-17 removed Building::*ActionFunction declaration as it is not used in this example
* blopez   2002-04-05 m_poa declaration removed
* jib/blo  2002-04-02 Created
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>

///CORBA generated servant stub
#include <acsexmplBuildingS.h>

///Includes for each BACI property used in this example
#include <baciROstring.h>

///Include the smart pointer for the property
#include <baciSmartPropertyPointer.h>

/** @file acsexmplBuildingImpl.h
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLBUILDINGDOC Building
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The purpose of Building is to show the implementation of hierarchical
devices. &nbsp;Building provides two synchronous methods,
openFrontDoor() and closeFrontDoor(). &nbsp;These methods use the container
to activate a Door object and then perform actions on that object.
&nbsp;Building also has one property, version, which is just the
version of the C++ Building implementation currently used.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::CharacteristicComponent IDL interface.</li>
  <li>overriding component lifecycle methods (see execute).</li>
  <li>the knowledge of how to create heirarchial devices.</li>
  <li>how to use the container services to activate other components.</li>
  <li>writing values to read-only BACI properties by using the property's underlying DevIO instance.</li>
  <li>standard ACS logging macros.</li>
  <li>limited CORBA error handling.</li>
  <li>limited CORBA exception handling.</li>
  ,li>how to use the SmartPropertyPointer for a property</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classBuilding.html">Building Class Reference</a></li>
  <li><a href="interfaceacsexmplBuilding_1_1Building.html">Building IDL Documentation</a></li>
  <li>Building CDB XML Schema</li>
</ul>
<br>
</div>
   @endhtmlonly
 * @}
 */

/** 
 *  Implements an hierarchical device using @ref ACSEXMPLDOORDOC Door.
 *  This class implements an example device "Building". The purpose
 *  is to show the implementation of hierarchical devices.  Building 
 *  has two methods: openFrontDoor() and closeFrontDoor().  It also provides
 *  one property, version, which is just the version of Building being 
 *  used.  Building does not have any Door members, but instead uses the ContainerServices 
 *  to activate a door whenever calls to these methods are made.
 *  Since this device has only synchronous methods, we do not inherit 
 *  from the ActionImplementator class and we do not implement the 
 *  invokeAction method.
 *  @version "@(#) $Id: acsexmplBuildingImpl.h,v 1.108 2008/10/09 08:41:11 cparedes Exp $"
 */
class Building: public baci::CharacteristicComponentImpl,     //Standard component superclass
	        public virtual POA_acsexmplBuilding::Building    //CORBA servant stub
{
  public:   
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other components. 
     * @param name component's name. This is also the name that will be used to find the
     * configuration data for the component in the Configuration Database.
     */
    Building(
	     const ACE_CString &name,
	     maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~Building();
    
    /* --------------------- [ CORBA interface ] ----------------------*/
    /**
     * Opens the FRONTDOOR
     * Implementation of the IDL openFrontDoor() interface.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    openFrontDoor ();
    
    /**
     * Closes the FRONTDOOR
     * Implementation of the IDL closeFrontDoor() interface.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    closeFrontDoor ();
    
    /**
     * Returns a reference to the version property
     * Implementation of IDL interface for the property.
     * @return A pointer to the string containing the version number
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual ACS::ROstring_ptr 
    version ();


    /*Override component lifecycle methods*/
    /**
     * Called after {@link #initialize} to tell the 
     * component that it has to be ready to accept 
     * incoming functional calls any time. 
     * Must be implemented as a synchronous (blocking) call 
     * (can spawn threads though).
     *
     * @throw ACSErr::ACSbaseExImpl
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void execute();

    /**
     * Called after the last functional call to the component has finished.
     * The component should then orderly release resources etc.
     *
     * As required by the CharacteristicComponentImpl class,
     * I call first explicitly the cleanUp() of the parent class.
     * This makes sure that all threads are stopped and the 
     * Component's state set.
     * Depending on what resources are used by a class implementing a 
     * Component and by the implementation of the parent class (if it does 
     * not inherit directly from acscomponent::ACSComponentImpl 
     * or baci:: CharacteristicComponentImpl) it might be
     * necessary to call the cleanuUp() method of the base class 
     * AFTER having released resources allocated by  the current class.
     * For an example, see the FridgeControl class
     * Always check the documentation of the parent class
     * and consider what resources are allocated by this class
     * to extablish the requirements for the execution of lifecycle
     * chained methods.
     *
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
     virtual void cleanUp();
    
  private:
    /**
     *  m_version_sp is a string which shows the version of class Building being used.
     */
     baci::SmartPropertyPointer<baci::ROstring>m_version_sp;
  
    /**
     *  The door that belongs to this building.
     */
    acsexmplBuilding::Door_var m_door_p;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const Building&);
};
/*\@}*/
/*\@}*/

#endif

