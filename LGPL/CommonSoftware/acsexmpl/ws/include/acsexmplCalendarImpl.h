#ifndef acsexamplCalendarImpl_h
#define acsexamplCalendarImpl_h
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
*
* "@(#) $Id: acsexmplCalendarImpl.h,v 1.98 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use smart pointers for properties
* msekoran 2002-07-05 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>

///CORBA generated servant stub
#include <acsexmplCalendarS.h>

///Includes for each BACI property used in this example
#include <baciRWpattern.h>
#include <enumpropROImpl.h>
#include <enumpropRWImpl.h>

///Include the smart pointer for the properties
#include <baciSmartPropertyPointer.h>

/** @file acsexmplCalendarImpl.h
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/
/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/
/** @defgroup ACSEXMPLCALENDARDOC Calendar
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
Calendar shows the proper usage of enumerated and pattern properties.
&nbsp;It provides two read-write enums, one read-only enum,
and a read-write pattern. &nbsp;There are no methods so this component
should only be used through objexp (e.g., a graphical user interface designed
specifically for manipulating components).
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::CharacteristicComponent IDL interface.</li>
  <li>usage of enums properties via the ACS enum template.</li>
  <li>usage of RW pattern properties.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classCalendar.html">Calendar Class Reference</a></li>
  <li><a href="interfaceacsexmplCalendar_1_1Calendar.html">Calendar IDL Documentation</a></li>
  <li>Calendar CDB XML Schema</li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/** 
 *  Simulates the behavior of a calendar using enums and pattern properties.
 *  This is an example component which includes enums and pattern types. It represents a 
 *  simple calendar. Calendar provides no methods, but it has four enum/pattern 
 *  properties.
 *
 *  @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 *  Jozef Stefan Institute, Slovenia<br>
 *  @version "@(#) $Id: acsexmplCalendarImpl.h,v 1.98 2008/10/01 04:30:47 cparedes Exp $"
 */
class Calendar: public baci::CharacteristicComponentImpl,     //Standard component superclass
		public virtual POA_acsexmplCalendar::Calendar    //CORBA servant stub
{  
  public:
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components
     * @param name component name
     */
    Calendar( 
	     const ACE_CString &name,
	     maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~Calendar();
    
    /* --------------------- [ CORBA interface ] ----------------------*/
    /**
     * Returns a reference to the day property
     * Implementation of IDL interface for the property.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ::acsexmplCalendar::RWDaysEnum_ptr 
    day ();
    
    /**
     * Returns a reference to the month property
     * Implementation of IDL interface for the property.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ::acsexmplCalendar::RWMonthEnum_ptr
    month ();
    
    /**
     * Returns a reference to the yearAttributes property
     * Implementation of IDL interface for the property.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ::ACS::RWpattern_ptr 
    yearAttributes ();
    
    /**
     * Returns a reference to the state property
     * Implementation of IDL interface for the property.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ::acsexmplCalendar::ROStateEnum_ptr 
    state ();
    
  private:
    /**
     *  m_yearAttributes_sp is the calendar's year "attribute"
     */
    baci::SmartPropertyPointer<baci::RWpattern> m_yearAttributes_sp;
    
    /**
     *  m_day_sp represents the day of the year.
     */
    baci::SmartPropertyPointer<
    	RWEnumImpl<ACS_ENUM_T(acsexmplCalendar::DaysEnum),  POA_acsexmplCalendar::RWDaysEnum>
    > m_day_sp;

    /**
     *  m_month_sp represents the month of the year.
     */
    baci::SmartPropertyPointer<
    	RWEnumImpl<ACS_ENUM_T(acsexmplCalendar::MonthEnum), POA_acsexmplCalendar::RWMonthEnum>
    > m_month_sp;
    
    /**
     *  m_state_sp represents how good are we with the plan this year...
     */
    baci::SmartPropertyPointer<
    	ROEnumImpl<ACS_ENUM_T(acsexmplCalendar::StateEnum), POA_acsexmplCalendar::ROStateEnum>
    > m_state_sp;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const Calendar&);
};
/*\@}*/
/*\@}*/

#endif  

