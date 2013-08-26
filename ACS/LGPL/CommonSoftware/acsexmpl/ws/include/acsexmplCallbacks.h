#ifndef acsexmplCallbacks_h
#define acsexmplCallbacks_h
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2004 
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
* "@(#) $Id: acsexmplCallbacks.h,v 1.8 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dave  2004-01-13  created 
*/

//Since this header just contains implementation of BACI classes, we must 
//include the CORBA stubs for baci.idl
#include <baciS.h>

//ACE's string class which is supported on tons of different platforms.
#include <ace/SString.h>

/** @file acsexmplCallbacks.h
 */


/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOMISCELLANEOUS
*/
/*@{
*/

/** @defgroup ACSEXMPLCALLBACKSDOC BACI Callback and Alarm Implementation
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 This header defines a number of BACI callback and alarm implementation classes that
 are used throughout the clients of the acsexmpl module. Any developer interested 
 in invoking asynchronous methods or creating monitors should definitely review 
 this section.
 <br>
 <br>
 <h2>What can I gain from this example?</h2>
 <ul>
   <li>implementations of CORBA servants derived from the OffShoot IDL interface.</li>
   <li>implementations of callbacks used to invoke asynchronous methods (e.g., MyCBvoid).</li>
   <li>implementations of callbacks used to obtain the value of a BACI property asynchronously (e.g., MyCBdouble).</li>
   <li>implementations of callbacks used to monitor the value of a BACI property (e.g., MyCBdouble).</li>
   <li>implementations of alarms set to go off when a BACI property's value goes out of range (e.g., MyAlarmdouble).</li>
 </ul>
 <br>
 <br>
 <h2>Links</h2>
 <ul>
   <li><a href="classCommonCallback.html">Common Callback Functionality Class Reference</a></li>
   <li><a href="classMyCBvoid.html">My Callback Void Class Reference</a></li>
   <li><a href="interfaceACS_1_1CBvoid.html">Callback Void IDL Documenation</a></li>
   <li><a href="classMyCBdouble.html">My Callback Double Class Reference</a></li>
   <li><a href="interfaceACS_1_1CBdouble.html">Callback Double IDL Documenation</a></li>
   <li><a href="classMyAlarmdouble.html">My Alarm Double Class Reference</a></li>
   <li><a href="interfaceACS_1_1AlarmDouble.html">Alarm double IDL Documenation</a></li>
 </ul>
 <br>
 <br>
 </div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

/**
 * Used to provide common functionality for the implementations of BACI 
 * callbacks and alarms.  Really all this class does is keep track of the number
 * of done invocations as well as the name of the BACI property it's monitoring, receiving
 * an asynchronous value from, etc.
 * 
 * @version "@(#) $Id: acsexmplCallbacks.h,v 1.8 2008/10/01 04:30:47 cparedes Exp $"
 */
class CommonCallback
{
  public:
    /**
     * Standard constructor
     */
    CommonCallback() {}
    
  protected:
    /**
     * 99% of the time, callback are used in conjunction with BACI properties.
     * This member is just the name of that property.
     */
    ACE_CString prop;
    
    /**
     * To keep the acsexmpl modular test deterministic, we keep track of the number
     * of times the done method is invoked.  From this value the logging priority
     * is dynamically changed under some circumstances.
     */
    unsigned int m_count;

  private:
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const CommonCallback&);
};
/////////////////////////////////////////////////////////////////////////
/**
 * This class is the implementation of the AlarmDouble IDL interface defined in baci.idl.
 * It is used so that we may create a local alarm for the value of a remote BACI 
 * property.  There are only two useful methods: alarm_raised and alarm_cleared.  They
 * do just what their names imply.
 * 
 * @version "@(#) $Id: acsexmplCallbacks.h,v 1.8 2008/10/01 04:30:47 cparedes Exp $"
 */
class MyAlarmdouble : public virtual POA_ACS::Alarmdouble,    //CORBA servant stub
		      protected CommonCallback
{
  public:
    /**
     * Constructor
     * @param _prop Name of this Alarmdouble instance
     */
    MyAlarmdouble(ACE_CString _prop) { prop = _prop; }
    
    /**
     * Destructor - nothing to delete.
     */
    ~MyAlarmdouble() {}

    /**
     * Method invoked when the double value goes out of range.
     * @param value The double's current (i.e., out of range) value
     * @param c Error handing structure.
     * @param desc Callback descriptor
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    alarm_raised (CORBA::Double value,
		  const ACSErr::Completion &c,
		  const ACS::CBDescOut &desc);
	
    /**
     * Method invoked when the double value goes back into the acceptable range.
     * @param value The double's new value
     * @param c Error handing structure.
     * @param desc Callback descriptor
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    alarm_cleared (CORBA::Double value,
		   const ACSErr::Completion &c,
		   const ACS::CBDescOut &desc);
	    
    /**
     * Method used so that client and servant can agree
     * upon the time it takes to transmit data (generally invocations
     * of the done method). A smart callback implementation would analize
     * the value of time_to_transmit and decide whether the value is acceptable,
     * but we just return true regardless in this simplistic example.
     * @param time_to_transmit Time to transmit data.
     * @param desc Callback descriptor
     * @return True regardless of parameter values.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Boolean 
    negotiate (ACS::TimeInterval time_to_transmit, const ACS::CBDescOut &desc) 
	{
	    return true;
	}

};
/////////////////////////////////////////////////////////////////////////
/**
 * This is the implementation of a callback class that will handle callbacks 
 * coming from the installed Monitor or asynchronous method invocation.
 */
class MyCBdouble : public virtual POA_ACS::CBdouble,    //CORBA servant stub
		   protected CommonCallback
{
  public:
    /**
     * Constructor
     * @param _prop Name of this CBdouble instance
     */
    MyCBdouble(ACE_CString _prop)  { prop = _prop; m_count = 0; }

    /**
     * Destructor - nothing to delete.
     */
    ~MyCBdouble() {}
    
    /**
     * Method invoked only within the context of a BACI monitor when a 
     * value changes or the timeout period has passed.
     * @param value The double value of the BACI property.
     * @param c Error handling structure.
     * @param desc Callback descriptor
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    working (CORBA::Double value, const ACSErr::Completion &c, const ACS::CBDescOut &desc);
    
    /**
     * Method invoked for the final value of a BACI monitor or when an asynchronous
     * method which returns a double is invoked.
     * @param value The double value we are interested in.
     * @param c Error handling structure.
     * @param desc Callback descriptor
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    done (CORBA::Double value, const ACSErr::Completion &c, const ACS::CBDescOut &desc);
    
    /**
     * Method used so that client and servant can agree
     * upon the time it takes to transmit data (generally invocations
     * of the done method). A smart callback implementation would analyze
     * the value of time_to_transmit and decide whether the value is acceptable,
     * but we just return true regardless in this simplistic example.
     * @param time_to_transmit Time to transmit data.
     * @param desc Callback descriptor
     * @return True regardless of parameter values.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Boolean 
    negotiate (ACS::TimeInterval time_to_transmit, const ACS::CBDescOut &desc) 
	{
	    return true;
	}
};
/////////////////////////////////////////////////////////////////////////
/**
 * This callback class is used so the caller of asynchronous methods can 
 * be informed when the method invocation has finally completed.
 */
class MyCBvoid: public virtual POA_ACS::CBvoid,    //CORBA servant stub
		protected CommonCallback
{   
  public:
    /**
     * Constructor
     * @param _prop Name of this CBvoid instance
     */
    MyCBvoid(ACE_CString _prop) { prop = _prop; }
    
    /**
     * Destructor - nothing to delete.
     */
    ~MyCBvoid() {}

    /**
     * Method invoked only to inform the developer of a client that the 
     * asynchronous method has not "forgot about it", but is still working
     * on processing the request.
     * @param c Error handling structure.
     * @param desc Callback descriptor
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    working (const ACSErr::Completion &c, const ACS::CBDescOut &desc); 
    
    /**
     * Method invoked to inform the developer of a client that the 
     * asynchronous method has completed.
     * @param c Error handling structure.
     * @param desc Callback descriptor
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    void 
    done (const ACSErr::Completion &c, const ACS::CBDescOut &desc); 

    /**
     * Method used so that client and servant can agree
     * upon the time it takes to transmit data (generally invocations
     * of the done method). A smart callback implementation would analyze
     * the value of time_to_transmit and decide whether the value is acceptable,
     * but we just return true regardless in this simplistic example.
     *
     * @param time_to_transmit Time to transmit data.
     * @param desc Callback descriptor
     * @return True regardless of parameter values.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    CORBA::Boolean 
    negotiate (ACS::TimeInterval time_to_transmit, const ACS::CBDescOut &desc) 
	{
	    return true;
	}
};
/////////////////////////////////////////////////////////////////////////

#endif
/*___oOo___*/



