#ifndef acsexmplAmsSeqImpl_h
#define acsexmplAmsSeqImpl_h
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
* "@(#) $Id: acsexmplAmsSeqImpl.h,v 1.103 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-10-04 removed *m_controlLoop_p and also ActionFunction* because they weren't being used
* david 2002-10-03 added inheritance from ACSDO and removed methods/members as a result
* david 2002-07-02 added GNU license header
* gchiozzi 2001-02-15 Added declaration of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created created standard header
* msekoran 2001-03-10 integrated with new BACI; ALMA coding convention used; doc.
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>

///CORBA generated servant stub
#include <acsexmplAmsSeqS.h>

///Includes for each BACI property used in this example
#include <baciRWdoubleSeq.h>
#include <baciROdoubleSeq.h>


/** @file acsexmplAmsSeqImpl.h
 */


/** @defgroup ACSEXMPLTOC ACS C++ Examples Documentation
 * @{
 * @htmlonly
  @endhtmlonly
*/

/** @defgroup ACSEXMPLTOCCOMPONENTS ACS Components Examples
 * @{
 * @htmlonly
  @endhtmlonly
*/

/** @defgroup ACSEXMPLAMSSEQDOC Antenna Mount System Sequence
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 AmsTestSeq can be considered to be a "real-world" distributed object
 for antenna systems.&nbsp; This component exists between two sections of the
 antenna mount system.&nbsp; The high-level section executes at the array
 central control area in the Array Control Computer (ACC), while the low-level
 section executes at each antenna in the Antenna Bus Master (ABM) computer.&nbsp;
 AmsTestSeq provides one synchronous method, setCoeff(...), and two properties
 used for the coefficient of an individual pointing model term.&nbsp; Essentially
 this device is used to manipulate pointing models.
 <br>
 <br>
 <h2>What can I gain from this example?</h2>
 <ul>
   <li>an example derived from the ACS::CharacteristicComponent IDL interface.</li>
   <li>overriding component lifecycle methods (see execute).</li>
   <li><b>read-only and read-write property sequence</b> usage.</li>
   <li>creating instances of CORBA sequences and manipulating them using the automatically generated _var type(s).</li>
   <li>writing values to read-only BACI properties by using the property's underlying DevIO instance.</li>
   <li>standard ACS logging macros.</li>
   <li>limited CORBA error handling.</li>
 </ul>
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classAmsTestSeq.html">AmsSeq Class Reference</a></li>
 <li><a href="interfaceAMSSEQ_1_1AmsTestSeq.html">AmsSeq IDL Documenation</a></li>
 <li>AmsSeq CDB XML Schema</li>
 </ul>
 <br>
 <br>
 </div>
   @endhtmlonly
 * @}
 */

/**
 *  Antenna Mount System Pointing Model.
 *  Class AmsTestSeq can be considered to be a "real-world" distributed object for antenna
 *  systems.  This class exists between two sections of the antenna mount system.  The 
 *  high-level section executes at the array central control area in the Array 
 *  Control Computer (ACC), while the low-level section executes at each 
 *  antenna in the Antenna Bus Master (ABM) computer.  AmsTestSeq provides one synchronous 
 *  method, setCoeff(...), and two properties used for the coefficient of an individual 
 *  pointing model term.  An important thing to note is this example shows usage of property
 *  sequence types (i.e., ROdoubleSeq and RWdoubleSeq).
 *
 *  @version "@(#) $Id: acsexmplAmsSeqImpl.h,v 1.103 2008/10/09 08:41:11 cparedes Exp $"
 */
class AmsTestSeq: public baci::CharacteristicComponentImpl,     //Standard component superclass
		  public virtual POA_AMSSEQ::AmsTestSeq    //CORBA servant stub
{
    
  public:
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components 
     * @param name component name
     */
    AmsTestSeq (
		const ACE_CString &name, 
		maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~AmsTestSeq();
    
    /* --------------------- [ CORBA interface ] ----------------------*/
    /**
     * Sets the coefficients of the pointing model.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */     
    virtual void 
    setCoeff ();
    
    /**
     * Returns a reference to the RWdoubleSeqPM property
     * Implementation of IDL interface for the property.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::RWdoubleSeq_ptr 
    RWdoubleSeqPM ();
    
    /**
     * Returns a reference to the ROdoubleSeqPM property
     * Implementation of IDL interface for the property. 
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROdoubleSeq_ptr 
    ROdoubleSeqPM ();

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
    
  private:    
    /**
     * m_ROdoubleSeqPM_p is all the coefficients currently set.
     */
    baci::RWdoubleSeq *m_RWdoubleSeqPM_p;

    /**
     * m_RWdoubleSeq_p is all the coefficients that the client can set.
     */
    baci::ROdoubleSeq *m_ROdoubleSeqPM_p;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const AmsTestSeq&);
};
/*\@}*/
/*\@}*/

#endif   /* acsexmplAmsSeqImpl_h */


