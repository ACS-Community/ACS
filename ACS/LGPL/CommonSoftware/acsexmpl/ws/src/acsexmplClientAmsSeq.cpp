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
* "@(#) $Id: acsexmplClientAmsSeq.cpp,v 1.9 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
*
* david     2002-06-13  Added a check for client.int()
*/

/** @file acsexmplClientAmsSeq.cpp
 *  @htmlonly
 *  <br><br>
    @endhtmlonly
 *  @param "component name" Use this required parameter to specify which AMS object
 *  should be activated.
 *  @param "pointing model values" Use this required parameter to specify which 12 values the pointing model should become.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 *  @param "-ORBEndpoint iiop://yyy:xxxx" Use this optional parameter to specify which host/port SimpleClient
 *  should run on.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 *  @param "-m corbaloc::yyy:xxxx/Manager" Use this optional parameter to specify where
 *  manager is.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 */  


/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCLIENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLCLIENTAMSSEQDOC Client AmsSeq
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This example shows a client that:
<ul>
  <li>logs into manager</li>
  <li>gets a reference to an Antenna Mount Sequence component specified from the command-line</li>
  <li>takes a read-only double sequence property from the component and prints out its values</li>
  <li>sets a read-write double sequence property and then invokes an IDL method</li>
  <li>logs out of manager</li>
</ul>
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Manipulation of BACI sequence properties.</li>
  <li>ACS logging mechanisms.</li>
  <li>Accessing (remote) components.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientAmsSeq_8cpp.html">AmsSeq Client File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

#include <maciSimpleClient.h>
#include <ACSErrTypeOK.h>
#include <acsexmplAmsSeqC.h>


ACE_RCSID(acsexmpl, acsexmplAmsSeqClient, "$Id: acsexmplClientAmsSeq.cpp,v 1.9 2007/02/01 05:14:26 cparedes Exp $")
using namespace maci;

/*******************************************************************************/
/** @cond
*/    

int main(int argc, char *argv[])
{
    //Checks command-line arguments.
    if (argc < 2)
	{
	ACS_SHORT_LOG((LM_INFO, "Usage: %s <component name> <pointing model double values> <options>", argv[0]));
	return -1;
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO, "Welcome to %s!", argv[0]));
	}
 
    //Creates and initializes the SimpleClient object
    SimpleClient client;   
    if(client.init(argc, argv)==0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot initialize client"));
	return -1;
	}
    else
	{
	client.login();
	}

    try
	{
	//get a reference to the component
	AMSSEQ::AmsTestSeq_var amsseq = client.getComponent<AMSSEQ::AmsTestSeq>(argv[1], 0, true);
	

	//Get the read-only sequence property from the component reference
	ACS::ROdoubleSeq_var ROdoubleSeqPM_p = amsseq->ROdoubleSeqPM();
	if (ROdoubleSeqPM_p.ptr() != ACS::ROdoubleSeq::_nil())
	    {
	    //synchronously reading the value of PM
	    ACSErr::Completion_var completion;
	    ACS::doubleSeq_var val_value = ROdoubleSeqPM_p->get_sync(completion.out());
	    
	    if (val_value.ptr() == 0)
		{
		ACS_SHORT_LOG((LM_INFO,"acsexmplAmsSeqClient: .. null value returned."));
		}
	    else
		{
		ACS_SHORT_LOG((LM_INFO,"acsexmplAmsSeqClient: .. value is (length %u):", val_value->length()));
		for (CORBA::ULong i = 0; i < val_value->length(); i++)
		    {
		    ACS_SHORT_LOG((LM_INFO,"\t(%u): %f", i, val_value[i]));
		    }
		}
	    }
   
	//get the read-write sequence property from the component reference
	ACS::RWdoubleSeq_var m_RWdoubleSeqPM_p = amsseq->RWdoubleSeqPM();
	if (m_RWdoubleSeqPM_p.ptr() != ACS::RWdoubleSeq::_nil())
	    {
	    ACS::doubleSeq_var val_value = new ACS::doubleSeq;
	    ACSErr::Completion_var completion;
	    
	    //must explicitly allocate the length of sequences
	    val_value->length(12);
	    
	    //store the value of the pointing model locally first
	    for( CORBA::ULong i = 0 ; i < val_value->length() ; i ++ )
		{
		val_value[i] = atof(argv[i+2]);
		}
	    
	    if (val_value.ptr() == 0)
		{
		ACS_SHORT_LOG((LM_INFO,"acsexmplAmsSeqClientSet: .. null value set."));
		}
	    else
		{
		ACS_SHORT_LOG((LM_INFO,"acsexmplAmsSeqClientSet: .. new value to be set is (length %u):", val_value->length()));
		completion = m_RWdoubleSeqPM_p->set_sync(val_value.in());
		//some error may have occured...
		if(completion->type != ACSErr::ACSErrTypeOK)
		    {
		    ACS_SHORT_LOG((LM_INFO, "acsexmplAmsSeqClientSet: .. Error in set_sync."));
		    }
		}
	    
	    /* Read  back the value of PM */
	    ACS::doubleSeq_var read_value = m_RWdoubleSeqPM_p->get_sync(completion.out());
	    if (read_value.ptr() == 0)
		{
		ACS_SHORT_LOG((LM_INFO,"acsexmplAmsSeqClient: .. null value returned."));
		}
	    else
		{
		ACS_SHORT_LOG((LM_INFO,"acsexmplAmsSeqClient: .. value is (length %u):", val_value->length()));
		for (CORBA::ULong i = 0; i < read_value->length(); i++)
		    {
		    ACS_SHORT_LOG((LM_INFO,"\t(%u): %f", i, read_value[i]));
		    }
		}
	    } 
	
	ACS_SHORT_LOG((LM_INFO, "Binding value to ROdoubleSeq"));
	//simply set the coefficient
	amsseq->setCoeff();
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    catch (...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"main");
	uex.log();
	return -1;
	}    
    
    try
	{
	//must cleanly release the component and log out from manager	
	ACS_SHORT_LOG((LM_INFO,"Releasing..."));
	client.releaseComponent(argv[1]);
	client.logout();
	}
    catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"main");
	uex.log();
	return -1;
	}
    
    // sleep for 3 seconds so the modular test is deterministic
    ACE_OS::sleep(3);
    return 0;
}

/** @endcond
*/    
/*___oOo___*/




