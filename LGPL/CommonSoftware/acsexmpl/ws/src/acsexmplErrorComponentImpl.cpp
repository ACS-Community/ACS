/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 *
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
* "@(#) $Id: acsexmplErrorComponentImpl.cpp,v 1.1 2006/04/12 16:35:05 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/
 
#include <acsexmplErrorComponentImpl.h>
#include <ACSErrTypeCommon.h>
#include <ACSErrTypeOK.h>
#include <iostream>

ACE_RCSID(acsexmpl, acsexmplErrorComponentImpl, "$Id: acsexmplErrorComponentImpl.cpp,v 1.1 2006/04/12 16:35:05 gchiozzi Exp $")

/* ----------------------------------------------------------------*/
ErrorComponent::ErrorComponent( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::ErrorComponent::ErrorComponent");
}
/* ----------------------------------------------------------------*/
ErrorComponent::~ErrorComponent()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::ErrorComponent::~ErrorComponent");
    ACS_DEBUG_PARAM("::ErrorComponent::~ErrorComponent", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
void
ErrorComponent::displayMessage ()
    throw (CORBA::SystemException)
{
    std::cout << "Hello World" << std::endl; 
}
/* ----------------------------------------------------------------*/
void 
ErrorComponent::badMethod(CORBA::Short depth) 
    throw(CORBA::SystemException, ACSErrTypeCommon::GenericErrorEx)
{
    ACS_TRACE("::ErrorComponent::badMethod");
    try
	{
	// We decrement the depth, because we are going to add one
        // error here in any case.
	errorTrace(depth-1);
	}
    catch(ACSErrTypeCommon::GenericErrorExImpl &ex)
	{
	ACSErrTypeCommon::GenericErrorExImpl ex2(ex, 
						 __FILE__, __LINE__, 
						 "ErrorComponent::badMethod");
	ex2.setErrorDesc("Generated multi level exception");
	throw ex2.getGenericErrorEx();
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl ex2(__FILE__, __LINE__, 
						 "ErrorComponent::badMethod");
	ex2.setErrorDesc("Got unexpected exception");
	throw ex2.getGenericErrorEx();
	}
    
    /*
     * We should get here only if a depth<=1 was requested.
     */
    ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__, 
					    "ErrorComponent::badMethod");
    ex.setErrorDesc("An error trace with depth lower or equal to 1 was requested.");
    throw ex.getGenericErrorEx();
}

/* ----------------------------------------------------------------*/
ACSErr::Completion *ErrorComponent::returnCompletion(CORBA::Short depth) 
    throw (CORBA::SystemException)
{
    ACS_TRACE("::ErrorComponent::returnCompletion");
    CompletionImpl *er;

    if( depth <= 0 )
	{
	er = new ACSErrTypeOK::ACSErrOKCompletion();
	}
    else
	{
	try
	    {
	    errorTrace(depth);
	    }
	catch(ACSErrTypeCommon::GenericErrorExImpl &ex)
	    {
	    ACSErrTypeCommon::GenericErrorCompletion *erg = 
		new ACSErrTypeCommon::GenericErrorCompletion(ex, 
						     __FILE__, __LINE__, 
						     "ErrorComponent::returnCompletion");
	    erg->setErrorDesc("Put an error trace in returnCompletion");
	    er = erg;
	    }
	catch(...)
	    {
	    ACSErrTypeCommon::GenericErrorCompletion *erg = 
		new ACSErrTypeCommon::GenericErrorCompletion( 
						     __FILE__, __LINE__, 
						     "ErrorComponent::returnCompletion");
	    erg->setErrorDesc("Got unexpected exception");
	    er=erg;
	    }
	}

    return er->returnCompletion();
}

void
ErrorComponent::errorTrace(CORBA::Short depth) 
    throw (ACSErrTypeCommon::GenericErrorExImpl)
{
    ACS_TRACE("::ErrorComponent::errorTrace");

    /*
     * If depth is 1, we are at the bottom and 
     * we just have to throw an exception.
     * Going up the recursive chain this will be
     * atteched to all other exceptions
     */
    if(depth == 1)
	{
	ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__, 
						"ErrorComponent::errorTrace");
	ex.setErrorDesc("Bottom of error trace");
	throw ex;
	}
	    
    /*
     * If depth > 1, make a recursive call.
     * We will have to get back an exception with a trace with
     * a depth shorter by 1 element.
     */
    if(depth > 1)
	{
	try
	    {
	    errorTrace(depth-1);
	    }
	catch(ACSErrTypeCommon::GenericErrorExImpl &ex)
	    {
	    ACSErrTypeCommon::GenericErrorExImpl ex2(ex, 
						     __FILE__, __LINE__, 
						     "ErrorComponent::errorTrace");
	    ex2.setErrorDesc("Generated multi level exception");
	    throw ex2;
	    }
	catch(...) // This should never happen!!!!
	    {
	    ACSErrTypeCommon::GenericErrorExImpl ex2(__FILE__, __LINE__, 
						     "ErrorComponent::errorTrace");
	    ex2.setErrorDesc("Got unexpected exception");
	    throw ex2;
	    }
	}
    /*
     * We should get here only if depth <= 0,
     * I.e. if there is not exception to throw.
     */
    return;
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ErrorComponent)
/* ----------------------------------------------------------------*/


/*___oOo___*/



