/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: acserrACSbaseExImpl.cpp,v 1.5 2004/04/30 07:10:55 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-02-17  created 
*/

static char *rcsId="@(#) $Id: acserrACSbaseExImpl.cpp,v 1.5 2004/04/30 07:10:55 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acserrACSbaseExImpl.h"

using namespace ACSErr;

/*
Implemantation of ACSbaseExImpl is done in such a way because of BKWD compaitibilty. It manges error trace which it gets as a parameter from subclass. This has to be removed. Parts of constructors are copied from acserr.cpp (ErrorTraceHelper)
 */

ACSbaseExImpl::ACSbaseExImpl(const ACSErr::ErrorTrace &errortrace) : 
    ErrorTraceForBaseEx(const_cast<ACSErr::ErrorTrace&>(errortrace)), ErrorTraceHelper (m_errorTrace)
{} // init error trace with what it gets from subclass and pass own reference to ErrorTraceHelper

ACSbaseExImpl::ACSbaseExImpl(ACSErr::ErrorTrace &errortrace) : 
    ErrorTraceForBaseEx(errortrace), ErrorTraceHelper (m_errorTrace)
{} // init error trace with what it gets from subclass and pass own reference to ErrorTraceHelper

// for OK cases and wrapping completion
ACSbaseExImpl::ACSbaseExImpl(ACSErr::ErrorTrace &errortrace, int depth) : 
    ErrorTraceForBaseEx(errortrace), ErrorTraceHelper (m_errorTrace, depth)
{}

// create new error trace
ACSbaseExImpl::ACSbaseExImpl (ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
		      const char* file, int line, const char* routine, const char* sd,
		      ACSErr::Severity severity):
    ErrorTraceHelper (et, ec, file, line, routine, sd, severity, m_errorTrace)
{}

// adding previos error trace
ACSbaseExImpl::ACSbaseExImpl (const ACSErr::ErrorTrace &pet,
			      ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
			      const char* file, int line, const char* routine, const char* sd, 
			      ACSErr::Severity severity): 
    ErrorTraceHelper (pet, et, ec, file, line, routine, sd, severity, m_errorTrace)
{}

ACSbaseExImpl::ACSbaseExImpl (const ACSbaseExImpl &t) :
    ErrorTraceForBaseEx(t.m_errorTrace),
    ErrorTraceHelper (m_errorTrace)
{}

ACSbaseExImpl& ACSbaseExImpl::operator=(const ACSbaseExImpl &t)
{
    if (this != &t )
	{
	m_errorTraceRef =  m_errorTrace;
	m_current = &m_errorTraceRef;
	}
    return *this;
}

/*___oOo___*/
