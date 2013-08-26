#ifndef _ACS_BASE_EX_IML_H_
#define _ACS_BASE_EX_IML_H_
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
* "@(#) $Id: acserrACSbaseExImpl.h,v 1.10 2009/02/05 09:29:07 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-02-13  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acserr.h"

namespace ACSErr 
{

/**
 *class ErrorTraceForBaseEx
 * is just continer for errotrace. It is here because errotrace of baseX has to beinitalized before its referenc is sent to ErrorTraceHelper
 */

class ErrorTraceForBaseEx
{
  protected:
    ErrorTraceForBaseEx();
    ErrorTraceForBaseEx(ACSErr::ErrorTrace &errortrace) :
	m_errorTrace (errortrace){} // ??? can be removed when errotrace is just in baseEx !!!
    ErrorTraceForBaseEx(const ACSErr::ErrorTrace &errortrace) :
	m_errorTrace (errortrace){} 

   ErrorTrace m_errorTrace;  
};

/**
 this class shall not be instantiated by user but only bt the derived classes
*/

class ACSbaseExImpl : public ErrorTraceForBaseEx, public ErrorTraceHelper
{
  protected:

// create new error trace
    ACSbaseExImpl (ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
		   const char* file, int line, const char* routine, const char* sd,
		   ACSErr::Severity severity);

// adding previos error trace
    ACSbaseExImpl (const ACSErr::ErrorTrace &pet,
		   ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
		   const char* file, int line, const char* routine, const char* sd, 
		   ACSErr::Severity severity);


// for remote exceptions
    ACSbaseExImpl(const ACSErr::ErrorTrace &et);

    ACSbaseExImpl(ACSErr::ErrorTrace &et);

// for OK cases and wrapping completion
    ACSbaseExImpl(ACSErr::ErrorTrace &et, int depth);

  public:

    /**
       convertion to remote ACSbaseEx exception
     */
    ACSbaseEx getACSbaseEx() { return ACSbaseEx(m_errorTrace); }
    
    /**
     * copy constructor
     */
    ACSbaseExImpl (const ACSbaseExImpl &t);

    /**
     * assignment operator
     */
    ACSbaseExImpl& operator=(const ACSbaseExImpl &t);

};

}; //namespace ACSErr

template <class T>
class ETHolder
{
  public:
    ETHolder(const CORBA::UserException &ex) :
	m_errorTrace (((T&)ex).errorTrace) {}
       

    ETHolder(const CompletionImpl &c) : 
	m_errorTrace( const_cast<CompletionImpl&>(c).isErrorFree() ? this->emptyErrorTrace : const_cast<ACSErr::ErrorTrace&>(c.previousError[0])) {}
/*const_cast<CompletionImpl&>(c).getErrorTraceHelper()->getErrorTrace()*/

	
    /*   this will be used later */
    
    ETHolder(const ACSErr::ACSbaseExImpl &ex) : 
	m_errorTrace( const_cast<ACSErr::ACSbaseExImpl&>(ex).getErrorTrace() ) {}

    ETHolder(ACSErr::ACSbaseExImpl &ex) : 
	m_errorTrace( ex.getErrorTrace() ) {}

    ACSErr::ErrorTrace& getErrorTrace() const { return m_errorTrace; } 

  private:
    ETHolder(const ETHolder<T>&) {}

    ACSErr::ErrorTrace &m_errorTrace;
    ACSErr::ErrorTrace emptyErrorTrace;
};

#endif /*!_H*/

