#ifndef baciTestUtils_h
#define baciTestUtils_h

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
* "@(#) $Id: baciTestUtils.h,v 1.104 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-06-06 added baciTestCBpattern
* msekoran 2002-02-07 Added ComponentAShutdown class.
* gchiozzi 2001-10-19 Created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

/** 
 * @file 
 * Header file for BACI Test Utils.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <tao/corba.h>
#include <ace/Read_Buffer.h>

#include <string>
#include <baci.h>

class baciTestCBvoid: public virtual POA_ACS::CBvoid
{
  public:
    baciTestCBvoid(std::string _prop) : prop(_prop) {}
 
    virtual void working (const ACSErr::Completion & c,
		  const ACS::CBDescOut & desc
		  )
	{
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBvoid::working)", prop.c_str()));
	}
 
    virtual void done (const ACSErr::Completion & c,
	       const ACS::CBDescOut & desc
	        )
	{
	    ACS_SHORT_LOG ((LM_INFO, "(%s::CBvoid::done)", prop.c_str()));
	}
 
    virtual CORBA::Boolean negotiate (ACS::TimeInterval time_to_transmit,
                              const ACS::CBDescOut & desc
                              )
	{
	    return true;
	}

  protected:
    std::string prop;
 
}; /* end baciTestCBvoid */ 

class baciTestCBdouble: public virtual POA_ACS::CBdouble 
{

  public:
    baciTestCBdouble(std::string _prop, int c=5, int dc=1) : prop(_prop), count(c), done_c(dc) {}

    virtual void working (
	CORBA::Double value,
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	)
	{
	    if (count>0)
		{
		CompletionImpl completion(c);
		if (completion.isErrorFree())
		    {
		    ACS_SHORT_LOG((LM_INFO, "(%s::CBdouble::working) desc.id_tag: %u Value: %f TimeStamp: %s", 
				   prop.c_str(), (unsigned)desc.id_tag, value, getStringifiedUTC(c.timeStamp).c_str()));
		    }
		else
		    {
		    ACS_SHORT_LOG((LM_INFO, "(%s::CBdouble::working) desc.id_tag: %u TimeStamp: %s containes an error", prop.c_str(), (unsigned)desc.id_tag, getStringifiedUTC(c.timeStamp).c_str()));
		    completion.log();
		    }//if-else
		}
	    count--;
	}

    virtual void done (
	CORBA::Double value,
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	)
	{	    
	    if (done_c > 0 )
		{
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBdouble::done) desc.id_tag: %u Value: %f TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, value, getStringifiedUTC(c.timeStamp).c_str()));
		}//if
	    done_c--;
	}


    virtual CORBA::Boolean negotiate (
	ACS::TimeInterval time_to_transmit,
	const ACS::CBDescOut & desc
	)
	{ 
	    return 1; 
	}

    int getCount(){ return count+done_c;}
  protected:
    std::string prop;
    int count, done_c;

}; /* end baciTestCBdouble */ 

class baciTestCBfloat: public virtual POA_ACS::CBfloat
{

  public:
    baciTestCBfloat(std::string _prop, int c=5, int dc=1) : prop(_prop), count(c), done_c(dc) {}

    virtual void working (
	CORBA::Float value,
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	)
	{
	    if (count>0)
		{
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBfloat::working) desc.id_tag: %u Value: %f TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, value, getStringifiedUTC(c.timeStamp).c_str()));
		}
	    count--;
	}

    virtual void done (
	CORBA::Float value,
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	)
	{	    
	    if (done_c > 0 )
		{
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBfloat::done) desc.id_tag: %u Value: %f TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, value, getStringifiedUTC(c.timeStamp).c_str()));
		}//if
	    done_c--;
	}


    virtual CORBA::Boolean negotiate (
	ACS::TimeInterval time_to_transmit,
	const ACS::CBDescOut & desc
	)
	{ 
	    return 1; 
	}

    int getCount(){ return count+done_c;}
  protected:
    std::string prop;
    int count, done_c;

}; /* end baciTestCBfloat */ 

class baciTestCBpattern: public virtual POA_ACS::CBpattern 
{

  public:
    baciTestCBpattern(std::string _prop, int c=5, int dc=1) : prop(_prop), count(c), done_c(dc) {}

    virtual void working (
	ACS::pattern value,
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	)
	{
	    if (count>0)
		{
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBpattern::working) desc.id_tag: %u Value: %llu TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, value, getStringifiedUTC(c.timeStamp).c_str()));
		}
	    count--;
	}

    virtual void done (
	ACS::pattern value,
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	)
	{	    
	    if (done_c > 0 )
		{
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBpattern::done) desc.id_tag: %u Value: %llu TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, value, getStringifiedUTC(c.timeStamp).c_str()));
		}//if
	    done_c--;
	}


    virtual CORBA::Boolean negotiate (
	ACS::TimeInterval time_to_transmit,
	const ACS::CBDescOut & desc
	)
	{ 
	    return 1; 
	}

    int getCount(){ return count+done_c;}
  protected:
    std::string prop;
    int count, done_c;

}; /* end baciTestCBdouble */ 

int write_iors_to_file (const int count, 
			CORBA::String_var name[], 
			CORBA::String_var type[], 
			CORBA::String_var ior[]);
int read_IOR_from_file (std::string device, std::string &readIOR);

class CORBAShutdown
{
  public:
    typedef void (*ShutdownFunction)();

    static void setShutdownFunction(ShutdownFunction function);
    static void shutdown();

  private:
    static ShutdownFunction m_func;
    
};

#endif   /* baciTestUtils_h */




