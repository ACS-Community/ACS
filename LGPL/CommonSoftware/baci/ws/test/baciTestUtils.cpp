/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
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
* "@(#) $Id: baciTestUtils.cpp,v 1.93 2005/04/20 13:31:46 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2001-10-19 created
*/

static char *rcsId="@(#) $Id: baciTestUtils.cpp,v 1.93 2005/04/20 13:31:46 gchiozzi Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <vltPort.h>
#include <acsutil.h> 
#include <baciTestUtils.h>
#include <ace/Thread_Manager.h>

int write_iors_to_file (const int count, 
			CORBA::String_var name[], 
			CORBA::String_var type[], 
			CORBA::String_var ior[])
{
    const char* ior_output_file = "iors.dat";
    
    FILE *output_file = ACE_OS::fopen (ior_output_file, "w");
    
    if (output_file == 0)
	{
	ACS_SHORT_LOG((LM_ERROR, "Cannot open output files for writing IORs: %s",
		       ior_output_file));
	return -1;
	}
    
    u_int result;
    for (int i=0; i < count; i++) 
	{
	result = ACE_OS::fprintf (output_file, "%s %s %s\n", 
				  name[i].in(), 
				  type[i].in(), 
				  ior[i].in());
	
	if (result != (
		ACE_OS::strlen (name[i].in()) + 1 +
		ACE_OS::strlen (type[i].in()) + 1 +
		ACE_OS::strlen (ior[i].in())  +
		ACE_OS::strlen ("\n")))
	    {
	    ACS_SHORT_LOG ((LM_ERROR,
			    "ACE_OS::fprintf failed while writing IORs to file %s",
			    ior_output_file));
	    return -1;
	    }   
	
	}
    ACE_OS::fclose (output_file);
    return 0;
}

int read_IOR_from_file (std::string device, std::string &readIOR)
{
    const char* ior_input_file = "iors.dat";
    
    // Open the file for reading.
    FILE *input_file = ACE_OS::fopen (ior_input_file, "r");
    
    if (input_file == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,
		       "Unable to open %s for reading",
		       ior_input_file));
	return -1;
	}
    
    char  name[256];
    char  type[256];
    char  ior[2048];
    
    ACS_SHORT_LOG((LM_INFO,"baciTestClient: Reading file")); 
    
    while( fscanf (input_file, "%s %s %s\n", name, type, ior) != 0)
	{
        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Looking at %s", name)); 
	if (ior == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,
			   "Unable to read ior"));
	    return -1;
	    }
        if(device.compare(name)==0)
	    {
	    ACS_SHORT_LOG((LM_INFO,"baciTestClient: Found"));
	    readIOR = ior;
	    break;
	    }
	}
    
    ACE_OS::fclose(input_file); 
    ACS_SHORT_LOG((LM_INFO,"baciTestClient: Input file closed")); 
    return 0;
    
} /* end read_IOR_from_file */


static void worker(void* proc)
{
    (*((CORBAShutdown::ShutdownFunction)proc))();
}


CORBAShutdown::ShutdownFunction CORBAShutdown::m_func = 0;

void
CORBAShutdown::setShutdownFunction(ShutdownFunction function)
{
    m_func = function;
}

void
CORBAShutdown::shutdown()
{
    if (!m_func)
	return;

    ACE_OS::sleep(1);

    ACE_Thread_Manager *thr_mgr = ACE_Thread_Manager::instance ();
    thr_mgr->spawn (ACE_THR_FUNC (worker), (void*)m_func);

}

    
