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
* "@(#) $Id: taskRunner.cpp,v 1.12 2006/11/29 23:09:51 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-09-10  created
* sharring 2005-03-22 modified to pass command-line parameters
*/

#include "taskStaticContainer.h"
#include "taskComponentS.h"
#include "taskErrType.h"
#include <ParamSetDef.h>
#include <vector>

using namespace maci;
using namespace Parameters;
using std::string;

int main(int argc, char *argv[])
{
    LoggingProxy logger(0, 0, 31);
    LoggingProxy::init(&logger);

    ACE_ARGV cleandArgv;
    StaticContainer staticContainer;

    ACE_CString libName,
	filePrefix,
	componentName,
	containerName;

   cleandArgv.add(argv[0]);

	vector<string> commandLineParamsVector;
   for(unsigned int i=1; i<(unsigned)argc; i++)
	{
	ACS_DEBUG_PARAM("taskRunner::main", "argv is %s \n", argv[i])
	if (ACE_OS::strcmp(argv[i], "-name") == 0)
	    {
	    componentName = argv[++i];
	    }
	else
	    {
	    if (ACE_OS::strcmp(argv[i], "-l") == 0)
		{
		libName=argv[++i];
		}
	    else
		{
		if (ACE_OS::strcmp(argv[i], "-c") == 0)
		    {
		    containerName=argv[++i];
		    }
		else
		    {
		    if (argv[i][0] != '-')
			{
			string currentArg(argv[i]);
			commandLineParamsVector.push_back(currentArg);
			}
		    else
			{
			cleandArgv.add(argv[i]);
			}
		    }
		}
	    }
	}//for

    if (commandLineParamsVector.size()==0)
	{
	ACE_OS::printf("parameter to be passed to the run method of the component not specified\n");
	return -1;
	}
  
    try
	{
	staticContainer.init(cleandArgv.argc(), cleandArgv.argv(), const_cast<char*>(containerName.c_str()));

	ACS_DEBUG(argv[0], "Creating component");
	CORBA::Object_var comp;

	comp = staticContainer.createComponent(componentName.c_str(), libName.c_str());

	if ( comp.ptr() != CORBA::Object::_nil() )
	    {
	    ACS_DEBUG(argv[0], "Task component has been created");
	    }
	else
	    {
	    ACS_LOG(LM_RUNTIME_CONTEXT, argv[0],
		    (LM_ERROR, "task component couldn't be created"));
	    return -1;
	    }

	ACS::TaskComponent_var taskComponent;
	taskComponent = ACS::TaskComponent::_narrow(comp.in());

	if (taskComponent.in() != ACS::TaskComponent::_nil())
	{
		// executing command
		try
		{
			ACS_DEBUG(argv[0], "Invoking run operation on the component");
			ACS::StringSequence commandLineArgSeq;
			commandLineArgSeq.length(commandLineParamsVector.size());
			for(unsigned int i = 0; i < commandLineParamsVector.size(); i++) 
			{
				commandLineArgSeq[i] = CORBA::string_dup(commandLineParamsVector.at(i).c_str());
				ACS_DEBUG_PARAM("taskRunner::main", "sequence element: %s \n", commandLineParamsVector.at(i).c_str())
			}
          
			ACS_DEBUG_PARAM("taskRunner::main", "length of commandline param sequence: %d \n", commandLineArgSeq.length())

			// remove the leading ./ if it exists in the argv[0] (i.e. the name of the command or executable)
			filePrefix = ACE_CString(argv[0]);
			ssize_t locationOfDotSlash = filePrefix.find("./");
			ACE_CString preppedFilePrefix = filePrefix;
			if(0 == locationOfDotSlash) {
				preppedFilePrefix = filePrefix.substr(2);
			}

			// invoke the task's run method
			taskComponent->run(commandLineArgSeq, preppedFilePrefix.c_str());
		}
		catch(taskErrType::TaskRunFailureEx &rex)
		{
			ACE_OS::printf("operation exited with TaskRunFailure exception. Log:\n");
			taskErrType::TaskRunFailureExImpl ex(rex);
			ex.log();
		}
		catch(ACSErr::ACSbaseExImpl &ex)
		{
			ACE_OS::printf("operation exited with an ACS exception. Log:\n");
			ex.log();
		}
		catch(CORBA::Exception &ex)
		{
			ACE_OS::printf("operation exited with CORBA exception\n");
			ex._tao_print_exception ("occured");
		}
		catch(...)
		{
			ACE_OS::printf("operation exited with an unknown exception\n");
		}

		ACS_DEBUG(argv[0], "command run has been invoked");
	}
	else
	    {
	    ACS_LOG(LM_RUNTIME_CONTEXT, argv[0],
		    (LM_ERROR, "component is not a task (it does not derive from ACS::Task interface)"));
	    }
	
	ACS_DEBUG(argv[0], "Destroying the task component");
	staticContainer.destroyComponent(comp.in());
	ACS_DEBUG(argv[0], "Task Component destroyed");
	staticContainer.done();
    }
    catch(...)
	{
	  printf("taskRunner unknown exception occured\n");
	  return -1;
	}
    return 0;

}

