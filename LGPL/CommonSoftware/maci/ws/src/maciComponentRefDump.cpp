
#include <maciSimpleClient.h>

#include <ace/ARGV.h>
#include <ace/Arg_Shifter.h>
#include <ace/INET_Addr.h>

int main (int argc, char **argv)
{
	/// The simple client to connect to the component to test
	maci::SimpleClient client;
    if (argc <= 1)
    {
    	printf("Please give component name as first parameter\n");
    	return -1;
    }

	if (client.init(argc,argv) == 0)
	{
		return -1;
	}
	else
	{
		// Log into the manager before doing anything
		client.login();
	}

	try
	{

		CORBA::Object_var obj = client.getComponent(argv[1], 0, true);


		printf("\nIOR of %s is: %s\n",
				argv[1],
				client.getORB()->object_to_string(obj.ptr())
				);

		ACS_SHORT_LOG((LM_INFO,"Releasing %s", argv[1]));
		client.releaseComponent(argv[1]);
	}
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
		// Here we catch eventual exception
		_ex.log();
	}
	catch( CORBA::SystemException &_ex )
	{
		ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
				"main");
		corbaProblemEx.setMinor(_ex.minor());
		corbaProblemEx.setCompletionStatus(_ex.completed());
		corbaProblemEx.setInfo(_ex._info().c_str());
		corbaProblemEx.log();
	}//try-catch
};
