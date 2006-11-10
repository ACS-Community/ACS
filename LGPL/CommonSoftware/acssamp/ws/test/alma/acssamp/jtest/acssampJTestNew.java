////////////////////////////////////////////////////////////////////////////////
package alma.acssamp.jtest;
////////////////////////////////////////////////////////////////////////////////

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;



public class acssampJTestNew extends ComponentClient
{
    private acssampConsumerNew consumer;

    public acssampJTestNew(String managerLoc, String clientName)
	throws Exception
	{
	    super(null, managerLoc, clientName);
	    String ncChannel = "NC_LAMP1_1000000_10000000";
	    ContainerServices csrv = getContainerServices();
	    
	    consumer = new acssampConsumerNew(ncChannel, csrv);
	    try
		{
		//After consumerReady() is invoked, push_structured_event(...) is invoked
		//by the notification channel.  That is, we have no control over when
		//that method is called.
		consumer.ready();
		System.out.println("Waiting for events...");
		}
	    catch(Exception e)
		{
		System.err.println(e);
		consumer.disconnect();
		}
	}

    public static void main(String[] args)
	{
	    String managerLoc = System.getProperty("ACS.manager");
	    if (managerLoc == null)
		{
		System.out.println("Java property 'ACS.manager' " + 
				   " must be set to the corbaloc of the ACS manager!");
		System.exit(-1);
		}
	    
	    String clientName = "acssampJTest1";
	    double d;
	    try
		{
		acssampJTestNew test = new acssampJTestNew(managerLoc, clientName);
		for(long i = 0; i < 100000000; i++)
		    d = i + i - (i/2.4);
		}
	    catch (Exception e)
		{
		e.printStackTrace(System.err);
		}
      
	}
}
