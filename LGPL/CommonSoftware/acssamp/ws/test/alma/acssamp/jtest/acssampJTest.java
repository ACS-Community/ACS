////////////////////////////////////////////////////////////////////////////////
package alma.acssamp.jtest;
////////////////////////////////////////////////////////////////////////////////

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;



public class acssampJTest extends ComponentClient
{
    private acssampConsumer consumer;

    public acssampJTest(String managerLoc, String clientName)
	throws Exception
	{
	    super(null, managerLoc, clientName);

	    String ncChannel = "NC_LAMP1_brightness_1000000_10000000";

	    ContainerServices csrv = getContainerServices();
	    
	    consumer = new acssampConsumer(ncChannel, csrv);
	    try
		{
		//After consumerReady() is invoked, push_structured_event(...) is invoked
		//by the notification channel.  That is, we have no control over when
		//that method is called.
		consumer.consumerReady();
		System.out.println("Waiting for events...");
		}
	    catch(Exception e)
		{
		System.err.println(e);
		consumer.ncDisconnect();
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

		
		acssampJTest test = new acssampJTest(managerLoc, clientName);

		Thread.sleep(50000);
		    
		}
	    catch (Exception e)
		{
		e.printStackTrace(System.err);
		}
      
	}
}
