/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2002 Copyright by ESO (in the framework of the ALMA collaboration), All
 * rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.demo.test.client;

import java.util.logging.Logger;
import alma.acs.component.client.ComponentClient;
import alma.acs.nc.LoggingConsumer;

/**
 * Client application that accesses the HelloDemo component. It demonstrates
 * how the class {@link ComponentClient}can be used as a base class.
 * 
 * @author hsommer Nov 21, 2002 5:53:05 PM
 */
public class LoggingConsumerTest extends ComponentClient
{
    /**
     * @param logger
     * @param managerLoc
     * @param clientName
     * @throws Exception
     */
    public LoggingConsumerTest(Logger logger, String managerLoc, String clientName)
	throws Exception 
	{
	    super(logger, managerLoc, clientName);
	    
	    m_consumer = new LoggingConsumer(getContainerServices(), this);
	    m_consumer.consumerReady();
	}
    
    public void receive(String xml)
	{
	    m_logger.info("receive method: xml=" + xml);
	    if (count_m < magicNumber)
		{
		count_m++;
		}
	}
    
    public void tearDown() throws Exception
	{
	    m_consumer.disconnect();

	    if(count_m==magicNumber)
		{
		System.out.println("Test passed!");
		}
	    else
		{
		System.out.println("Test failed!");
		}
	    super.tearDown();
	}
    
    private LoggingConsumer m_consumer = null;
    private int count_m = 0;
    private int magicNumber = 11;
    
    /**
     * Checks whether the Java property 'ACS.manager' is set and calls the
     * other methods from this class.
     */
    public static void main(String[] args) {
	String managerLoc = System.getProperty("ACS.manager");
	if (managerLoc == null) 
	    {
	    System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
	    System.exit(-1);
	    }
	String clientName = "LoggingConsumerTest";
	LoggingConsumerTest hlc = null;
	
	try 
	    {
	    hlc = new LoggingConsumerTest(null, managerLoc, clientName);
	    }
	catch (Exception e) 
	    {
	    e.printStackTrace(System.err);
	    }
	
	//sleep 15 seconds
	
	
	if (hlc != null) {
	try 
	    {
	    Thread.sleep(15000);
	    hlc.tearDown();
	    }
	catch (Exception e1) 
	    {
	    // bad luck
	    }
	}
    }
}

