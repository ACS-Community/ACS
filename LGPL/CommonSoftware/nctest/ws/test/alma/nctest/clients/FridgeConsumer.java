/*
 *    ALMA - Atacama Large Millimiter Array
 *   (c) Associated Universities Inc., 2002 
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 *
 * FridgeConsumer.java
 *
 * Created on March 12, 2003, 2:33 PM
 */
package alma.nctest.clients;

import alma.acs.component.client.ComponentClient;

/** 
 * FridgeConsumer is a simple class that connects to the "fridge" notification channel,
 * receives five events, and then disconnects from the channel.
 * @author dfugate
 */
public class FridgeConsumer extends alma.acs.nc.Consumer
{
    /** 
     * Total number of events that have been consumed.
     */    
    int eventCount = 0;
    
    volatile boolean readyForShutdown = false;

    /** Creates a new instance of FridgeConsumer */
    public FridgeConsumer(alma.acs.container.ContainerServices services) throws Exception
    {
        super(alma.FRIDGE.CHANNELNAME_FRIDGE.value, services);
    }
    
    
    ////////////////////////////////////////////////////////////////////////////
    /** 
     * <code>processEvent</code> <B>must</B> be overriden in Consumer subclasses to
     * do something useful.
     * 
     * @param corbaData CORBA data extracted from the event.
     */    
    public void processEvent(Object corbaData) {
		// Know how many events this instance has received.
		eventCount++;
		try {
			// Stop receiving events
			if (eventCount > 5) {
				suspend();
			} 
			else {
				// Ensure we're getting the correct type of data from the CORBA
				// Any.
				if (corbaData.getClass() == alma.FRIDGE.temperatureDataBlockEvent.class) {
					System.out.println("The temp difference is: " + ((alma.FRIDGE.temperatureDataBlockEvent) corbaData).absoluteDiff);
				} 
				else {
					System.err.println("The type of filterable_data[0] is not correct!");
				}
			}
		} 
		catch (Exception e) {
			System.err.println(e);
		}
		finally {
			if (eventCount > 5) {
				readyForShutdown = true;
			}
		}
	}
    
    
    // //////////////////////////////////////////////////////////////////////////
    /** Illustrates a simple example outside of the component/container model.
     * @param args Not used!
     */    
    public static void main(String[] args) {
		try {
			//Setup an ACS Java client. This has little to do with the NC API
			String managerLoc = System.getProperty("ACS.manager");
			if (managerLoc == null) {
				System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
				System.exit(-1);
			}
			
			String clientName = "FridgeConsumer";
			ComponentClient myClient = new ComponentClient(null, managerLoc, clientName);

			FridgeConsumer joe = new FridgeConsumer(myClient.getContainerServices());

			//Subscribe to an event type.
			joe.addSubscription(alma.FRIDGE.temperatureDataBlockEvent.class);

			//After consumerReady() is invoked, processEvent(...) is invoked
			//by the notification channel.  That is, we have no control over when
			//that method is called.
			joe.consumerReady();

			//Sleep until five events have been received... @todo: better sync using a CountDownLatch or similar instead of sleep&check
			System.out.println("Waiting for events...");
			while (!joe.readyForShutdown) {
				Thread.sleep(1000);
			}

			//then destroy everything
			joe.disconnect();
			myClient.tearDown();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("Done...");
	}
    
    
    ////////////////////////////////////////////////////////////////////////////
}
