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

import java.util.logging.Logger;

import alma.FRIDGE.temperatureDataBlockEvent;
import alma.acs.component.client.ComponentClient;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.AcsEventSubscriber.Callback;
import alma.acsnc.EventDescription;

/** 
 * FridgeConsumer is a simple class that connects to the "fridge" notification channel,
 * receives five events, and then disconnects from the channel.
 * @author dfugate
 */
public class FridgeConsumer extends ComponentClient implements Callback<temperatureDataBlockEvent>
{
	/**
	 * Total number of events that have been consumed.
	 */
	private int eventCount = 0;

	private volatile boolean readyForShutdown = false;
	
	private AcsEventSubscriber<temperatureDataBlockEvent> subscriber;
	

	public FridgeConsumer(Logger logger, String managerLoc) throws Exception {
		super(logger, managerLoc, FridgeConsumer.class.getSimpleName());
	}
	
	private void run() throws AcsJException {
		subscriber = getContainerServices().
				createNotificationChannelSubscriber(alma.FRIDGE.CHANNELNAME_FRIDGE.value, temperatureDataBlockEvent.class);

		// Subscribe to an event type.
		subscriber.addSubscription(this);

		// After startReceivingEvents() is invoked, the receive(...) callback method is invoked
		// by the notification channel. That is, we have no control over when
		// that method is called.
		subscriber.startReceivingEvents();

		// Sleep until five events have been received... @todo: better sync using a CountDownLatch or similar instead of
		// sleep&check
		System.out.println("Waiting for events...");
		while (!readyForShutdown) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException ex) {
				Thread.currentThread().interrupt();
			}
		}
	}
	
	@Override
	public void receive(temperatureDataBlockEvent eventData, EventDescription eventDescrip) {
		// Know how many events this instance has received.
		eventCount++;
		try {
			// Stop receiving events
			if (eventCount > 5) {
				subscriber.suspend(); // not really needed, but nice to see that it works...
			} 
			else {
				// Ensure we're getting the correct type of data from the CORBA Any.
				if (eventData.getClass() == alma.FRIDGE.temperatureDataBlockEvent.class) {
					System.out.println("The temp difference is: " + eventData.absoluteDiff);
				} 
				else {
					// should never happen
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

	@Override
	public Class<temperatureDataBlockEvent> getEventType() {
		return temperatureDataBlockEvent.class;
	}
	
	@Override
	public void tearDown() throws Exception {
		subscriber.disconnect();
		super.tearDown();
	}


    // //////////////////////////////////////////////////////////////////////////
	
	/**
	 * Illustrates a simple example outside of the component/container model.
	 * 
	 * @param args
	 *            Not used!
	 */
	public static void main(String[] args) {
		try {
			// Setup an ACS Java client. This has little to do with the NC API
			String managerLoc = System.getProperty("ACS.manager");
			if (managerLoc == null) {
				System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
				System.exit(-1);
			}

			FridgeConsumer fridgeConsumer = new FridgeConsumer(null, managerLoc);
			fridgeConsumer.run();

			// then destroy everything
			fridgeConsumer.tearDown();

		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("Done...");
	}

}
