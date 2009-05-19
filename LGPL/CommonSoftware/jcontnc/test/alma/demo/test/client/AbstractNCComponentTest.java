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

import alma.demo.NCPublisher;
import alma.demo.NCReceiver;

import alma.acs.component.client.ComponentClient;

/**
 * HSO 2009-05: The components used by this test have been modified to no longer use 
 *              the deprecated AbstractNotificationChannel
 *              but instead to use CorbaNotificationChannel directly. 
 *              Therefore this test is outdated and should probably be removed.
 *              I just leave it in now, hoping that it might give some little additional coverage.
 * @author sroberts March 17, 2004
 */
public class AbstractNCComponentTest extends ComponentClient {

    private NCPublisher publisherComp;
    private NCReceiver receiverComp;

	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public AbstractNCComponentTest(Logger logger, 
                                String managerLoc, 
                                    String clientName) throws Exception {
                                    
		super(logger, managerLoc, clientName);
		publisherComp = alma.demo.NCPublisherHelper.narrow(
                getContainerServices().getComponent("ABSTRACT_PUBLISHER"));
		receiverComp = alma.demo.NCReceiverHelper.narrow(
                getContainerServices().getComponent("ABSTRACT_RECEIVER"));
	}

	/**
     *
	 */
	public void sendEvents() {
        try { 
    		publisherComp.publish("event 1");
            Thread.sleep(2000);
    		publisherComp.publish("event 2");
            Thread.sleep(2000);
    		publisherComp.publish("event 3");
            Thread.sleep(2000);
    		publisherComp.publish("event 4");
            Thread.sleep(2000);
    		publisherComp.publish("event 5");
            Thread.sleep(2000);
        } catch (Exception e) {
        }
	}

    public void tearDown() throws Exception
	{
	    getContainerServices().releaseComponent("ABSTRACT_PUBLISHER");
	    getContainerServices().releaseComponent("ABSTRACT_RECEIVER");
	    super.tearDown();
	}


	/**
     *
	 */
	public static void main(String[] args) {
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out.println(
                "Java property 'ACS.manager' must be "+
                    "set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		String clientName = "AbstractNC_Client";
		AbstractNCComponentTest test=null;
		try {
			test = new AbstractNCComponentTest(null, managerLoc, clientName);
			test.sendEvents();
			test.tearDown();
		}
		catch (Exception e) {
			e.printStackTrace(System.err);
		}
	}
}

