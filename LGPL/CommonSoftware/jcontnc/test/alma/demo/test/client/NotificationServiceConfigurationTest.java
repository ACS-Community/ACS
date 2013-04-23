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

import org.omg.CosNaming.NamingContext;

import alma.acs.component.client.ComponentClient;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Helper;
import alma.acs.util.AcsLocations;

/**
 * Test notification channel configuration (mapping).
 */
public class NotificationServiceConfigurationTest extends ComponentClient
{
	
	private final NamingContext nctx;

	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public NotificationServiceConfigurationTest(Logger logger, String managerLoc, String clientName)
			throws Exception {
		super(logger, managerLoc, clientName);
		
		nctx = Helper.getNamingServiceInitial(getContainerServices());
	}

	/**
	 * Gets the supplier component to send out events 
	 */
	public void doSomeStuff() throws AcsJException {
		// nothing configured in CDB -> encoded default NotifyEventChannelFactory
		System.out.println((new Helper("any", getContainerServices(), nctx).getNotificationFactoryNameForChannel()));
		
		// channel mapping in CDB -> ParticularNotifyEventChannelFactory
		System.out.println((new Helper("PARTICULAR1", getContainerServices(), nctx).getNotificationFactoryNameForChannel()));
		
		// channel mapping in CDB and service name expansion -> ParticularNotifyEventChannelFactory
		System.out.println((new Helper("PARTICULAR2", getContainerServices(), nctx).getNotificationFactoryNameForChannel()));
		
		// wildchars channel mapping in CDB -> ControlNotifyService
		System.out.println((new Helper("CONTROL_CHANNEL", getContainerServices(), nctx).getNotificationFactoryNameForChannel()));
		
		// domain mapping in CDB and service name expansion -> AlarmNotifyEventChannelFactory
		System.out.println((new Helper("anyOnLaser", "ALARMSYSTEM", getContainerServices(), nctx).getNotificationFactoryNameForChannel()));
		
		// non-existing domain -> encoded default NotifyEventChannelFactory
		System.out.println((new Helper("anyOnNonExistingDomain", "NONEXISTING_DOMAIN", getContainerServices(), nctx).getNotificationFactoryNameForChannel()));
	}


	/**
	 * Checks whether the Java property 'ACS.manager' is set and calls the
	 * other methods from this class.
	 */
	public static void main(String[] args) {
		String managerLoc = AcsLocations.figureOutManagerLocation();
		String clientName = NotificationServiceConfigurationTest.class.getName();
		NotificationServiceConfigurationTest hlc = null;
		try {
			hlc = new NotificationServiceConfigurationTest(null, managerLoc, clientName);
			hlc.doSomeStuff();
		}
		catch (Throwable e) {
			e.printStackTrace(System.err);
		}
		finally {
			if (hlc != null) {
				try {
					hlc.tearDown();
				}
				catch (Throwable e1) {
					// bad luck
				}
			}
		}
	}
}

