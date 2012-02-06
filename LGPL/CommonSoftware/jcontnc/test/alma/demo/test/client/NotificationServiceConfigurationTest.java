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
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Helper;

/**
 * Test notification channel configuration (mapping).
 */
public class NotificationServiceConfigurationTest extends ComponentClient
{
	
	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public NotificationServiceConfigurationTest(Logger logger, String managerLoc, String clientName)
			throws Exception {
		super(logger, managerLoc, clientName);
	}

	/**
	 * Gets the supplier component to send out events 
	 */
	public void doSomeStuff() throws AcsJException {
		// default expected
		System.out.println((new Helper(getContainerServices()).getNotificationFactoryNameForChannel("any")));
		
		// channel mapping 
		System.out.println((new Helper(getContainerServices()).getNotificationFactoryNameForChannel("PARTICULAR")));
		
		// wildchars channel mapping 
		System.out.println((new Helper(getContainerServices()).getNotificationFactoryNameForChannel("CONTROL_CHANNEL")));
		
		// domain mapping
		System.out.println((new Helper(getContainerServices()).getNotificationFactoryNameForChannel("anyOnLaser", "ALARMSYSTEM")));
		
		// fallback to default
		System.out.println((new Helper(getContainerServices()).getNotificationFactoryNameForChannel("anyOnNonExistingDomain", "NONEXISTING_DOMAIN")));
	}


	/**
	 * Checks whether the Java property 'ACS.manager' is set and calls the
	 * other methods from this class.
	 */
	public static void main(String[] args) {
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
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

