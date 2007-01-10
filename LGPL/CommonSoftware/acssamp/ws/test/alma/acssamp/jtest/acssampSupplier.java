/*
 *    ALMA - Atacama Large Millimiter Array
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
 *    acssampConsumer.java
 *
 *    Created on 19/11/2003 by oat
 */

/**
 * This test program is used to demonstrate/test the ACS 
 * sampling system. In particular it samples
 * LAMP1:brightness property with a frequency of 0.1 Hz and
 * report rate 1 sec.
 */

////////////////////////////////////////////////////////////////////////////////
package alma.acssamp.jtest;

////////////////////////////////////////////////////////////////////////////////

import java.util.logging.Level;
import java.util.logging.Logger;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.acssamp.Samp;
import alma.acssamp.SampHelper;
import alma.acssamp.SampObj;

/**
 * A client of the Samp component, which asks the samp component to sample 
 * the brightness property of the Lamp component.
 * The result should be that the samp component publishes the aggregated sampling 
 * information on the notification channel "NC_LAMP1_brightness_1000000_10000000".
 * <p>
 * Indirectly this class can thus be seen as an NC supplier, which I guess explains its name.
 */
public class acssampSupplier extends ComponentClient {
	
	private final Logger m_logger;
	
	public acssampSupplier(String managerLoc, String clientName) throws Exception {
		super(null, managerLoc, clientName);
		m_logger = getContainerServices().getLogger();
	}

	/**
	 * Calls methods on our samp component to sample LAMP1.brightness,
	 * with the full show of suspending, resuming, stopping and eventually destroying this. 
	 */
	public void sampleLampBrightness() throws AcsJContainerServicesEx {

		// get (CORBA) reference to C++ Samp component (type IDL:alma/acssamp/Samp:1.0)
		String sampCurl = "SAMP1";
		Samp samp = SampHelper.narrow(getContainerServices().getComponent(sampCurl));

		m_logger.info("will now use the samp component...");
		SampObj sampObj = null;
		try {
			// register for sampling the 'brightness' property of the Lamp component
			// in a way that the report rate is 10 times slower than the sampling rate
			sampObj = samp.initSampObj("LAMP1", "brightness", 1000000, 10000000);

			sampObj.start();
			m_logger.info("ACS sampling of Lamp#brightness has started. Will sample this for one minute.");
			Thread.sleep(60000);
			
			sampObj.suspend();
			m_logger.info("ACS sampling suspended. Will resume after 5 seconds.");
			Thread.sleep(5000);
			
			sampObj.resume();
			m_logger.info("ACS sampling resumed. Will sample another 6 seconds.");
			Thread.sleep(6000);
			
			sampObj.stop();
			m_logger.info("ACS sampling stopped. Will destroy the sampling object in 2 seconds.");
			Thread.sleep(2000);
			
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Unexpected exception occured while using the samp component.", e);
		} finally {
			if (sampObj != null) {
				sampObj.destroy();
			}
			if (samp != null) {
				getContainerServices().releaseComponent(sampCurl);
			}
			m_logger.info("ACS sampling destroyed.");			
		}
	}

	public static void main(String[] args) {

		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}

		try {
			acssampSupplier foo = new acssampSupplier(managerLoc, "acssampSupplier1");
			
			foo.sampleLampBrightness();
			
			Thread.sleep(1000);
		} catch (Exception e) {
			e.printStackTrace(System.err);
		}
	}
}
