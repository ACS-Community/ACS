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
package alma.demo.client;

import java.util.logging.Level;
import java.util.logging.Logger;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.demo.HelloDemo;

/**
 * Client application that accesses the HelloDemo component. It demonstrates
 * how the class {@link ComponentClient}can be used as a base class.
 * 
 * @author hsommer Nov 21, 2002 5:53:05 PM
 */
public class HelloDemoClient extends ComponentClient
{
	private HelloDemo m_helloComp;

	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public HelloDemoClient(Logger logger, String managerLoc, String clientName)
			throws Exception {
		super(logger, managerLoc, clientName);
	}

	/**
	 * Calls sayHello() on the hello component.
	 * @throws AcsJContainerServicesEx 
	 */
	public void doSomeStuff() throws AcsJContainerServicesEx {
        m_helloComp = alma.demo.HelloDemoHelper.narrow(getContainerServices().getComponent("HELLODEMO1"));
        
		String helloRet = m_helloComp.sayHello();
		m_logger.info("got string from component's sayHello method: "
				+ helloRet);
	}

	/**
	 * Checks whether the Java property 'ACS.manager' is set and calls the
	 * other methods from this class.
	 */
	public static void main(String[] args) {
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out
					.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		String clientName = "HelloDemoClient";
		HelloDemoClient hlc = null;
		try {
			hlc = new HelloDemoClient(null, managerLoc, clientName);
			hlc.doSomeStuff();
		}
		catch (Exception e) {
            try {
                Logger logger = hlc.getContainerServices().getLogger();
                logger.log(Level.SEVERE, "Client application failure", e);
            } catch (Exception e2) {
                e.printStackTrace(System.err);
            }
		}
		finally {
			if (hlc != null) {
				try {
					hlc.tearDown();
				}
				catch (Exception e3) {
					// bad luck
                    e3.printStackTrace();
				}
			}
		}
	}
}
