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
 */
package alma.demo.client;

import java.util.logging.Logger;

import alma.ACSErr.ACSException;
import alma.acs.component.client.ComponentClient;
import alma.demo.LampCallback;
import alma.demo.LampUnavailable;


/**
 * Client application that accesses the HelloDemo component.
 * It demonstrates how the class {@link ComponentClient} can be used as a base class.
 * 
 * @author hsommer Nov 21, 2002 5:53:05 PM
 */
public class LampCallbackClient extends ComponentClient
{
	private LampCallback m_lampCallback;
		
	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public LampCallbackClient(Logger logger, String managerLoc, String clientName)
		throws Exception
	{
		super(logger, managerLoc, clientName);

		m_lampCallback = alma.demo.LampCallbackHelper.narrow(
			getContainerServices().getComponent("LAMPCALLBACK1"));
			
	}

	public void doSomeStuff() throws LampUnavailable
	{
		monitorLampBrightness();
		
		try
		{
			m_lampCallback.acsExceptionMethodVoid();
			m_logger.warning("no ex was thrown!");
		}
		catch (ACSException e)
		{
			m_logger.info("received exception from acsExceptionMethodVoid(): ");
			
			e.printStackTrace();
		}

		try
		{
			double d = m_lampCallback.acsExceptionMethodDouble();
			
			m_logger.warning("no ex was thrown, double=" + d);
		}
		catch (ACSException e)
		{
			m_logger.info("received exception from acsExceptionMethodDouble(): ");
			
			e.printStackTrace();
		}
		
		
	}

	public void monitorLampBrightness() throws LampUnavailable
	{
//		if (m_comp == null)
//		{
//			try
//			{
//				setUp();
//			}
//			catch (Exception e)
//			{
//				e.getStackTrace();
//			}
//
//		}
		String actualBrightness = "";

		actualBrightness = m_lampCallback.monitorLampBrightness() + "";

//		String expectedBrightness = 0.0 + "";
		System.out.println("monitored brightness " + actualBrightness);
	}
	
	
	/**
	 * Checks whether the Java property 'ACS.manager' is set and calls the other methods from this class.
	 */
	public static void main(String[] args)
	{
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null)
		{
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		
		String clientName = "LampCallbackClient";
		LampCallbackClient lcbcl = null;
		try
		{
			lcbcl = new LampCallbackClient(null, managerLoc, clientName);
			
			lcbcl.doSomeStuff();
		}
		catch (Exception e)
		{
			e.printStackTrace(System.err);
		}
		finally 
		{
			if (lcbcl != null)
			{
				try
				{
					lcbcl.tearDown();
				}
				catch (Exception e1)
				{
					// bad luck
				}
			}				
		}
	}
}
