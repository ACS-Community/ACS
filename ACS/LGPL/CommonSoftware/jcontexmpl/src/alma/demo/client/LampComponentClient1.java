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

import alma.ACS.RWdouble;
import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.acsexmplLamp.Lamp;
import alma.acsexmplLamp.LampHelper;

/**
 * Client to the C++ Lamp demo component.
 * 
 * @author hsommer Jun 23, 2003 6:43:08 PM
 */
public class LampComponentClient1 extends ComponentClient
{
	private Lamp m_lamp;
	
	private Logger m_logger;


	public LampComponentClient1(String managerLoc, String clientName)
		throws Exception
	{
		super(null, managerLoc, clientName);

		// same service interface that a component would get from the container...
		ContainerServices csrv = getContainerServices();
		
		// get a logger
		m_logger = csrv.getLogger();
		
		String lampCurl = "LAMP1";
		
		// or if we don't know the curl (=instance name)...	
//		String[] lampCurls = csrv.findComponents(null, "IDL:alma.acsexmplLamp.Lamp:1.0");
//		if (lampCurls.length > 0)
//		{
//			lampCurl = lampCurls[0];
//		}
//		else
//		{
//			throw new Exception("no Lamp component available");
//		}

		// get (CORBA) reference to Lamp component
		org.omg.CORBA.Object lampObj = csrv.getComponent(lampCurl);

		// use CORBA helper class for the type cast
		m_lamp = LampHelper.narrow(lampObj);
	}


	/**
	 * Calls methods on our lamp component
	 */
	public void doSomeStuff()
	{
		m_logger.info("will now use the lamp component...");
		
		RWdouble propBrightness = m_lamp.brightness();
		
		propBrightness.set_sync(22.345);

		m_logger.info("lamp component calls done...");
	}
	



	public static void main(String[] args)
	{
		//
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null)
		{
			System.out.println("Java property 'ACS.manager' " + 
			" must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		
		String clientName = "LampComponentClient1";
		
		try
		{
			LampComponentClient1 lcc = new LampComponentClient1(managerLoc, clientName);
			
			lcc.doSomeStuff();
			
			lcc.tearDown();
		}
		catch (Exception e)
		{
			e.printStackTrace(System.err);
		}
	}
}

