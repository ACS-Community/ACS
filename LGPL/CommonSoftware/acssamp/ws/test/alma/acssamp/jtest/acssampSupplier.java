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

import java.util.logging.Logger;
import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.ACSSamp.*;


public class acssampSupplier extends ComponentClient
{

	private Samp m_samp;
	private Logger m_logger;


	public acssampSupplier(String managerLoc, String clientName)
		throws Exception
	{
		super(null, managerLoc, clientName);

		// same service interface that a component would get from the container...
		ContainerServices csrv = getContainerServices();
		
		// get a logger
		m_logger = csrv.getLogger();
		
		String sampCurl = "SAMP1";
		
		// get (CORBA) reference to Samp component
		org.omg.CORBA.Object sampObj = csrv.getComponent(sampCurl);

		// use CORBA helper class for the type cast
		m_samp = SampHelper.narrow(sampObj);
	}


	/**
	 * Calls methods on our samp component
	 */
	public void doSomeStuff()
	{
	    m_logger.info("will now use the samp component...");
	    try 
		{
		    SampObj sampObj = m_samp.initSampObj("LAMP1","brightness",1000000,10000000);
		    sampObj.start();
		    m_logger.info(" ACS sampling started");

		    Thread.sleep(5000);
		    sampObj.suspend();
		    m_logger.info("ACS sampling suspended");

		    Thread.sleep(5000);
		    sampObj.resume();
		    m_logger.info("ACS sampling resumed");

		    Thread.sleep(6000);
		    sampObj.stop();
		    m_logger.info("ACS sampling stopped");

		    Thread.sleep(2000);
		    sampObj.destroy();
		    m_logger.info("ACS sampling destroyed");
		    
		}
	    catch(Exception e)
		{
		// not handled for the moment
		}
	}
	



	public static void main(String[] args)
	{
	    
	    String managerLoc = System.getProperty("ACS.manager");
	    if (managerLoc == null)
		{
		    System.out.println("Java property 'ACS.manager' " + 
				       " must be set to the corbaloc of the ACS manager!");
		    System.exit(-1);
		}
	    
	    String clientName = "acssampSupplier1";
	    
	    try
		{
		    acssampSupplier foo = new acssampSupplier(managerLoc, clientName);
		    
		    foo.doSomeStuff();
		}
	    catch (Exception e)
		{
		    e.printStackTrace(System.err);
		}
	}
}

