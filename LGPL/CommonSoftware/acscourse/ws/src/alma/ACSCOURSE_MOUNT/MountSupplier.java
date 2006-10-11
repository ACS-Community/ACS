/*
 *    ALMA - Atacama Large Millimiter Array
 *   (c) Associated Universities Inc., 2002 
 *   (c) European Southern Observatory, 2002
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
 * MountSupplier.java
 *
 * Created on March 13, 2003, 11:43 AM
 */
////////////////////////////////////////////////////////////////////////////////
package alma.ACSCOURSE_MOUNT;
////////////////////////////////////////////////////////////////////////////////
import alma.acs.component.client.ComponentClient;
import alma.acs.nc.SimpleSupplier;
////////////////////////////////////////////////////////////////////////////////
/** MountSupplier is an event channel example that shows how to use the SimpleSupplier
 *  class.
 * @author dfugate
 */
public class MountSupplier extends Thread
{
    ////////////////////////////////////////////////////////////////////////////
    /** In this example, the main function is all that is used.
     * @param args Not used!
     */    
    public static void main(String[] args)
	{
	    try
		{
		//Setup an ACS Java client. This has little to do with the NC API
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null)
		    {
		    System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
		    System.exit(-1);
		    }
		String clientName = "MountSupplierClient";
		ComponentClient myClient = new ComponentClient(null, managerLoc, clientName);

		//Create and initialize the supplier
		SimpleSupplier joe = new SimpleSupplier(MOUNT_CHANNEL.value,  //the channel's name 
							myClient.getContainerServices());  //type of event to be published

		//Create the event that will be published
		MountEventData t_block = new MountEventData(3.14F, 43.0F);
		
		//Publish the event 50 times int 50 seconds
		System.out.println("Now sending events...");
		for(int i=0; i<50; i++)
                    {		        
		    joe.publishEvent(t_block);
		    sleep(1000);
                    }

		//Must cleanly disconnect the supplier
		joe.disconnect();

		//Must cleanly disconnect the client
		myClient.tearDown();
                }
	    catch(Exception e)
                {
		e.printStackTrace(System.err);
                }
    }
    ////////////////////////////////////////////////////////////////////////////
}
