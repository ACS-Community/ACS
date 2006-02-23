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
 * MountConsumer.java
 *
 * Created on March 12, 2003, 2:33 PM
 */
////////////////////////////////////////////////////////////////////////////////
package alma.ACSCOURSE_MOUNT;
////////////////////////////////////////////////////////////////////////////////
import alma.acs.nc.Consumer;
import alma.acs.container.ContainerServices;
import alma.acs.component.client.ComponentClient;
////////////////////////////////////////////////////////////////////////////////
/** 
 * MountConsumer is a simple class that connects to the MOUNT_CHANNEL channel,
 * and prints events to standard out.
 * @author dfugate
 */
public class MountConsumer extends Consumer
{
    /** Creates a new instance of MountConsumer */
    public MountConsumer(ContainerServices cServices)
	throws alma.acs.exceptions.AcsJException
    {
        super(MOUNT_CHANNEL.value, cServices);
	
	//Subscribe to an event type and provide a reference to a class capable
	//of processing the event (see receive method).
	addSubscription(MountEventData.class, this);
    }    
    ////////////////////////////////////////////////////////////////////////////
    /** 
     * A <code>receive</code> method <B>must</B> be created for each type of event
     * we plan on receiving.
     * 
     * @param joe A data structure extracted from a CORBA event.
     */
    public void receive(MountEventData joe)
	{
	    System.out.println("The commanded Az/El received by this consumer are:" + 
			       joe.Azimuth + 
			       "," + 
			       joe.Elevation);
	}
    ////////////////////////////////////////////////////////////////////////////
    /** Illustrates a simple example outside of the component/container model.
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
	    String clientName = "MountConsumerClient";
	    ComponentClient myClient = new ComponentClient(null, managerLoc, clientName);
	    
	    //Create the consumer using the ACS Java client's container services.
	    MountConsumer joe = new MountConsumer(myClient.getContainerServices());
	    
            //After consumerReady() is invoked, processEvent(...) is invoked
            //by the channel.  That is, we have no control over when
            //that method is called.
            joe.consumerReady();
            System.out.println("Waiting for events...");

	    //Wait a while for some events
	    Thread.sleep(50000);

	    //Disconnect from the channel
	    joe.disconnect();

	    //Must cleanly disconnect the client
	    myClient.tearDown();
        }
        catch(Exception e)
        {
            e.printStackTrace(System.err);
        }
        System.out.println("Done...");
    }
    ////////////////////////////////////////////////////////////////////////////
}
