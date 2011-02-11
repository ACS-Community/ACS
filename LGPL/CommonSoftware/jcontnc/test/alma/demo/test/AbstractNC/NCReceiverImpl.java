/*
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * All rights reserved
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 * 
 * File NCReceiverImpl.java
 * 
 */
package alma.demo.test.AbstractNC;

import java.util.logging.Logger;

import alma.acs.component.ComponentImplBase;
import alma.acs.container.ContainerServices;
import alma.acs.nc.CorbaNotificationChannel;
import alma.acs.nc.CorbaReceiver;
import alma.acs.nc.Receiver;
import alma.acsnc.EventDescription;
import alma.demo.NCReceiverOperations;
/**
 * This is an example of a receiver class for the Simulated
 * classes. The only method that is required is the receive method.
 * This is how Receiver.java knows that this is a valid receiver 
 * class.
 * @author sroberts Dec 3, '03
 */
public class NCReceiverImpl 
    extends ComponentImplBase 
        implements NCReceiverOperations {


	private Receiver receiver;
	private Logger logger;

    public NCReceiverImpl() {
    }

    public void initialize(ContainerServices containerServices) 
       throws alma.acs.component.ComponentLifecycleException {
        super.initialize(containerServices);
        this.logger = containerServices.getLogger();
        
        receiver = CorbaNotificationChannel.getCorbaReceiver("AbstractNC_Channel", m_containerServices);
        logger.info("Created legacy CorbaReceiver for channel 'AbstractNC_Channel'");
    }

    public void execute() {
        receiver.attach(EventDescription.class.getName(), this);
        logger.info("Attached 'NCReceiverImpl' as receiver object for '" + EventDescription.class.getName() + "' events.");
        receiver.begin();
    }

	@Override
	public void cleanUp() throws alma.maciErrType.wrappers.AcsJComponentCleanUpEx {
		// of course our receiver is a CorbaReceiver, but the old API is so messed up that we do it like this
		if (receiver instanceof CorbaReceiver) {
			CorbaReceiver corbaReceiver = (CorbaReceiver) receiver;
			// check if someone else has destroyed the channel already
			corbaReceiver.disconnect();
		}
		super.cleanUp();
	}

    public void receive(EventDescription e) {
        logger.info("NC_TEST: Got temperatureDataBlockEvent in receive, event=" +e.name);
    }
}
