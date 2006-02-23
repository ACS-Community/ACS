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
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

import alma.acsnc.EventDescription;
import alma.acs.nc.*;

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
    
    public NCReceiverImpl() {
    }

    public void initialize(ContainerServices containerServices) 
       throws alma.acs.component.ComponentLifecycleException {
        super.initialize(containerServices);
        System.out.println("Created NC Receiver");
        receiver = CorbaNotificationChannel.getCorbaReceiver("AbstractNC_Channel", m_containerServices);
    }

    public void execute() {
        receiver.attach("alma.acsnc.EventDescription", this);
        receiver.begin();
    }


    public void receive(EventDescription e) {
        System.out.println("NC_TEST: Got temperatureDataBlockEvent in receive");
        System.out.println("NC_TEST: "+e.name);
    }
        
}


