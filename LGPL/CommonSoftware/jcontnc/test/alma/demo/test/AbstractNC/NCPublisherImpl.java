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
 * File NCPublisherImpl.java
 * 
 */
package alma.demo.test.AbstractNC;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.CorbaNotificationChannel;
import alma.acsnc.EventDescription;
import alma.demo.NCPublisherOperations;


/**
 * This is an example of a how an abstract notification channel
 * can be created. 
 * @author sroberts Dec 3, '03
 */
public class NCPublisherImpl 
    extends ComponentImplBase
        implements NCPublisherOperations {
    
    
    private CorbaNotificationChannel nc;
    

    public NCPublisherImpl() {
    }

    public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {

		super.initialize(containerServices);
		/*
		 * If you want a local channel put in AbstractNotificationChannel.LOCAL
		 */
		try {
			nc = new CorbaNotificationChannel("AbstractNC_Channel", m_containerServices);
		} catch (AcsJException ex) {
			throw new ComponentLifecycleException(ex);
		}
		System.out.println("Created NC Publisher");
	}

    
    public void publish(String name) throws CouldntPerformActionEx {
        EventDescription event = new EventDescription(name, 32L, 64L);
        try {
			nc.publish(event);
		} catch (AcsJException ex) {
			throw (new AcsJCouldntPerformActionEx(ex)).toCouldntPerformActionEx();
		}
    }

    public void cleanUp() {
        try {
			nc.deactivate();
		} catch (AcsJException ex) {
			ex.printStackTrace();
		}
    }
}
