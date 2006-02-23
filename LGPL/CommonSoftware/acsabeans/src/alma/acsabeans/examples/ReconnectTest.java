/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.acsabeans.examples;

import com.cosylab.datatypes.AbstractDynamicValueEvent;
import com.cosylab.datatypes.DynamicValueListener;

import abeans.models.ConnectionEvent;
import abeans.models.ConnectionListener;
import abeans.models.LinkEvent;
import abeans.models.LinkListener;
import abeans.pluggable.acs.ACSAbeansEngine;
import alma.ACS.abeans.ROdouble;
import alma.ACS.abeans.RWdouble;
import alma.PS.abeans.PowerSupply;

/**
 * Testing reconnect bug.
 * 
 * @author <a href="mailto:igor.kriznar@cosylab.com">Igor Kriznar</a>
 * @version		$Id: ReconnectTest.java,v 1.1 2005/08/23 15:23:52 ikriznar Exp $
 */
public class ReconnectTest
{
	
	/**
	 * The main entry point for every Java application.
	 */
	public static void main(String[] args)
	{
		// initialize the engine (initializes all the services and logs into the manager)
		ACSAbeansEngine engine = new ACSAbeansEngine();
		engine.initialize();

		try
		{
			// create a bean
			PowerSupply powerSupply = new PowerSupply();
			// set remote name and do the connect 		
			powerSupply.setRemoteName("TEST_PS_1");
			
			// print component name
			System.out.println("Name: " + powerSupply.getName());
			
			// get readback and its units
			ROdouble readback = powerSupply.getReadback();
			String units = readback.getUnits();

			// read readback value
			System.out.println("Value: " + readback.getValue() + units);
			
			// set current value
			RWdouble current = powerSupply.getCurrent();
			double newValue = (current.getMaximum() + current.getMinimum())/2.0;
			current.setValue(newValue);
			
			// readback value should change...
			System.out.println("New value: " + readback.getValue() + units);
			
			powerSupply.addConnectionListener(new ConnectionListener() {
				public void connectableConnecting(ConnectionEvent arg0) {
					System.out.println("connectableConnecting "+arg0);
				}

				public void connectableDisconnecting(ConnectionEvent arg0) {
					System.out.println("connectableDisconnecting "+arg0);
				}

				public void connectableConnectionFailed(ConnectionEvent arg0) {
					System.out.println("connectableConnectionFailed "+arg0);
				}

				public void connectableDestroyed(ConnectionEvent arg0) {
					System.out.println("connectableDestroyed "+arg0);
				}

				public void connectableInitialState(ConnectionEvent arg0) {
					System.out.println("connectableInitialState "+arg0);
				}

				public void connectableReady(ConnectionEvent arg0) {
					System.out.println("connectableReady "+arg0);
				}
			});
			
			powerSupply.addLinkListener(new LinkListener() {
				public void linkableResumed(LinkEvent arg0) {
					System.out.println("linkableResumed "+arg0);
				}

				public void linkableSuspended(LinkEvent arg0) {
					System.out.println("linkableSuspended "+arg0);
				}

				public void linkEstablished(LinkEvent arg0) {
					System.out.println("linkEstablished "+arg0);
				}

				public void linkLost(LinkEvent arg0) {
					System.out.println("linkLost "+arg0);
				}
			});
			
			current.addLinkListener(new LinkListener() {
				public void linkableResumed(LinkEvent arg0) {
					System.out.println("linkableResumed "+arg0);
				}

				public void linkableSuspended(LinkEvent arg0) {
					System.out.println("linkableSuspended "+arg0);
				}

				public void linkEstablished(LinkEvent arg0) {
					System.out.println("linkEstablished "+arg0);
				}

				public void linkLost(LinkEvent arg0) {
					System.out.println("linkLost "+arg0);
				}
			});
			
			current.addDynamicValueListener(new DynamicValueListener() {
				public void valueUpdated(AbstractDynamicValueEvent arg0) {
				}

				public void valueChanged(AbstractDynamicValueEvent arg0) {
				}

				public void valueTimeoutStarts(AbstractDynamicValueEvent arg0) {
					System.out.println("valueTimeoutStarts "+arg0);
				}

				public void valueTimeoutStops(AbstractDynamicValueEvent arg0) {
					System.out.println("valueTimeoutStops "+arg0);
				}

				public void valueTimelagStarts(AbstractDynamicValueEvent arg0) {
				}

				public void valueTimelagStops(AbstractDynamicValueEvent arg0) {
				}

				public void valueErrorResponse(AbstractDynamicValueEvent arg0) {
					System.out.println("valueErrorResponse "+arg0);
				}
			});
			
		}
		catch (Throwable t)
		{
			t.printStackTrace();
		}
		finally
		{
			// destroy and exit
			//engine.destroy();
		}
	}
}
