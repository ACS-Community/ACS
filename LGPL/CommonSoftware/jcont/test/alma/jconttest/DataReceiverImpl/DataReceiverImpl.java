/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2008
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
package alma.jconttest.DataReceiverImpl;

import alma.acs.component.ComponentImplBase;
import alma.jconttest.DataReceiverOperations;

/**
 * @author rtobar  
 * Created on Feb 25, 2008, 11:18:27 PM
 */
public class DataReceiverImpl extends ComponentImplBase implements
		DataReceiverOperations {

	private volatile int invocations = 0;
	private volatile boolean started = false;
	private long initTime = 0;
	
	public double stop() {
	
		long endTime  = System.nanoTime();
		double timeInSeconds = (endTime - initTime)/1000000000.;
		return (invocations/timeInSeconds);
		
	}
	
	public void storeThis(String arg0) {
		
		if(!started) {
			started = true;
			initTime = System.nanoTime();
		}
		invocations++;
		
		// We sleep a while, simulating the DB access
		try {
			Thread.sleep(5);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

}
