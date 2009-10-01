/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2009
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
package alma.acs.alarmsystem.acsimpl;

import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.AlarmServicePOA;
import alma.alarmsystem.Triplet;
import alma.acs.alarmsystem.corbaservice.AlarmSystemCorbaServer;

import alma.acs.util.XmlNormalizer;

import org.omg.CosPropertyService.Property;

import alma.ACSErrTypeCommon.BadParameterEx;
import alma.ACSErrTypeCommon.UnexpectedExceptionEx;

public class AcsAlarmSystem extends AlarmServicePOA {
	
	/**
	 * The CORBA server
	 */
	private final AlarmSystemCorbaServer corbaServer;
	
	/**
	 * Set to <code>true</code> if the alarm service has been shut down
	 */
	private volatile boolean closed=false;
	
	/**
	 * The logger
	 */
	private final Logger logger;
	
	/**
	 * A class to terminate the alarm service asynchronously.
	 * <P>
	 * The alarm service is stopped by calling the shutdown IDL method.
	 * But inside such a method, the ORB can't be closed.
	 * This class shuts down the servant outside of the ORB thread.
	 * 
	 * @author acaproni
	 *
	 */
	public class AcsComponentTerminator implements Runnable {
		public void run() {
			corbaServer.shutdown();
			logger.log(AcsLogLevel.DEBUG,"See you soon :-)");
			
		}
	}
	
	/**
	 * Constructor
	 */
	public AcsAlarmSystem(AlarmSystemCorbaServer corbaServer) throws Exception {
		if (corbaServer==null) {
			throw new Exception("The CORBA server can't be null");
		}
		this.corbaServer=corbaServer;
		logger=corbaServer.getLogger();
	}
	
	/**
	 * Return the type of alarm system
	 * 
	 * @return always <code>true</code>
	 */
	public boolean isACSAlarmService() {
		return true;
	}
	
	/**
	 * Shutdown the alarm service
	 */
	public synchronized void shutdown() {
		if (closed) {
			return;
		}
		closed=true;
		logger.log(AcsLogLevel.DEBUG,"Shutting down");
		Thread t = new Thread(new AcsComponentTerminator(),"LaserComponentTerminator");
		t.start();
	}
	
	/**
	 * IDL method: submit an alarm without.
	 * <P>
	 * Build a message to sent to the {@link AlarmMessageProcessorImpl#process(Message)}.
	 * 
	 * @param triplet The triplet of the alarm
	 * @param active if <code>true</code> the alarm is active
	 * @param sourceHostName The name of the host of the source
	 * @param timestamp The timestamp of the source
	 * @param alarmProperties Additional user-defined properties of the alarm
	 */
	public synchronized void submitAlarm(
			Triplet triplet,
			boolean active,
			String sourceHostName,
			String sourceName,
			long timestamp,
			Property[] alarmProperties) throws BadParameterEx, UnexpectedExceptionEx {
		String activeString=active?"ACTIVE":"TERMINATE";
		StringBuilder sb = new StringBuilder("Alarm sent: <");
		sb.append(triplet.faultFamily+','+triplet.faultMember+','+triplet.faultCode+"> ");
		sb.append(activeString);
		logger.log(AcsLogLevel.ALERT,XmlNormalizer.normalize(sb.toString()));
	}
}
