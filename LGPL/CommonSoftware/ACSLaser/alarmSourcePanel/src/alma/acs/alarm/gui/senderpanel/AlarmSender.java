/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2011
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarm.gui.senderpanel;

import java.security.InvalidParameterException;
import java.sql.Timestamp;
import java.util.Properties;
import java.util.logging.Logger;

import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.FaultStateCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * The class to send an alarm
 * 
 * @author acaproni
 *
 */
public class AlarmSender {

	/**
	 * The source to send alarms
	 */
	private final ACSAlarmSystemInterface alarmSource;
	
	/**
	 * The logger
	 */
	private final Logger logger;
	
	/**
	 * Constructor
	 * 
	 * @throws SourceCreationErrorEx 
	 * @throws ACSASFactoryNotInitedEx 
	 */
	public AlarmSender(Logger logger) throws ACSASFactoryNotInitedEx, SourceCreationErrorEx {
		if (logger==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		alarmSource=ACSAlarmSystemInterfaceFactory.createSource(this.getClass().getName());
		this.logger=logger;
	}
	
	/**
	 * Send an alarm 
	 * 
	 * @param faultFamily The Fault Family
	 * @param faultMember The Fault member
	 * @param faultCode The Fault Code
	 * @param descriptor The descritptor (ACTIVE, TERMINATE, CHANGE, INSTANT)
	 * @param props The user properties
	 * @throws ACSASFactoryNotInitedEx
	 * @throws FaultStateCreationErrorEx
	 */
	public void send(
			String faultFamily, 
			String faultMember, 
			int faultCode, 
			String descriptor,
			Properties props) throws ACSASFactoryNotInitedEx, FaultStateCreationErrorEx {
		logger.finer("Sending alarm "+faultFamily+","+faultMember+","+faultCode+" with descriptor "+descriptor);
		ACSFaultState state =ACSAlarmSystemInterfaceFactory.createFaultState(
				faultFamily.trim(),
				faultMember.trim(),
				faultCode);
		state.setDescriptor(descriptor);
		state.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
		if (props!=null && !props.isEmpty()) {
			state.setUserProperties(props);
		}
		alarmSource.push(state);
		logger.finer("Alarm "+faultFamily+","+faultMember+","+faultCode+" sent");
	}
	
	/**
	 * Send the specified alarm
	 * @param triplet The triplet in the form FF,FM,FC
	 * @param The descriptor
	 * @param props The user properties
	 * @throws FaultStateCreationErrorEx 
	 * @throws ACSASFactoryNotInitedEx 
	 */
	public void send(String triplet, String descriptor, Properties props) throws ACSASFactoryNotInitedEx, FaultStateCreationErrorEx {
		triplet=triplet.trim();
		String[] vals=triplet.split(",");
		if (vals.length!=3) {
			throw new InvalidParameterException("Invalid triplet "+triplet);
		}
		int code;
		try {
			code=Integer.parseInt(vals[2]);
		} catch (NumberFormatException nfe) {
			throw new InvalidParameterException("Invalid fault code "+vals[2]);
		}
		System.out.println("FF="+vals[0]+" FM="+vals[1]+", FC="+vals[2]);
		send(vals[0],vals[1],code,descriptor,props);
	}
	
	/**
	 * Close the sender
	 */
	public void close() {
		alarmSource.close();
	}
}
