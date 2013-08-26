/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
package alma.alarmsystem.dump;

import java.util.logging.Logger;

import alma.acs.component.client.AdvancedComponentClient;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.source.SourceListener;
import cern.laser.source.alarmsysteminterface.FaultState;

/**
 * Dumps the alarms published by the sources
 * 
 * @author  acaproni
 * @since ACS 10.1    
 */
public class AlarmSourceDumper extends AlarmDumperBase implements SourceListener {
	
	/**
	 * The client to receive the XML for the alarm sources
	 */
	private final SourceClient m_sourceClient;
	
	/**
	 * Constructor
	 * 
	 * @param theLogger The logger
	 * @param mangerLoc The CORBA loc of the manager
	 * @throws Exception if an error happens instantiating the {@link AdvancedComponentClient}
	 */
	public AlarmSourceDumper(Logger theLogger, String mangerLoc) throws Exception {
		super(theLogger,mangerLoc,AlarmSourceDumper.class.getName());
		m_sourceClient = new SourceClient(getContainerServices());
	}
	
	@Override
	public void startReceivingAlarms() throws Exception {
		m_sourceClient.addAlarmListener(this);
		m_sourceClient.connect();		
	}
	
	/**
	 * Print the {@link FaultState} to {@link System#out}
	 */
	@Override
	public void faultStateReceived(FaultState fs) {
		StringBuilder str = new StringBuilder(formatTimestamp(fs.getUserTimestamp()));
		str.append(' ');
		str.append(fs.getFamily());
		str.append(':');
		str.append(fs.getMember());
		str.append(':');
		str.append(fs.getCode());
		str.append(' ');
		str.append(fs.getDescriptor());
		System.out.println(str.toString());
	}

	/**
	 * An XML has been received: nothing to do
	 */
	@Override
	public void sourceXMLMsgReceived(String asiMessage) {
	}

	@Override
	public void close() {
		m_sourceClient.close();
	}
	
	/**
	 * Main delegate to {@link AlarmDumperBase#clientRunner(String[], boolean)}
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		AlarmDumperBase.clientRunner(args, true,0,null);
	}
}
