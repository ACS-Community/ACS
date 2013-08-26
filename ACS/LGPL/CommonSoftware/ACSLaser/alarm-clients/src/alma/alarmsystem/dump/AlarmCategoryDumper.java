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
import alma.alarmsystem.clients.CategoryClient;
import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;

/**
 * Dumps the alarms published by the alarm system to clients
 * 
 * @author  acaproni
 * @since ACS 10.1    
 */
public class AlarmCategoryDumper extends AlarmDumperBase implements AlarmSelectionListener {
	
	/**
	 * The client to receive the XML for the alarm sources
	 */
	private final CategoryClient m_categoryClient;
	
	/**
	 * Constructor
	 * 
	 * @param theLogger The logger
	 * @param mangerLoc The CORBA loc of the manager
	 * @throws Exception if an error happens instantiating the {@link AdvancedComponentClient}
	 */
	public AlarmCategoryDumper(Logger theLogger, String mangerLoc) throws Exception {
		super(theLogger,mangerLoc,AlarmSourceDumper.class.getName());
		m_categoryClient = new CategoryClient(getContainerServices());
	}
	
	@Override
	public void startReceivingAlarms() throws Exception {
		m_categoryClient.connect(this);		
	}

	@Override
	public void close() throws Exception {
		m_categoryClient.close();
	}

	@Override
	public void onAlarm(Alarm alarm) {
		StringBuilder str = new StringBuilder(formatTimestamp(alarm.getStatus().getSourceTimestamp()));
		str.append(' ');
		str.append(alarm.getAlarmId());
		str.append(" active=");
		str.append(alarm.getStatus().isActive());
		str.append(" priority=");
		str.append(alarm.getPriority());
		str.append(" reduced=");
		str.append(alarm.getStatus().isReduced());
		str.append(" masked=");
		str.append(alarm.getStatus().isMasked());
		System.out.println(str.toString());
	}

	@Override
	public void onException(LaserSelectionException e) {
		System.err.println("Exception caught from alarm service: "+e.getMessage());
		e.printStackTrace(System.err);
	}
	
	/**
	 * Main delegate to {@link AlarmDumperBase#clientRunner(String[], boolean)}
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		AlarmDumperBase.clientRunner(args, false,0,null);
	}
}
