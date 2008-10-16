/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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
package alma.alarmsystem.clients.test;

import java.sql.Timestamp;
import java.util.logging.Logger;

import cern.laser.source.alarmsysteminterface.FaultState;

import alma.acs.component.client.ComponentClient;
import alma.acs.logging.ClientLogManager;
import alma.alarmsystem.clients.test.CategoryClientTest.AlarmsFromCDB;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * An object sending alarms and used to check the system outside of tat.
 * 
 * This process sends the same alarms defined in <code>CategoryClientTest</code> to use the
 * same CDB.
 * It is composed of a thread whose termination is defined by the first parameter of the 
 * command line (i.e. a SenderMode.)
 * 
 * @author almadev
 *
 */
public class AlarmSender extends Thread{
	
	/**
	 * The different ways of working of this client.
	 * 
	 * @author almadev
	 *
	 */
	private enum SenderMode {
		NEVER_ENDING, // Continuously send alarms
		TIME_LIMIT, // Send alarms for the specified time
		NUM_OF_ALARMS; // Send a defined number of alarms
		
		/**
		 * The SenderMode for the passed string
		 * 
		 * @param str The string describing the mode
		 * @return a SenderMode for the passed string
		 *         <code>null</code> if the string does not describe any SenderMode
		 */
		public static SenderMode fromString(String str) {
			for (SenderMode mode: SenderMode.values()) {
				if (str.equalsIgnoreCase(mode.toString())) {
					return mode;
				}
			}
			return null;
		}
	}
	
	// Interval (msec) between the sending of 2 alarms
	private static final int TIME_INTERVAL = 250;
	
	// The component client
	private ComponentClient client=null;
	
	// The first parameter of the command line
	private SenderMode mode;
	
	// The parameter set in the command line whose meaning depends by
	// the mode in use
	private long param;
	
	// The alarm source
	private ACSAlarmSystemInterface alarmSource;
	
	/**
	 * Constructor.
	 * 
	 * @param mode The way of functioning
	 * @param param Depends on the selected <code>mode</code>
	 *              It can be the number of alarms to send or the time (seconds).
	 *              It is ignored if the mode is <code>NEVER_ENDING</code>
	 * 
	 * @see <code>ComponentClient</code>
	 */
	public AlarmSender(SenderMode mode, long param) throws Exception {
		if (mode!=SenderMode.NEVER_ENDING && param<=0) {
			throw new IllegalArgumentException(param+" is invalid for "+mode);
		}
		// Connect the component client
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("SourcePanel",true);
        String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
        	System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
        	System.exit(-1);
        }
        this.mode=mode;
        this.param=param;
		client = new ComponentClient(logger,managerLoc,"AlarmSender");
		alarmSource = ACSAlarmSystemInterfaceFactory.createSource();
	}
	
	/**
	 * Push an alarm
	 * 
	 * @param active If true the alarm is active
	 */
	private void send_alarm(String family, String member, int code, boolean active) throws Exception {
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(family, member, code);
		if (active) {
			fs.setDescriptor(FaultState.ACTIVE);
		} else {
			fs.setDescriptor(FaultState.TERMINATE);
		}
		fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

		alarmSource.push(fs);
	}
	
	/**
	 * The thread sending alarms.
	 * the termination of the thread depends on the selected SenderMode.
	 * 
	 * The thread is composed of a loop that sends each alarms until the max
	 * limit of alarms or time is reached or forever if the mode is
	 * NEVER_ENDING. 
	 * 
	 * The loop send all the alarms defined in <code>AlarmsFromCDB</code>
	 * as active then as inactive and so on.
	 */
	@Override
	public void run() {
		// The time to finish
		long endTime = (mode==SenderMode.TIME_LIMIT)?System.currentTimeMillis()+param*1000:Long.MAX_VALUE;
		// The number of alarms to send
		long numOfAlarms = (mode==SenderMode.NUM_OF_ALARMS)?param:Long.MAX_VALUE;
		
		long alarmsSent=0;
		boolean active=true;
		while (true) {
			for (AlarmsFromCDB alarm: AlarmsFromCDB.values()) {
				// The time has elapsed
				if (mode==SenderMode.TIME_LIMIT && System.currentTimeMillis()>endTime) {
					return;
				}
				try {
					send_alarm(alarm.FF, alarm.FM, alarm.FC, active);
				} catch (Throwable t) {
					System.err.println("Exception sending alarm: "+t.getMessage());
				}
				alarmsSent++;
				if (mode==SenderMode.NUM_OF_ALARMS && alarmsSent>=numOfAlarms) {
					return;
				}
				try {
					Thread.sleep(TIME_INTERVAL);
				} catch (Exception e) {}
			}
			active=!active;
		}
	}
	
	/**
	 * Print the USAGE string in the standard output
	 */
	public static void printUsage() {
		System.out.println("\nUSAGE: AlarmSender mode <value>");
		System.out.print("mode: ");
		for (SenderMode mode: SenderMode.values()) {
			System.out.print(mode+" ");
		}
		System.out.println();
		System.out.println("value:");
		System.out.println("\tthe number of alarms to send if mode is NUM_OF_ALARMS");
		System.out.println("\tthe number of seconds spent sending alarms if mode is TIME_LIMIT");
		System.out.println("\n");
	}
	
	public static void main(String[] args) {
        if (args.length<1 || args.length>2) {
        	printUsage();
        	System.exit(-1);
        }
        SenderMode mode = SenderMode.fromString(args[0]);
        if (mode==null) {
        	printUsage();
        	System.exit(-1);
        }
        System.out.println("MODE: "+mode);
        long param = -1;
        if (mode!=SenderMode.NEVER_ENDING) {
        	if (args.length!=2) {
        		printUsage();
            	System.exit(-1);
        	}
        	try {
        		param = Long.parseLong(args[1]);
        	} catch (Exception e) {
        		printUsage();
            	System.exit(-1);
        	}
        }
        AlarmSender sender=null;
        try {
        	sender = new AlarmSender(mode, param);
        } catch (Exception e) {
        	System.err.println("Error instantiating the AlarmSender: "+e.getMessage());
        	e.printStackTrace(System.err);
        	return;
        }
        // Start the thread
        sender.start();
	}

}
