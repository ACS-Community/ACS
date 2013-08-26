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
 package alma.acs.jlog.test;

import java.util.Calendar;
import alma.acs.component.client.AdvancedComponentClient;
import java.util.logging.Logger;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;

/**
 * The class sends log till a certain time.
 * The purpose is to execute jlog for a long periodo of time.
 * <p>
 * @TODO: this class illegally uses code from module jcont 
 * which is not availeble because jlog builds before jcont.
 * It only works with the current practice of first building src in all modules and then the tests later.
 * 
 * @author acaproni
 */
public class LongRunTest extends Thread {
	
	// The time interval between the sending of two logs
	// in a block of logs
	private static final int LOGS_INTERVAL=100;
	
	// The text of the message of each log 
	// The current time (msec) is appended
	private static final String logMsg = "Log message published at ";
	
	// The hour and time to terminate
	private int endHr, endMin; 
	
	// The time interval in msec between the sending of two blocks of logs
	private int msec;
	
	// The time interval (msec) between the sending of 2 logs in a block
	// Default is LongRunTest.LOGS_INTERVAL
	private int blockInterval=LongRunTest.LOGS_INTERVAL;
	
	// The ACS component client
	private AdvancedComponentClient client=null;
	
	// The logger
	private Logger logger=null;
	
	/**
	 * Constructor
	 * 
	 * @param endTime The actual time to stop sending logs (format HH:MM) 
	 * @param msec Time interval between the sending of 2 block of logs
	 * @param logInt Time interval between the sending of 2 logs
	 * @throws IllegalArgumentException
	 */
	public LongRunTest(String endTime, String msec, String logInt) throws IllegalArgumentException {
		if (endTime==null || msec==null) {
			throw new IllegalArgumentException("Invalid null date/interval");
		}
		setEndTime(endTime);
		setInterval(msec,logInt);
		try {
			connectACSComponentClient();
		} catch (Throwable t) {
			System.err.println("Error connecting to ACS:"+t.getMessage());
			t.printStackTrace(System.err);
			System.exit(-1);
		}
		start();
	}
	
	/**
	 * Decode the millisec
	 * 
	 * @param millisec The interval in msec between the sending of two blocks of logs
	 * @param logInt The interval in msec between the sending of two logs in a block
	 *               (it can be null, in that case the default is used)
	 */
	private void setInterval(String millisec, String blockInt) throws IllegalArgumentException {
		if (millisec==null) {
			throw new IllegalArgumentException("Invalid null interval");
		}
		try {
			msec= Integer.parseInt(millisec);
		} catch (Throwable e) {
			throw new IllegalArgumentException("Error decoding the interval between 2 blocks of logs: "+millisec,e);
		}
		if (msec<0) {
			throw new IllegalArgumentException("Invalid interval between blocks of logs: "+millisec);
		}
		try {
			blockInterval= Integer.parseInt(blockInt);
		} catch (Throwable e) {
			throw new IllegalArgumentException("Error decoding the interval between 2 logs: "+blockInt,e);
		}
		if (blockInterval<0) {
			throw new IllegalArgumentException("Invalid interval between logs: "+blockInt);
		}
	}
	
	/**
	 * Decode the hr:min string into the local variables
	 * 
	 * @param time The time in the format hh:mm
	 */
	private void setEndTime(String time) throws IllegalArgumentException {
		String[] times = time.split(":");
		if (times.length!=2 && times.length!=3) {
			throw new IllegalArgumentException("Malformed end time "+time);
		}
		try {
			endHr = Integer.parseInt(times[0]);
			endMin = Integer.parseInt(times[1]);
		} catch (Exception e) {
			throw new IllegalArgumentException("Error decoding the time "+time,e);
		}
		if (endHr<0 || endHr>23 || endMin<0 || endMin>59) {
			throw new IllegalArgumentException("Invalid time "+time);
		}
	}
	
	/**
	 * The starting point of the application
	 *
	 */
	public static void main(String args[]) {
		if (args.length!=2 && args.length!=3) {
			LongRunTest.printUsage();
		} else {
			LongRunTest test;
			try {
				test = new LongRunTest(args[0],args[1],args[2]);
			} catch (Throwable e) {
				System.err.println("Exception: "+e.getMessage());
				e.printStackTrace(System.err);
				LongRunTest.printUsage();
			}
		}
	}
	
	/**
	 * 
	 * @return true if the current time 
	 */
	private boolean checkTime() {
		Calendar cal = Calendar.getInstance();
		int actualHr=cal.get(Calendar.HOUR_OF_DAY);
		int actualMin=cal.get(Calendar.MINUTE);
		return actualHr==endHr && actualMin==endMin;
	}
	
	/**
	 * Connect to ACS as component client.
	 * It connects client and the logger.
	 * 
	 * @return True if the connection is ok
	 */
	private boolean connectACSComponentClient() {
		String clientName="Mount GUI";
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(clientName, true);
		if (logger==null) {
			System.err.println("Error getting the logger");
			return false;
		}
        String managerLoc = System.getProperty("ACS.manager").trim();

        try {
        	client = new AdvancedComponentClient(logger, managerLoc, clientName);
        	logger.log(AcsLogLevel.INFO,"Connected to ACS");
        } catch (Exception e) {
        	logger.log(AcsLogLevel.ERROR,"Error connecting the simple client: "+e.getMessage());
        	client=null;
        }
        
		return (client!=null);
	}
	
	/**
	 * Logs a block of logs i.e. one log of each type separated by
	 * LOGS_INTERVAL msec
	 *
	 */
	private void publishLogsBlock() {
		for (int t=0; t<9; t++) {
			AcsLogLevel level=null;
			switch (t) {
			case 0: {
				level=AcsLogLevel.DELOUSE;
				break;
				}
				case 1: {
					level=AcsLogLevel.DEBUG;
					break;
				}
				case 2: {
					level=AcsLogLevel.INFO;
					break;
				}
				case 3: {
					level=AcsLogLevel.NOTICE;
					break;
				}
				case 4: {
					level=AcsLogLevel.WARNING;
					break;
				}
				case 5: {
					level=AcsLogLevel.ERROR;
					break;
				}
				case 6: {
					level=AcsLogLevel.CRITICAL;
					break;
				}
				case 7: {
					level=AcsLogLevel.EMERGENCY;
					break;
				}
				case 8: {
					level=AcsLogLevel.ALERT;
					break;
				}
				default: {
					level=AcsLogLevel.INFO;
				}
			
			}
			if (t==0) {
				logger.log(AcsLogLevel.TRACE,"");
			} else {
				logger.log(level,LongRunTest.logMsg+System.currentTimeMillis());
			}
			try {
				Thread.sleep(blockInterval);
			} catch (InterruptedException ie) {}
		}
	}
	
	/**
	 * The thread to publish logs
	 *
	 */
	public void run() {
		while (true) {
			if (checkTime()) {
				return;
			}
			publishLogsBlock();
			try {
				Thread.sleep(msec);
			} catch (InterruptedException ie) {}
		}
	}
	
	/**
	 * Print the usage string in the command line
	 */
	public static void printUsage() {
		StringBuilder str = new StringBuilder("\nUSAGE: LongRunTest HH:MM String msecBlockInterval msecLogInterval\n");
		str.append("\tHH:MM: Actual time to stop sending logs\n");
		str.append("\tmsecBlockInterval: The interval in msec between the sending of two blocks of logs\n");
		str.append("\tmsecLogInterval: The interval in msec between the sending of two logs\n");
		str.append("LongRunTest sends longs until the actual time is HH:MM\n");
		str.append("Logs are sent in blocks of 10, one log for each type separated by msecLogInterval msecs.\n");
		str.append("After sending one block of logs, LongRunTest waits msecBlockInterval msecs before sending the next block.\n");
		System.out.println(str.toString());
	}
	
}
