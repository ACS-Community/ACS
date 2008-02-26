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
package alma.acs.monitoring;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Example on how {@link alma.acs.monitoring.RemoteThreadsClient} is intended
 * to be used.
 * @author rtobar
 * @since ACS 7.0
 */
public class RemoteThreadsMonitor {
	
	private RemoteThreadsClient rtc = null;
	
	/**
	 * Starts the application
	 * @param args See {@link #usage()}
	 * @see #usage()
	 */
	public static void main(String[] args) {
		
		if( args.length != 1 && args.length != 2 ) {
			RemoteThreadsMonitor.usage();
			System.exit(1);
		}

		RemoteThreadsMonitor rtm = new RemoteThreadsMonitor(args);
		RemoteThreadsMBean mbean = rtm.getMBean();
		System.out.println("Total JacORB threads: " + mbean.getJacORBThreadsCount());
		System.out.println("Runnable JacORB's threads:");
		RemoteThreadsUtil.printThreadsInfo(RemoteThreadsUtil.toThreadsInfo(mbean.getJacORBThreadsInfo(Thread.State.RUNNABLE)) ,System.out,false);
		System.out.println("\nRunnable JacORB's threads (grouped):");
		RemoteThreadsUtil.printThreadsInfo(RemoteThreadsUtil.toThreadsInfo(mbean.getJacORBThreadsInfo(Thread.State.RUNNABLE)) ,System.out,true);
		System.out.println("\nTotal number of threads: " + mbean.getAllThreadsCount());
		RemoteThreadsUtil.printThreadsInfo(RemoteThreadsUtil.toThreadsInfo(mbean.getAllThreadsInfo()) ,System.out,true);
		System.out.println("\nThreads staying at org.jacorb.poa.RequestProcessor: " + mbean.getThreadsCount("org.jacorb.poa.RequestProcessor",null));
		rtm.close();
	}

	/**
	 * Creates a new RemoteThreadsMonitor with the given command line args
	 * @param args The command line arguments.
	 */
	public RemoteThreadsMonitor(String[] args) {
		
		// Can be PID, className or host
		if( args.length == 1 ) {
			
			// Check if the arg is a className, a host or a PID
			boolean isNumber = true;
			boolean isHost = true;
			int pid = 0;
			InetAddress remoteHost = null;
			
			try {
				pid = Integer.valueOf(args[0]);
			} catch(NumberFormatException e) {
				isNumber = false;
			}

			try {
				remoteHost = InetAddress.getByName(args[0]);
			} catch (UnknownHostException e) {
				isHost = false;
			}
			
			// If args[0] is an integer it might be a PID
			if( isNumber ) {
				try {
					rtc = new RemoteThreadsClient(pid);
				} catch(RemoteThreadsException e) {
					System.err.println("Can't create a RemoteThreadsClient");
					e.printStackTrace();
					System.exit(1);
				}
			}
			
			// It might be a IP address or host name
			else if( isHost ) {
				try {
					rtc = new RemoteThreadsClient(remoteHost);
				} catch(RemoteThreadsException e) {
					System.err.println("Can't create a RemoteThreadsClient");
					e.printStackTrace();
					System.exit(1);
				}
			}
			
			// Try with a class name
			else {
				try {
					rtc = new RemoteThreadsClient(args[0]);
				} catch (RemoteThreadsException e) {
					System.err.println("Can't create a RemoteThreadsClient");
					e.printStackTrace();
					System.exit(1);
				}
			}
			
		} else if ( args.length == 2 ){
			try {
				InetAddress remoteHost = null;
				try {
					remoteHost = InetAddress.getByName(args[0]);
				} catch (UnknownHostException e) {
					System.err.println("Unknown host " + args[0]);
					System.exit(1);
				}
				rtc = new RemoteThreadsClient(remoteHost,Integer.valueOf(args[1]).intValue());
			} catch (NumberFormatException e) {
				System.err.println("The given port is not a number");
				e.printStackTrace();
				System.exit(1);
			} catch (RemoteThreadsException e) {
				System.err.println("Can't create a RemoteThreadsClient");
				e.printStackTrace();
				System.exit(1);
			}
		}
		
		if( rtc.connect() ) {
			System.out.println("Connected to the remote JVM");
		} else {
			System.err.println("Can't connect to the remote JVM");
			System.exit(1);
		}
		
	}
	
	/**
	 * Prints the usage. The command line accepts four different arguments
	 * combinations:
	 * <ul>
	 * 	<li><i>PID</i></li>
	 * 	<li><i>className</i></li>
	 * 	<li><i>host</i></li>
	 * 	<li><i>host</i> <i>port</i></li>
	 * </ul>
	 */
	public static void usage() {
		System.out.println("Usage:\n");
		System.out.println("For local processes:  RemoteThreadsMonitor <pid>");
		System.out.println("                      RemoteThreadsMonitor <className>\n");
		System.out.println("For remote processes: RemoteThreadsMonitor <host>");
		System.out.println("                      RemoteThreadsMonitor <host> <RMIport>\n");
		return;
	}
	
	/**
	 * Returns the {@link RemoteThreadsMBean} retreived by the internal
	 * {@link RemoteThreadsClient} instance.
	 * @return The {@link RemoteThreadsMBean} retreived by the 
	 * internal {@link RemoteThreadsClient} instance.
	 */
	public RemoteThreadsMBean getMBean() {
		
		RemoteThreadsMBean mbean = null;
		
		try {
			mbean = rtc.getMBean();
		} catch (RemoteThreadsException e) {
			System.err.println("Can't get the MBean");
			e.printStackTrace();
		}
		
		return mbean;
	}
	
	/**
	 * Closes the connection of the internal {@link RemoteThreadsClient}
	 * instance.
	 */
	public void close() {
		try {
			rtc.close();
		} catch (RemoteThreadsException e) {
			System.err.println("Can't close connection to the MBean server");
		}
	}
}
