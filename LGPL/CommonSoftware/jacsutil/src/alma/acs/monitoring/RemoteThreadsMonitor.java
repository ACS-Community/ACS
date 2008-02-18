/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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

public class RemoteThreadsMonitor {
	
	private RemoteThreadsClient rtc = null;
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		if( args.length != 1 && args.length != 2 ) {
			RemoteThreadsMonitor.usage();
			System.exit(1);
		}
		
		RemoteThreadsMonitor rtm = new RemoteThreadsMonitor(args);
		RemoteThreadsMBean mbean = rtm.getMBean();
		System.out.println(mbean.getAcsContainerThreadsCount());
	}

	public RemoteThreadsMonitor(String[] args) {
		
		if( args.length == 1 ) {
			try {
				rtc = new RemoteThreadsClient(args[0]);
			} catch (RemoteThreadsException e) {
				System.err.println("Can't create a RemoteThreadsClient");
				e.printStackTrace();
				System.exit(1);
			}
		} else if ( args.length == 2 ){
			try {
				rtc = new RemoteThreadsClient(args[0],Integer.valueOf(args[1]).intValue());
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
	
	public static void usage() {
		System.out.println("Usage:\n");
		System.out.println("For local processes:  RemoteThreadsMonitor <pid>");
		System.out.println("                      RemoteThreadsMonitor <className>\n");
		System.out.println("For remote processes: RemoteThreadsMonitor <host>");
		System.out.println("                      RemoteThreadsMonitor <host> <RMIport>\n");
		return;
	}
	
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
}
