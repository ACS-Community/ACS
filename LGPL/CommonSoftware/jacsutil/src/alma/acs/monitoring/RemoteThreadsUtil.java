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

import java.io.PrintStream;
import java.lang.Thread.State;
import java.lang.management.ThreadInfo;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.management.openmbean.CompositeData;

import alma.acs.util.StringOutputStream;

/** 
 * This class is intended to have only <code>public static</code> methods to
 * help in the management of information given by the {@link RemoteThreadsMBean}
 * class, like type conversion and printing information.
 * @author rtobar   
 * @since ACS 7.0
 */
public class RemoteThreadsUtil {

	/**
	 * Converts a {@link CompositeData} array into a {@link ThreadInfo} one
	 * using the {@link ThreadInfo#from(javax.management.openmbean.CompositeData)}
	 * method.
	 * @param data The {@link CompositeData} array
	 * @return The {@link ThreadInfo} array corresponding to the <code>
	 * data</code> input data.
	 */
	public static ThreadInfo[] toThreadsInfo(CompositeData[] data) {
		
		ThreadInfo[] info = new ThreadInfo[data.length];
		
		for(int i=0; i!= data.length; i++)
			info[i] = ThreadInfo.from(data[i]);
		
		return info;
	}
	
	/**
	 * Returns an array with the information of a group of threads that can be
	 * printed. The information can be grouped or not by 
	 * thread state and by the name of the class where the thread "started". 
	 * @param info The {@link ThreadInfo} array with the information to be printed
	 * @param grouped If true, the information is grouped by class name and state,
	 * printing the count for each group. 
	 */
	public static String printThreadsInfo(ThreadInfo[] info,boolean grouped) {
		
		// We use the alma.acs.util.StringOutputStream class
		StringOutputStream stringOS = new StringOutputStream();
		PrintStream printer = new PrintStream(stringOS);
		
		printThreadsInfo(info,printer,grouped);
		printer.flush();
		
		return stringOS.toString();
	}
	
	/**
	 * Prints the information of a group of threads into the specified printer
	 * object <code>printer</code>. The information can be grouped or not by 
	 * thread state and by the name of the class where the thread "started". 
	 * @param info The {@link ThreadInfo} array with the information to be printed
	 * @param printer Object where the information is going to be printed, such
	 * as {@link System.out}.
	 * @param grouped If true, the information is grouped by class name and state,
	 * printing the count for each group. 
	 */
	public static void printThreadsInfo(ThreadInfo[] info, PrintStream printer, boolean grouped) {

		// For grouped data
		List<Data> threadStats = new ArrayList<Data>(info.length);
		
		// Head
		printer.printf("%-50s%-15s","Class name","State");
		if( grouped ) {
			printer.printf("%-5s","#");
		}
		printer.print("\n==========                                        =====");
		if( grouped ) {
			printer.print("          =");
		}
		printer.println();
		
		for(int i=0; i!=info.length; i++) {
			StackTraceElement[] elements = info[i].getStackTrace();
			int idx = elements.length - 1;
			if( idx > 1 ) {
				String className = elements[idx].getClassName();
				if( className.equals("java.lang.Thread") )
					className = elements[--idx].getClassName();
				
				if( !grouped ) {
					printer.printf("%-50s%-15s\n",elements[idx].getClassName(),info[i].getThreadState());
				}
				
				else {
					boolean found = false;
					Data threadEntry = new Data(className, info[i].getThreadState(), 1);
					for (Iterator<Data> iterator = threadStats.iterator(); iterator
					.hasNext();) {
						Data candidate = iterator.next();
						if (threadEntry.equals(candidate)) {
							candidate.bumpCount(); // already there, increment count
							found = true;
							break;
						}
					}
					if (!found) threadStats.add(threadEntry);
				}
			}
		}
		
		if( grouped ) {
			Collections.sort(threadStats);
			
			for (Iterator<Data> iterator = threadStats.iterator(); iterator
			.hasNext();) {
				if (iterator != null) {
					Data dat = iterator.next();
					printer.printf("%-50s%-15s%d\n",dat.getName(),
							dat.getState(), dat.getCount());
				}
			}
		}
		
		return;
	}
	
	private static class Data implements Comparable<Data>{
		private String name;
		private State state;
		private int count;
		
		public Data(String name, State state, int count) {
			super();
			this.name = name;
			this.state = state;
			this.count = count;
		}
		
		public int getCount() {
			return count;
		}

//		public void setCount(int count) {
//			this.count = count;
//		}
		
		public void bumpCount() {
			this.count++;
		}
		
		public String getName() {
			return name;
		}
		public State getState() {
			return state;
		}
		
		public boolean equals(Object o) {
			if (o == null || !(o instanceof Data)) return false;
			Data test = (Data) o;
			if (name.equals(test.getName()) && state.equals(test.getState())) return true;
			return false;						
		}

		public int compareTo(Data o) {
			if (o == null) throw new NullPointerException();
			int nameResult = o.getName().compareTo(getName());
			if ( nameResult == 0 )
				return o.getState().compareTo(getState());
			return nameResult;		
		}
		
		
	}
}
