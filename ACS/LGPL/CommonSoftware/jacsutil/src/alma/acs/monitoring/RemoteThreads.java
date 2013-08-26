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

import java.lang.Thread.State;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;

import javax.management.openmbean.CompositeData;

import sun.management.ThreadInfoCompositeData;

/**
 * Main implementation of the {@link alma.acs.monitoring.RemoteThreadsMBean}
 * interface. Further implementations of the implemented interface could
 * inherit from this class.
 * @author rtobar
 * @since ACS 7.0
 */
public class RemoteThreads implements RemoteThreadsMBean {

	public static String[] JACORB_CLASSES = {
			"org.jacorb.",
			"org.omg."
	};
	
	public static String[] ACS_CLASSES = {
			"alma."
	};
	
	protected ThreadMXBean tmxb = null;
	
	public RemoteThreads() {
		tmxb = ManagementFactory.getThreadMXBean();
	}
	
	public CompositeData[] getAllThreadsInfo() {
		ThreadInfo []info = tmxb.dumpAllThreads(true,true);
		CompositeData []data = new CompositeData[info.length];

		for(int i=0; i!=info.length; i++)
			data[i] = ThreadInfoCompositeData.toCompositeData(info[i]);

		return data;
	}

	public int getJacORBThreadsCount() {
		return getThreadsCount(JACORB_CLASSES,null);
	}
	
	public CompositeData[] getJacORBThreadsInfo() {
		return getJacORBThreadsInfo(null);
	}

	public CompositeData[] getJacORBThreadsInfo(State state) {
		return getThreadsInfo(JACORB_CLASSES,state);
	}

	public int getAcsContainerThreadsCount() {
		return getThreadsCount(ACS_CLASSES,null);
	}
	
	public CompositeData[] getAcsContainerThreadsInfo() {
		return getAcsContainerThreadsInfo(null);
	}

	public CompositeData[] getAcsContainerThreadsInfo(State state) {
		return getThreadsInfo(ACS_CLASSES,state);
	}

	public int getAllThreadsCount() {
		return tmxb.dumpAllThreads(true,true).length;
	}

	public CompositeData[] getThreadsInfo(String className, State state) {
		return getThreadsInfo(new String[] {className}, state);
	}

	public int getThreadsCount(String className, State state) {
		return getThreadsCount(new String[] {className}, state);
	}

	protected CompositeData[] getThreadsInfo(String [] classes,State state) {
		
		ThreadInfo []info = tmxb.dumpAllThreads(true,true);
		CompositeData[] dataList = new CompositeData[info.length];
		int includedData = 0;
	
		for(int i=0; i!=info.length; i++) {
			
			// Get the last StackTraceElement from the thread information
			StackTraceElement []stackElements = info[i].getStackTrace();
			int idx = stackElements.length-1;
			if(stackElements.length > 0) {
				
				// Get the class name of the top class of this thread
				String cName = stackElements[idx].getClassName();
				if( cName.equals("java.lang.Thread")) {
					cName = stackElements[idx - 1].getClassName();
				}
				
				// See if the class name is in the give list
				for(int j=0; j!=classes.length; j++) 
					if( cName.startsWith(classes[j]) &&
					    (state == null || info[i].getThreadState() == state )) {
						dataList[includedData++] = ThreadInfoCompositeData.toCompositeData(info[i]);
						break;
					}
				
			}
		}
		
		// Return only the filled data
		CompositeData []newDataList = new CompositeData[includedData];
		System.arraycopy(dataList,0,newDataList,0,includedData);
		return newDataList;
	}

	protected int getThreadsCount(String [] classes, State state) {
		ThreadInfo []info = tmxb.dumpAllThreads(true,true);
		int threadCount = 0;
	
		for(int i=0; i!=info.length; i++) {
			
			// Get the last StackTraceElement from the thread information
			StackTraceElement []stackElements = info[i].getStackTrace();
			int idx = stackElements.length-1;
			if(stackElements.length > 0) {
				
				// Get the class name of the top class of this thread
				String cName = stackElements[idx].getClassName();
				if( cName.equals("java.lang.Thread") && stackElements.length >= 2) {
					cName = stackElements[idx - 1].getClassName();
				}
				
				// See if the class name is in the give list
				for(int j=0; j!=classes.length; j++) 
					if( cName.startsWith(classes[j]) &&
					    (state == null || info[i].getThreadState() == state )) {
						threadCount++;
						break;
					}

			}
		}
		
		return threadCount;
	}
}
