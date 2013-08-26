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

import javax.management.openmbean.CompositeData;

/**
 * MBean interface representing a set of threads of a remote java process. In
 * order to have all the threading information fo the JVM, the implementation
 * should use the {@link ThreadMXBean} platform MXBean to get all
 * the necessary information.<br/>
 * 
 * All the methods returning {@link CompositeData} arrays 
 * actually return {@link java.lang.management.ThreadInfo} objects transformed
 * into <code>CompositeData</code> ones. Since <code>ThreadInfo</code> is not 
 * serializable, then <code>CompositeData</code> type must be used to get the 
 * information. When calling these methods, therefore, the 
 * <code>CompositeData</code> type should be transformed into 
 * <code>ThreadInfo</code> ones using the  
 * {@link java.lang.management.ThreadInfo#from(javax.management.openmbean.CompositeData)}
 * method.
 * 
 * @author rtobar
 * @since ACS 7.0
 */
public interface RemoteThreadsMBean {

	/**
	 * Returns a dump of all the threads of the JVM. 
	 * @return All the threads' information of the JVM, as an array of
	 * {@link CompositeData}  type.
	 */
	public CompositeData[] getAllThreadsInfo();

	/**
	 * Returns the total number of threads, for all states
	 * @return The total number of threads
	 */
	public int getAllThreadsCount();

	/**
	 * Returns all the JacORB related threads' information
	 * @return All the JacORB related threads' information
	 */
	public CompositeData[] getJacORBThreadsInfo();

	/**
	 * Returns all the JacORB related threads' information for a given thread
	 * state.
	 * @param state The desired threads' state
	 * @return All the JacORB related threads' information for the given thread 
	 * state. If <code>state</code> is null, then return all threads
	 */
	public CompositeData[] getJacORBThreadsInfo(Thread.State state);

	/**
	 * Returns the total count of JacORB-related threads, for all thread states
	 * @return The total count of JacORB-related threads
	 */
	public int getJacORBThreadsCount();

	/**
	 * Returns all the JacORB related threads' information
	 * @return All the JacORB related threads' information
	 */
	public CompositeData[] getAcsContainerThreadsInfo();

	/**
	 * Returns all the ACS related threads' information for a given thread
	 * state.
	 * @param state The desired threads' state. If null, then all states are
	 * considered
	 * @return All the ACS related threads' information for the given thread 
	 * state. If <code>state</code> is null, then return all threads
	 */
	public CompositeData[] getAcsContainerThreadsInfo(Thread.State state);

	/**
	 * Returns the total count of ACS-related threads, for all thread states
	 * @return The total count of ACS-related threads
	 */
	public int getAcsContainerThreadsCount();

	/**
	 * Returns all the threads that are currently placed on the class given by
	 * <code>className</code> and that present the state <code>state</code>.
	 * @param className The class name where the threads are placed. Full class name
	 * should be given (e.g., java.lang.String).
	 * @param state The threads' state
	 * @return All the threads placed at that class
	 */
	public CompositeData[] getThreadsInfo(String className, Thread.State state);

	/**
	 * Returns the count of all the threads that are currently placed on the class
	 * given by <code>className</code> and that present the given state.
	 * @param className The class name where the threads are placed. Full class name
	 * should be given (e.g., java.lang.String). 
	 * @param state The threads' state. If null, then all states are considered.
	 * @return The threads' count for the given class name and thread state.
	 */
	public int getThreadsCount(String className, Thread.State state);
}
