/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package alma.acs.logging.engine.utils;

/**
 * A class to get system resources.
 * 
 * @author acaproni
 *
 */
public class ResourceChecker implements IResourceChecker {

	/**
	 * The <code>RunTime</code> used to monitor the memory usage
	 */
	private static final Runtime rt = Runtime.getRuntime();
	
	/**
	 * The maximum amount of memory that your Java application can use
	 * 
	 * @see {@link Runtime} 
	 */
	private volatile long maxMemory;
	
	/**
	 * The memory allocated by the application.
	 * Measured in bytes.
	 * 
	 * @see {@link Runtime} 
	 */
	private volatile long allocatedMemory;
	
	/**
	 * The amount of free memory from the allocated memory
	 * Measured in bytes.
	 * 
	 * @see {@link Runtime} 
	 */
	private volatile long freeMemory;
	
	/**
	 * The memory allocated that can be used by the application
	 * taking into account the memory not yet allocated by the JVM.
	 * Measured in bytes.
	 * 
	 * @see {@link Runtime} 
	 */
	private volatile long totalFreeMemory;
	
	/**
	 * 
	 * @return
	 */
	public long getMaxMemory() {
		updateMemory();
		return maxMemory;
	}

	/**
	 * 
	 * @return
	 */
	public long getAllocatedMemory() {
		updateMemory();
		return allocatedMemory;
	}

	/**
	 * 
	 * @return
	 */
	public long getFreeMemory() {
		updateMemory();
		return freeMemory;
	}

	/**
	 * 
	 * @return
	 */
	public long getTotalFreeMemory() {
		updateMemory();
		return totalFreeMemory;
	}
	
	private void updateMemory() {
		synchronized (rt) {
			allocatedMemory=rt.totalMemory();
			maxMemory=rt.maxMemory();
			freeMemory=rt.freeMemory();
			totalFreeMemory=(freeMemory+(maxMemory-allocatedMemory));	
		}
	}
}
