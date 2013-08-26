/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.util;

public class MemoryUtil
{

	/**
	 * Uses {@link Runtime} calls to generate a string of the form
	 * <code>Memory usage 2555 of 15872 kB (= 1.0% of JVM growth limit 253440 kB) </code>.
	 */
	public static String getHeapSizeMessage() {
		Runtime rt = Runtime.getRuntime();
		long totalMemKB = rt.totalMemory() / 1024;
		long usedMemKB = totalMemKB - rt.freeMemory() / 1024;
		String memStatus = "Memory usage " + usedMemKB + " of " + totalMemKB + " kB ";
		long maxMem = rt.maxMemory();
		if (maxMem < Long.MAX_VALUE) {
			long maxMemKB = maxMem / 1024;
			memStatus += "(= " + (usedMemKB * 1000 / maxMemKB) / 10.0 + "% of JVM growth limit " + maxMemKB + " kB) ";
		}
		return memStatus;
	}
}
