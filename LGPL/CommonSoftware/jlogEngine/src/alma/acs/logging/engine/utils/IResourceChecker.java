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
 * An interface for getting system resource.
 * <P>
 * Different implementations of this interface can be provided for testing
 * puprposes.
 * 
 * @author acaproni
 *
 */
public interface IResourceChecker {
	
	/**
	 * The memory allocated that can be used by the application
	 * taking into account the memory not yet allocated by the JVM.
	 * Measured in bytes.
	 * 
	 * @see {@link Runtime} 
	 */
	public long getTotalFreeMemory();
}
