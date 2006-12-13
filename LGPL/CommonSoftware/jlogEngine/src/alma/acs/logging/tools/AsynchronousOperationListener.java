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
package alma.acs.logging.tools;

public interface AsynchronousOperationListener {
	
	/**
	 * Notify the listener that an async operation is started
	 * 
	 * @param ID The ID of the async operation
	 */
	public void operationStarted(int ID);
	
	/**
	 * Called to notify about the termination of an asynchronous operation
	 * @param The identifier of the async operation
	 */
	public void operationTerminated(int ID);
	
	/**
	 * Notify the listener about an error
	 * 
	 * @param e The exception describing the error
	 * @param ID The identifier of the async operation
	 */
	public void errorDetected(Exception e, int ID);
	
	/**
	 * Notify the listener about the progress of the current async
	 * operation
	 * 
	 * @param The start value of the operation
	 * @param The target of the operation
	 * @param The current position
	 * @param ID The identifier of the async operation
	 * 
	 */
	public void operationProgress(long start, long end, long current, int ID);
	
}
