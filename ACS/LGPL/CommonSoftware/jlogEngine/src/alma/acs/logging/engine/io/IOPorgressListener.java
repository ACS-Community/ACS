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
package alma.acs.logging.engine.io;

/**
 * The listener to be notified about progress while loading or saving
 * 
 * @author acaproni
 *
 */
public interface IOPorgressListener {

	/**
	 * This method notifies about the number of bytes read during a load.
	 * <P>
	 * The value passed as parameter represent the total number of bytes read
	 * since the beginning of the load and includes the bytes of the header
	 *  
	 * @param nBytes The number of bytes read while loading
	 * 
	 */
	public void bytesRead(long nBytes);
	
	/**
	 * This method notifies about the total number of bytes written during a save.
	 * <P>
	 * The value passed as parameter includes the number of the bytes read since the 
	 * beginning of the save.
	 *  
	 * @param nBytes The number of bytes read while loading
	 * 
	 */
	public void bytesWritten(long nBytes);
	
	/**
	 * This method notifies about the total number of logs read during a load.
	 * <P>
	 * The value passed as parameter is the number of logs read since the
	 * beginning of the save.
	 * 
	 * @param numOfLogs
	 */
	public void logsRead(int numOfLogs);
	
	/**
	 * This method notifies about the total number of logs written during a save.
	 * <P>
	 * The value passed as parameter is the number of logs written since the
	 * beginning of the save.
	 * 
	 * @param numOfLogs
	 */
	public void logsWritten(int numOfLogs);
}
