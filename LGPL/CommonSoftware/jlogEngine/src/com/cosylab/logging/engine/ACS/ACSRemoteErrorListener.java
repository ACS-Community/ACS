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
package com.cosylab.logging.engine.ACS;

/**
 * This interface defines the listener to be notified
 * of errors encountered getting logs from the NC
 * 
 * @author acaproni
 *
 */
public interface ACSRemoteErrorListener {
	
	/**
	 * The method is executed when an error happened building a log
	 * from a string.
	 * 
	 * The parameter is the string that caused the error and it can be
	 *  - the XML that was not possible to parse
	 *  - a cache string if the binary format of logs is in use
	 *  
	 *  The cache string is a string used in the cache whose format might change
	 *  with different implementation of of the cache.
	 *  
	 * 
	 * 
	 * 
	 * @param xml That string that caused the error
	 */
	public void errorReceived(String xml);
}
