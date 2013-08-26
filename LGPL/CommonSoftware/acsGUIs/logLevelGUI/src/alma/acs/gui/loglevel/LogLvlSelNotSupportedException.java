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
package alma.acs.gui.loglevel;

/**
 * The exception thrown when an object (for example a Container) does not
 * support the selection of the log levels
 * 
 * @author acaproni
 *
 */
public class LogLvlSelNotSupportedException extends Exception {

	private static final long serialVersionUID = -5262248034684925612L;

	/**
	 * Constructor
	 * 
	 * @param msg The message of the exception
	 */
	public LogLvlSelNotSupportedException(String msg) {
		super(msg);
	}
	
	/**
	 * Constructor
	 * 
	 * @param msg The message of the exception
	 */
	public LogLvlSelNotSupportedException(String msg, Throwable t) {
		super(msg,t);
	}
}
