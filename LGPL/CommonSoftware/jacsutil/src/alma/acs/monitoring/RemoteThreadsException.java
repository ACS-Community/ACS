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

/**
 * Exception used by the {@link RemoteThreadsClient} class to catch and throw information
 * when errors are present in the execution of the process.
 * @author rtobar
 */
public class RemoteThreadsException extends Exception {

	public RemoteThreadsException(String string, Throwable e) {
		super(string,e);
	}

	public RemoteThreadsException(Throwable e) {
		super(e);
	}
	
	public RemoteThreadsException(String msg) {
		super(msg);
	}
	
	/**
	 * Default serialVersionUID
	 */
	private static final long serialVersionUID = 1L;
	
}
