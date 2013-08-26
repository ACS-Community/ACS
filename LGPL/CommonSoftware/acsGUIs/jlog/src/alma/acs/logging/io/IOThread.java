/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.logging.io;

/**
 * The thread to perform IO.
 * <P>
 * 
 * 
 * @author acaproni
 *
 */
public class IOThread extends Thread {
	
	/**
	 * Constructor
	 */
	public IOThread() {
		setDaemon(true);
		setName("IOThread");
	}
	
	/**
	 * The boolean set to <code>true</code> to terminate the thread.
	 */
	protected volatile boolean terminateThread=false;
	
	/**
	 * Stop the current thread by setting a boolean.
	 * <P>
	 * <B>Note</B>: the thread has to periodically check the boolean and terminate
	 * 
	 * @return
	 */
	public void stopThread(boolean sync) {
		terminateThread=true;
		interrupt();
		while (sync && isAlive()) {
			try {
				join();
			} catch (InterruptedException e) {
				continue;
			}
		}
	}
}
