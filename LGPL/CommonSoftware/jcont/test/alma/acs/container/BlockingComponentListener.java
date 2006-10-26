/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.container;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import alma.acs.component.ComponentDescriptor;

/**
 * Test helper class that allows to wait until notification about unexpected component (un)availability has been received.
 * @author hsommer
 */
public class BlockingComponentListener implements ContainerServices.ComponentListener {
	private final Logger logger;
	private final List<ComponentDescriptor> allCompsAvailable;
	private final List<String> allCompNamesUnavailable;
	private volatile CountDownLatch sync;
	
	public BlockingComponentListener(Logger logger) {
		this.logger = logger;
		allCompsAvailable = new ArrayList<ComponentDescriptor>();
		allCompNamesUnavailable = new ArrayList<String>();
	}
	
	public boolean includeForeignComponents() {
		return false;
	}

	public void componentsAvailable(List<ComponentDescriptor> comps) {
		logger.info("************* Got call to componentsUnavailable ***********");
		if (sync != null) {
			sync.countDown();
		}
		allCompsAvailable.addAll(comps);
	}
	
	public void componentsUnavailable(List<String> compNames) {
		logger.info("************* Got call to componentsUnavailable ***********");
		if (sync != null) {
			sync.countDown();
		}
		allCompNamesUnavailable.addAll(compNames);
	}
	
	public void clearAndExpect(int nCalls) {
		allCompsAvailable.clear();
		allCompNamesUnavailable.clear();
		sync = new CountDownLatch(nCalls);
	}
	
	/**
	 * Waits until <code>nCalls</code> notifications have been received since the call to <code>clearAndExpect(nCalls)</code>.
	 */
	public boolean awaitNotifications(long timeout, TimeUnit unit) throws InterruptedException {		
		return sync.await(timeout, unit);
	}
	
	List<ComponentDescriptor> getAllCompsAvailable() {
		return allCompsAvailable;
	}
	
	List<String> getAllCompNamesUnavailable() {
		return allCompNamesUnavailable;
	}
}