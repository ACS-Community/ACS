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

package alma.acs.container.corba;

import java.util.logging.Logger;

import org.jacorb.orb.acs.AcsORBProfiler;

/**
 * Currently just a dummy impl.
 * <p>
 * @TODO: Check with manager, CDB etc about reusing some common base class.
 * @author hsommer
 */
public class ContainerOrbProfiler implements AcsORBProfiler
{
	private final Logger logger;

	public ContainerOrbProfiler(Logger logger) {
		this.logger = logger;
	}
	
	@Override
	public void connectionThreadPoolSizeChanged(int idleThreads, int totalThreads, int maxThreads) {
		// TODO Auto-generated method stub
//		int freeThreadsPrecentage = (int) (((totalThreads - idleThreads) / (double) maxThreads) * 100);
//		manager.setThreadUsage(freeThreadsPrecentage);
	}
	
	@Override
	public void undeliveredRequest(int messageSize, String poaName, String operation, boolean causedByQueueFull) {
		// TODO Auto-generated method stub
	}

	@Override
	public void requestQueueSizeChanged(int requestId, String poaName, int queueSize, int maxQueueLength) {
		// TODO Auto-generated method stub
	}

	@Override
	public void threadPoolSizeChanged(String poaName, int idleThreads, int totalThreads, int maxThreads) {
		// TODO Auto-generated method stub
	}

	@Override
	public void requestStarted(int requestId, String poaName, String operation) {
		// TODO Auto-generated method stub
	}

	@Override
	public void requestFinished(int requestId, String poaName, String operation) {
		// TODO Auto-generated method stub
	}
}
