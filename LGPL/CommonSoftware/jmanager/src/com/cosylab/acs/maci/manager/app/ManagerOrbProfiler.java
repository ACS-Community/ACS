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

package com.cosylab.acs.maci.manager.app;

import com.cosylab.acs.maci.manager.ManagerImpl;

import alma.acs.logging.AcsLogger;
import alma.acs.profiling.orb.AcsORBProfilerImplBase;

/**
 * @author hsommer
 */
public class ManagerOrbProfiler extends AcsORBProfilerImplBase
{
	private final ManagerImpl manager;
	
	/**
	 * @param manager Callback to the manager, to notify it about resource shortages.
	 * @param logger Logger to be used by this class.
	 */
	public ManagerOrbProfiler(ManagerImpl manager, AcsLogger logger) {
		super(logger);
		this.manager = manager;
	}
	
	@Override
	public void connectionThreadPoolSizeChanged(int idleThreads, int totalThreads, int maxThreads) {
		super.connectionThreadPoolSizeChanged(idleThreads, totalThreads, maxThreads);
		
		// TODO: make AcsORBProfilerImplBase#connectionPoolUsePercent protected in the base class and use it instead of computing it again.
		// For now we don't want to make jmanager depend on jacsutil2 changes on the HEAD, to make patching jmanager easier for scalability tests.
		int connectionPoolUsePercent = (int) (((totalThreads - idleThreads) / (double) maxThreads) * 100);
		
		manager.setConnectionThreadUsage(connectionPoolUsePercent);
	}
		
//	@Override
//	public void undeliveredRequest(int messageSize, String poaName, String operation, boolean causedByQueueFull) {
//		// TODO Auto-generated method stub
//	}
//
//	@Override
//	public void requestQueueSizeChanged(int requestId, String poaName, int queueSize, int maxQueueLength) {
//		// TODO Auto-generated method stub
//	}
//
//	@Override
//	public void threadPoolSizeChanged(String poaName, int idleThreads, int totalThreads, int maxThreads) {
//		// TODO Auto-generated method stub
//	}
//
//	@Override
//	public void requestStarted(int requestId, String poaName, String operation) {
//		// TODO Auto-generated method stub
//	}
//
//	@Override
//	public void requestFinished(int requestId, String poaName, String operation) {
//		// TODO Auto-generated method stub
//	}
}
