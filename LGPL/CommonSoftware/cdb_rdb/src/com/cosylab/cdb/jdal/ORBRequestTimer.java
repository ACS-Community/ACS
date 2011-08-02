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

package com.cosylab.cdb.jdal;

import java.util.HashMap;
import java.util.logging.Logger;

import org.jacorb.orb.acs.AcsORBProfiler;

import alma.acs.logging.AcsLogLevel;

/**
 * Profiler implementation that monitors request duration.
 * @author msekoran
 */
public class ORBRequestTimer implements AcsORBProfiler
{
	private final Logger logger;

	public ORBRequestTimer(Logger logger) {
		this.logger = logger;
	}
	
	public void connectionThreadPoolSizeChanged(int idleThreads, int totalThreads, int maxThreads) {
		// TODO Auto-generated method stub
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

	static final class ThreadRequestId {
		private long threadId;
		private int requestId;
		public ThreadRequestId(long threadId, int requestId) {
			this.threadId = threadId;
			this.requestId = requestId;
		}
		@Override
		public int hashCode() {
			return (int)threadId * 911 + requestId;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			ThreadRequestId other = (ThreadRequestId) obj;
			if (requestId != other.requestId)
				return false;
			if (threadId != other.threadId)
				return false;
			return true;
		}
	}
	HashMap<ThreadRequestId, Long> requestTimeMap = new HashMap<ThreadRequestId,Long>();

	@Override
	public void requestStarted(int requestId, String poaName, String operation) {
		logger.log(AcsLogLevel.INFO, "requestStarted(" + requestId + ", " + poaName + ", " + operation + ")");
		synchronized (requestTimeMap) {
			requestTimeMap.put(new ThreadRequestId(Thread.currentThread().getId(), requestId), System.currentTimeMillis());
		}
	}

	@Override
	public void requestFinished(int requestId, String poaName, String operation) {
		synchronized (requestTimeMap) {
			Long startTime = requestTimeMap.remove(new ThreadRequestId(Thread.currentThread().getId(), requestId));
			if (startTime != null)
			{
				long timeSpent = System.currentTimeMillis() - startTime.longValue();
				logger.log(AcsLogLevel.INFO, "requestFinished(" + requestId + ", " + poaName + ", " + operation + ") in " + timeSpent + " ms");
			}
			else
				logger.log(AcsLogLevel.INFO, "requestFinished(" + requestId + ", " + poaName + ", " + operation + ")");
		}
	}
}
