/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
package com.cosylab.acs.laser;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Logger;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.acsnc.EventDescription;

import com.cosylab.acs.jms.ACSJMSMessageEntity;

/** 
 * AlarmSourcesListenerCached extends AlarmSourcesListener to decouple the 
 * receiving of alarms from the processing speed of the alarm service.
 * <P>
 * Each alarm received from the NCs is immediately received and pushed in the cache.
 * Another threads pops alarms from the cache and inject in the alarm service.
 * <P>
 * We do not want to lose alarms for any reason so if the queue is full we wait until 
 * the thread picks an item out of it.
 * <P>
 * {@link #start()} must be called at the beginning and {@link #shutdown()} when terminated
 * using objects of this class.
 * 
 * @author  acaproni
 * @version $Id: AlarmSourcesListenerCached.java,v 1.1 2012/12/07 17:55:53 acaproni Exp $
 * @since ACS-11.0
 */
public class AlarmSourcesListenerCached extends AlarmSourcesListener implements Runnable {
	
	/**
	 * Max number of items in queue
	 */
	private final int MAX_QUEUE_SIZE=15000;
	
	/**
	 * The thread of this class;
	 */
	private final Thread thread;
	
	/**
	 * The boolean to signal the thread to terminate
	 */
	private volatile boolean terminateThread=false; 
	
	/**
	 * The cache where the <code>AlarmSourcesListenerCached</code> pushed the alarms 
	 * it receives from the sources NCs.
	 */
	private final LinkedBlockingQueue<String> queue = new LinkedBlockingQueue<String>(MAX_QUEUE_SIZE);
	
	/**
	 * Constructor. 
	 * 
	 * @param contSvcs Alarm service container services
	 * @param logger The logger
	 */
	public AlarmSourcesListenerCached(ContainerServicesBase contSvcs, Logger logger) {
		super(contSvcs,logger);
		thread= new Thread(this, this.getClass().getName());
	}
	
	/**
	 * Constructor.
	 * 
	 * @param contSvcs Alarm service container services
	 * @param logger The logger
	 * @param listener the listener to notify messages to
	 */
	public AlarmSourcesListenerCached(ContainerServicesBase contSvcs, Logger logger, SourceListener listener) {
		super(contSvcs,logger,listener);
		thread= new Thread(this, this.getClass().getName());
	}
	
	
	
	public void start() {
		thread.setDaemon(true);
		thread.start();
	}
	
	@Override
	public void shutdown() {
		logger.log(AcsLogLevel.DEBUG,"AlarmSourceListenerCached shutting down");
		terminateThread=true;
		thread.interrupt();
		super.shutdown();
		logger.log(AcsLogLevel.DEBUG,"AlarmSourceListenerCached shut down");
	}

	@Override
	public void run() {
		long lastTimeUpdate=System.currentTimeMillis();
		while (!terminateThread) {
			try {
				String xml = queue.take();
				if (xml != null) {
					notifyListeners(xml);
				}
			} catch (Throwable t) {
				System.err.println("Cache pop error");
				t.printStackTrace();
			}
			if (System.currentTimeMillis()-lastTimeUpdate>60000) {
				logger.log(AcsLogLevel.DEBUG,"Queued source alarm messages waiting to be processed "+queue.size());
				lastTimeUpdate=System.currentTimeMillis();
			}
		}
		logger.log(AcsLogLevel.DEBUG,"AlarmSourceListenerCached thread terminated");
	}

	/**
	 * Each alarm is pushed into the alarm service unless there are no alarms in cache
	 */
	@Override
	public void receive(ACSJMSMessageEntity message,
			EventDescription eventDescrip) {
		if (message==null) {
			throw new NullPointerException("The message received is null");
		}
		// Extract the XML and cache it
		if (message.text!=null && !message.text.isEmpty()) {
			try {
				// It waits if the queue is full!
				queue.put(message.text);
			} catch (InterruptedException ie) {}
		}
	}
}
