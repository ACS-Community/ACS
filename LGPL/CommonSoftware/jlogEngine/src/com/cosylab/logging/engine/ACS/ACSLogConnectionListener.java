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
 * The listener for the status of the connection with the 
 * logging channel
 * 
 * @author acaproni
 *
 */
public interface ACSLogConnectionListener {
	/** 
	 * Notify that the connection with ACS NC has been established
	 */
	public void acsLogConnEstablished();
	
	/**
	 * Notify that the connection with ACS NC has been disconnected
	 * It can happen as a consequence of an error as well as as consequence
	 * of a request 
	 */
	public void acsLogConnDisconnected();
	
	/**
	 * Notify that the connection with ACS NC has been lost
	 * (it means an error or something abnormal).
	 *
	 */
	public void acsLogConnLost();
	
	/**
	 * Notify that an attempt to connect to ACS NC is in progress
	 *
	 */
	public void acsLogConnConnecting();
	
	/**
	 * Notify that the service is supended 
	 * (i.e. it is connected to the NC and receiving logs
	 * but the logs are discarded instead of being sent to the 
	 * listeners)
	 * Note: the suspension of the service is not a malfunctioning
	 *       but a status requested by the user
	 */
	public void acsLogConnSuspended();
	
	/**
	 * Notify that for some internal reason the service is not able
	 * to follow the flow of the incoming logs and is queueing
	 * the messages to be inserted later, when flow will decrease and 
	 * enough CPU time is availbale 
	 * 
	 * This method is not executed each time a log is queued but
	 * once when the situation begins.
	 * When the temporary problem has been fixed, the status will revert
	 * to connected
	 */
	public void acsLogsDelay();
	
	/**
	 * Send a report string with the current status of the connection
	 * 
	 * @param status The status string
	 */
	public void reportStatus(String status);
}
