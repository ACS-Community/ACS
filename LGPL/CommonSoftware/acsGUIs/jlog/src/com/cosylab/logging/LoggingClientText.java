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
package com.cosylab.logging;

import com.cosylab.logging.engine.ACS.LCEngine;

/**
 * This class is not used for testing. It sets access type "ACS" to the LCEngine.
 * Creation date: (11/2/2001 10:42:40 AM)
 * @author: 
 */
public class LoggingClientText {
	public LCEngine lct = null;
	private RemoteResponseCallbackText rrct = null;
/**
 * LoggingClientText constructor comment.
 */
public LoggingClientText() {
	this("ACS");
}
/**
 * LoggingClientText constructor comment.
 */
public LoggingClientText(String accessType) {
	initialize(accessType);
}
/**
 * Insert the method's description here.
 * Creation date: (11/2/2001 10:43:03 AM)
 */
private void initialize(String accessType) {
	
	rrct = new RemoteResponseCallbackText();
	lct = new LCEngine();
	lct.addLogConnectionListener(rrct);
	lct.addLogListener(rrct);

	lct.setAccessType("ACS");
	lct.connect();
	try {
		Thread.sleep(20000);
	} catch (Exception e) {
	}
	System.out.println("Attemping to destroy...");
	lct.disconnect();
	
}
/**
 * Starts the application.
 * @param args an array of command-line arguments
 */
public static void main(java.lang.String[] args) {
//	LoggingClientText lct = new LoggingClientText();
	LoggingClientText lct = new LoggingClientText("ACS");
}
}
