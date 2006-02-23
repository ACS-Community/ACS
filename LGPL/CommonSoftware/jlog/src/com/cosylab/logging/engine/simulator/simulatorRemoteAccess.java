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
package com.cosylab.logging.engine.simulator;

import com.cosylab.logging.engine.*;
import com.cosylab.logging.LCEngine;
import java.util.Random;
/**
 * Simulator for remote access that initializes, runs and destroys the engine.
 * Creation date: (11/22/2001 2:13:12 PM)
 * @author: 
 */
public class simulatorRemoteAccess extends Thread implements com.cosylab.logging.engine.RemoteAccess {
	private boolean isInitalized = false;
	private LCEngine engine;
	private Random random;
/**
 * simulatorRemoteAccess constructor comment.
 */
public simulatorRemoteAccess(LCEngine engine) {
	this.engine = engine;
	random = new Random();
}
/**
 * destroy method comment.
 */
public void destroy() {
	isInitalized = false;
}
/**
 * initialize method comment.
 */
public void initialize() {
/*
	try {
//	engine.addFilter(new Filter(LogEntry.FIELD_ENTRYTYPE, true, new Short(LogEntry.ENTRYTYPE_EMERGENCY)));

//	engine.addFilter(new Filter(LogEntry.FIELD_ENTRYTYPE, true, 
//		new Short(LogEntry.ENTRYTYPE_INFO),new Short(LogEntry.ENTRYTYPE_ERROR)));

	engine.addFilter(new Filter(LogEntry.FIELD_STACKID, true, "Ex*rmi*t?r*"));

//	engine.addFilter(new Filter(LogEntry.FIELD_ENTRYTYPE, true, new Short(LogEntry.ENTRYTYPE_ERROR), null));

//	engine.addFilter(new Filter(LogEntry.FIELD_ENTRYTYPE, true, null, new Short(LogEntry.ENTRYTYPE_ERROR)));
	
	} catch (InvalidFilterConstraintException ifce) {
		System.out.println("Exception in simulatorRemoteAccess::initialize(): " + ifce);
	}
*/
	isInitalized = true;
	start();
}

public boolean isConnected() {
	return true;
}
/**
 * isInitialized method comment.
 */
public boolean isInitialized() {
	return isInitalized;
}
public void run() {
	try {
//		sleep(5000);
		while (isInitalized) {
//			System.out.println(">sim< A Random Log is being sent.");
			engine.pushStructuredEvent(LogEntry.generateRandomLog(random));
			sleep(300*(random.nextInt(5)));
		}
	} catch (InterruptedException e) {
	}
}
}
