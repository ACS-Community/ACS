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
package alma.acs.logging.dialogs.main;

import java.awt.event.WindowEvent;
import java.util.logging.Logger;

import alma.acs.shutdown.ShutdownHookBase;

/**
 * The object from this class intercepts the Ctrl+C event and cleanly
 * close the application
 * 
 * @author acaproni
 *
 */
public class ShutdownHook extends ShutdownHookBase {
	
	// The LogFrame
	private LogFrame logFrame;

	public ShutdownHook(Logger logger, String processName, LogFrame log) {
		super(logger, processName);
		if (log==null) {
			throw new IllegalArgumentException("The LogFrame can't be null");
		}
		logFrame=log;
	}

	@Override
	protected void interruptDetected() {
		WindowEvent wEvt = new WindowEvent(logFrame,WindowEvent.WINDOW_CLOSING);
		logFrame.dispatchEvent(wEvt);
	}

}
