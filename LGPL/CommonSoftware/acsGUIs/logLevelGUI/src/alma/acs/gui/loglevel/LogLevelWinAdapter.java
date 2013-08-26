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
package alma.acs.gui.loglevel;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * A class to receive window events
 * 
 * @author acaproni
 *
 */
public class LogLevelWinAdapter extends WindowAdapter {
	
	// The frame whose events this object receives
	private LogLevelFrame frame;
	
	public LogLevelWinAdapter(LogLevelFrame frame) {
		if (frame==null) {
			throw new IllegalArgumentException("Invalid null LogLevelFrame");
		}
		this.frame=frame;
	}
	
	public void windowClosing(WindowEvent e) {
		frame.stopPanel();
		frame.disconnectACSComponentClient();
		frame.dispose();
	}
}
