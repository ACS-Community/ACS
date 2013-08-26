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
package alma.acsplugins.alarmsystem.gui;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

/** 
 * @author  acaproni
 * @version $Id: NoAcsPanel.java,v 1.1 2012/11/19 09:48:03 acaproni Exp $
 * @since    
 */

/**
 * This panel is show initially before the ORB and Logger and injected
 * by the OMC or by ContainerServices if running in stand alone mode.
 * <P>
 * The panel shows a label explaining that it is not yet started.
 */
class NoAcsPanel extends JPanel {
	
	private final JLabel noACSLbl = new JLabel("Panel not yet initialized to work against ACS.");
	
	public NoAcsPanel() {
		SwingUtilities.invokeLater(new Runnable() {
			
			@Override
			public void run() {
				add(noACSLbl);
			}
		});
	}
}