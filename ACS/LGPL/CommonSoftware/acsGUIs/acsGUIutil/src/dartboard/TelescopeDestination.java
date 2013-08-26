/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package dartboard;

import java.awt.*;

import javax.swing.ImageIcon;

/**
 * Insert the type's description here. Creation date: (11/27/00 9:09:15 PM)
 * 
 * @author: Administrator
 */
public class TelescopeDestination extends Dart {

	// The cross icon
	private ImageIcon icon = null;

	/**
	 * TelescopeDestination constructor comment.
	 */
	public TelescopeDestination() {
		super();
		java.net.URL url = this.getClass().getClassLoader().getSystemResource("dartboard/dest.png");
		if (url != null) {
			icon = new ImageIcon(url);
		} else {
			System.err.println("Destination icon is null");
		}
	}

	protected void drawDart(Graphics g) {
		if (icon != null) {
			icon.paintIcon(dartboard, g, -icon.getIconWidth() / 2, -icon.getIconHeight() / 2);
		}
		else {
			g.setColor(Color.yellow);
			g.fillRect(-10, -10, 20, 20);
		}
	}

	public void setError(boolean err) {
	}
}
