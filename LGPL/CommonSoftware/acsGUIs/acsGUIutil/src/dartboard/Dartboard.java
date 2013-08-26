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

import java.awt.event.*;
import javax.swing.*;

public class Dartboard extends JPanel {
	private Dart[] darts;
	
	private class ComponentHandler implements ComponentListener {
		public void componentResized(ComponentEvent e) {
			if (darts != null)
				for (int i = 0; i < darts.length; i++)
					darts[i].setDartboardSize(getSize());
		}
		public void componentMoved(ComponentEvent e) {
		}
		public void componentHidden(ComponentEvent e) {
		}
		public void componentShown(ComponentEvent e) {
		}
	}
public Dartboard() {
	super();
	initialize();
}
private void initialize() {
	addComponentListener(new ComponentHandler());
}
public void paintComponent(java.awt.Graphics g) {
	super.paintComponent(g);

	// draw all the darts
	if (darts != null) {
		for (int i = 0; i < darts.length; i++) {
			darts[i].draw(g);
		}
	}
	
	
}
protected void setDarts(Dart[] darts) {
	this.darts = darts;
	for (int i = 0; i < darts.length; i++) {
		darts[i].setDartboard(this);
	}
	repaint();
}
}