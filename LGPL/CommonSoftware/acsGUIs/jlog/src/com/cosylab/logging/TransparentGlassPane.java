/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
package com.cosylab.logging;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JComponent;
import javax.swing.SwingUtilities;

/**
 * The semi-transparent GlassPane visible when jlog runs offline.
 * The purpose of this panel is to color the windows with a light gray.
 * <P>
 * When the component is made visible, it is possible to set one and
 * only one component of the content pane to receive mouse events.
 * This is especially useful when showing an error panel that the user
 * must explicitly acknowledge.
 * 
 * @author acaproni
 *
 */
public class TransparentGlassPane extends JComponent implements MouseListener{
	
	/**
	 * The content pane below this glass pane
	 */
	private final Container contentPane;
	
	/**
	 * The component receiving mouse events.
	 */
	private Component mouseEvtComponent=null;
	
	public TransparentGlassPane(Container c) {
		contentPane=c;
	}

	/**
	 * Paint the panel in a light gray.
	 */
	 public void paint(Graphics g) {
         Graphics2D g2D = (Graphics2D)g;
         g2D.setColor(Color.black);
         g2D.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER , 0.05f));
         g2D.fillRect(0, 0, getWidth(), getHeight());
     }

	@Override
	public void mouseClicked(java.awt.event.MouseEvent e) {
		redispatchMouseEvent(e, true);
	}

	@Override
	public void mouseEntered(java.awt.event.MouseEvent e) {
		redispatchMouseEvent(e, true);
	}

	@Override
	public void mouseExited(java.awt.event.MouseEvent e) {
		redispatchMouseEvent(e, true);
	}

	@Override
	public void mousePressed(java.awt.event.MouseEvent e) {
		redispatchMouseEvent(e, true);
	}

	@Override
	public void mouseReleased(java.awt.event.MouseEvent e) {
		redispatchMouseEvent(e, true);
	}
	
	private void redispatchMouseEvent(MouseEvent e, boolean repaint) {
		Point glassPanePoint = e.getPoint();
		Container container = contentPane;
		Point containerPoint = SwingUtilities.convertPoint(this,
				glassPanePoint, contentPane);

		if (containerPoint.y < 0) { // we're not in the content pane
		// Could have special code to handle mouse events over
		// the menu bar or non-system window decorations, such as
		// the ones provided by the Java look and feel.
		} else {
			// The mouse event is probably over the content pane.
			// Find out exactly which component it's over.
			Component component = SwingUtilities.getDeepestComponentAt(
					container, containerPoint.x, containerPoint.y);

			if (component==null) {
				return;
			}
			
			if (mouseEvtComponent==null || component.equals(mouseEvtComponent)) {
				// Forward events over the check box.
				Point componentPoint = SwingUtilities.convertPoint(this,
						glassPanePoint, component);
				component
						.dispatchEvent(new MouseEvent(component, e.getID(), e
								.getWhen(), e.getModifiers(), componentPoint.x,
								componentPoint.y, e.getClickCount(), e
										.isPopupTrigger()));
			}
		}

		// Update the glass pane if requested.
//		if (repaint) {
//			this.setPoint(glassPanePoint);
//			this.repaint();
//		}
	}
	
	/**
	 * Show/hide the component
	 * 
	 * @param b if <code>true</code> make the glass pane visible
	 * @param comp If not <code>null</code>, mouse events are forwarded only to the
	 * 			comp component; otherwise all the events are forwarded to the
	 * 			components of the content pane;
	 */
	public void setVisible(boolean b, Component comp) {
		setEventComponent(comp);
		super.setVisible(b);
	}
	
	/**
	 * Set the component to receive mouse events.
	 * <P>
	 * If a component is set, it is the only one receiving mouse events.
	 * This means that all the other componets of the content pane are
	 * disabled.
	 * 
	 * @param comp the component to receive mouse events;
	 * 		if <code>null</code> the mouse events are forwarded to the 
	 * 		components of the content pane
	 */
	public void setEventComponent(Component comp) {
		mouseEvtComponent=comp;
		if (comp==null) {
			removeMouseListener(this);
		} else {
			addMouseListener(this);
		}
	}

}
