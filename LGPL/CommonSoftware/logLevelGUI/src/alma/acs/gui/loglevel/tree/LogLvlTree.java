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
package alma.acs.gui.loglevel.tree;

import java.awt.Dimension;
import java.util.logging.Logger;

import javax.swing.JTree;

import org.omg.CORBA.ORB;

import alma.acs.gui.loglevel.LogLevelPanel;

/**
 * The tree to display manager, components and container
 * whose log level the user may want to change
 * 
 * @author acaproni
 *
 */
public class LogLvlTree extends JTree {

	private static final long serialVersionUID = -9182797794666037745L;

	// The min width and heigh of the tree
	// See the overridden getPreferredSize
	private static final int MIN_WIDTH=150;
	private static final int MIN_HEIGHT=150;
	
	// The model of this tree
	protected LogLvlTreeModel model;
	
	// The listener for mouse events
	private TreeMouseListener mouseListener;
	
	// The panel showing the tabs
	private LogLevelPanel tabPanel;
	
	/**
	 * Constructor
	 * 
	 * @param panel The panel that shows the tabs
	 * @param theOrn The CORBA ORB
	 * @param theLogger The logger
	 *
	 */
	public LogLvlTree(LogLevelPanel panel, ORB theOrb, Logger theLogger) {
		super();
		if (panel==null) {
			throw new IllegalArgumentException("Invalid null panel of tabs");
		}
		tabPanel=panel;
		model = new LogLvlTreeModel(theOrb,theLogger);
		setModel(model);
		
		setRootVisible(false);
	}
	
	/**
	 * Start the computation
	 * 
	 * @throws Exception if an error happens while starting
	 */
	public void start() throws Exception {
		model.start();
		mouseListener = new TreeMouseListener(this);
		addMouseListener(mouseListener);
	}
	
	/**
	 * Terminate the computation
	 */
	public void stop() {
		removeMouseListener(mouseListener);
		mouseListener=null;
		model.stop();
	}
	
	/**
	 * Return the preferred size of the JTree
	 * This is needed to see the scrollbars:
	 * "JScrollPane basis the desicision to show scroll bars off the views preferred size. 
	 * When you explicitly set the preferred size, and using a JScrollPane, you are effectively 
	 * forcing the size of the JTree to be the preferred size.
	 * If you want to limit the size of the JTree to at least a certain size, 
	 * override getPrefferredSize to return at least a particular value"
	 * 
	 * @see javax.swing.jtree
	 */
	public Dimension getPreferredSize() {
	  Dimension size = super.getPreferredSize();
	  size.width = Math.max(MIN_WIDTH, size.width);
	  size.height = Math.max(MIN_HEIGHT, size.height);
	  return size;
	}

	/**
	 * Getter 
	 * 
	 * @return The panel showing the tabs
	 */
	public LogLevelPanel getTabPanel() {
		return tabPanel;
	}

}
