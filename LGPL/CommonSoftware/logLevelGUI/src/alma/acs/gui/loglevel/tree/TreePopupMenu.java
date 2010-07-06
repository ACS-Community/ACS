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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

/**
 * The popup menu for the LogLevelTree
 * 
 * @author acaproni
 *
 */
public class TreePopupMenu extends JPopupMenu implements ActionListener {

	private static final long serialVersionUID = -8975189067722437194L;

	// The items in the popup menu
	private JMenuItem refreshTreeMI = new JMenuItem("Refresh");

	// yatagai: not used any more   
	private JCheckBoxMenuItem viewClientsCMI = new JCheckBoxMenuItem("View clients node");
	private JCheckBoxMenuItem viewComponentsCMI = new JCheckBoxMenuItem("View components node");
	
	// The model of the tree
	private LogLvlTreeModel model;
	
	/**
	 * Constructor
	 *
	 */
	public TreePopupMenu(LogLvlTreeModel model) {
		super();
		if (model==null) {
			throw new IllegalArgumentException("Invalid null LogLvlModel in constructor");
		}
		this.model=model;
		initialize();
	}
	
	/**
	 * Init the menu
	 *
	 */
	private void initialize() {
		// Insert the items
		add(refreshTreeMI);

		// yatagai: removed
		// add(new JPopupMenu.Separator());
		// add(viewClientsCMI);
		// add(viewComponentsCMI);
		
		// Connect listeners
		refreshTreeMI.addActionListener(this);
		viewClientsCMI.addActionListener(this);    // yatagai: not used any more   
		viewComponentsCMI.addActionListener(this); // yatagai: not used any more   
		
		// Clients and components are visible at startup
		viewClientsCMI.setSelected(true);         // yatagai: not used any more   
		viewComponentsCMI.setSelected(true);      // yatagai: not used any more   
		
		setLabel("Options");
		setBorderPainted(true);
		pack();
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==refreshTreeMI) {
			model.refreshTree();
		} else if (e.getSource()==viewClientsCMI) { // yatagai: not used any more   
			model.showClients(viewClientsCMI.isSelected());
		} else if (e.getSource()==viewComponentsCMI) { // yatagai: not used any more   
			model.showComponents(viewComponentsCMI.isSelected());
		}
	}
}
