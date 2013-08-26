/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.tree;

import javax.swing.ImageIcon;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeNode;

import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acs.gui.util.threadsupport.EDTExecutor;
import cern.laser.client.data.Alarm;

/**
 * The tree of reduced alarms
 * 
 * @author acaproni
 *
 */
public class AlarmTree extends JTree {
	
	/**
	 * The root node
	 */
	private final DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode();
	
	/**
	 * The model of the tree
	 */
	private final AlarmTreeModel model=new AlarmTreeModel(rootNode);
	
	/**
	 * Constructor
	 */
	public AlarmTree() {
		initGUI();
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initGUI() {
		EDTExecutor.instance().execute(new Runnable() {
			public void run() {
				setModel(model);
				setEditable(false);
				setRootVisible(false);
				
				DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
				ImageIcon leafIcon=new ImageIcon(AlarmGUIType.class.getResource(AlarmGUIType.iconFolder+"arrow_in.png"));
				ImageIcon icon=new ImageIcon(AlarmGUIType.class.getResource(AlarmGUIType.iconFolder+"add.png"));
				renderer.setLeafIcon(leafIcon);
				renderer.setOpenIcon(icon);
				renderer.setClosedIcon(icon);
				setCellRenderer(renderer);		
			}
		});
	}
	
	/**
	 * Add a child to the parent.
	 * The content of the child is given by the passed alarm.
	 * 
	 * @param al The alarm to set as content
	 * @param parent The parent
	 * @return The newly added {@link TreeNode}
	 */
	public DefaultMutableTreeNode add(Alarm al, DefaultMutableTreeNode parent) {
		return model.addChild(al, parent);
	}
	
	/**
	 * Clear the tree setting the passed alarm as root
	 * 
	 * @param al The alarm to set as root
	 */
	public void clear(Alarm al) {
		model.clearTree(al);
	}
	
}
