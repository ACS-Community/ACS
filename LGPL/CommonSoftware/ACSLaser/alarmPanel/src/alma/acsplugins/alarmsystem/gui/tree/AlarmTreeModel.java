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

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import cern.laser.client.data.Alarm;

/**
 * The model for the tree of reduced alarms
 * 
 * @author acaproni
 *
 */
public class AlarmTreeModel extends DefaultTreeModel {
	
	/**
	 * The content of each node
	 * <P>
	 * The purpose of this class is to show only the ID of each
	 * alarm in every node
	 * 
	 * @author acaproni
	 *
	 */
	public class NodeContent {
		/**
		 * The alarm whose ID is show in a node
		 */
		private final Alarm alarm;
		
		/**
		 * Constructor
		 * 
		 * @param al The alarm shown in the node
		 */
		public NodeContent(Alarm al) {
			if (al==null) {
				throw new IllegalArgumentException("The content can't be null");
			}
			alarm=al;
		}
		
		@Override
		public String toString() {
			StringBuilder str = new StringBuilder(alarm.getTriplet().getFaultMember());
			str.append(' ');
			str.append(alarm.getTriplet().getFaultCode());
			str.append(": ");
			str.append(alarm.getProblemDescription());
			return str.toString();
		}
	}
	
	/**
	 * constructor
	 * 
	 * @param rootNode The root node
	 */
	public AlarmTreeModel(DefaultMutableTreeNode rootNode) {
		super(rootNode);
	}
	
	/**
	 * Add an alarm as a child of the passed node
	 * 
	 * @param al The alarm to add
	 * @param parent The parent node;
	 * 				if it is <code>null</code> the child is added to the root node
	 * @return The newly added node
	 */
	public DefaultMutableTreeNode addChild(Alarm al, DefaultMutableTreeNode parent) {
		if (al==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		if (parent == null) {
			parent=(DefaultMutableTreeNode)root;
		}
		DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(new NodeContent(al));
		insertNodeInto(newNode, parent, 0);
		nodeStructureChanged(parent);
		return newNode;
	}
	
	/**
	 * Empty the tree and set the passed alarm as the root 
	 * 
	 * @param label The string to set as label of the root node
	 */
	public void clearTree(Alarm al) {
		for (int t=0; t<root.getChildCount(); t++) {
			DefaultMutableTreeNode node = (DefaultMutableTreeNode)root.getChildAt(t);
			removeNodeFromParent(node);
		}
		((DefaultMutableTreeNode)root).setUserObject(al);
		nodeStructureChanged(root);
	}
}
