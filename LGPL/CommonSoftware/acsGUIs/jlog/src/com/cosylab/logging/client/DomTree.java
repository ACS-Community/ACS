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
package com.cosylab.logging.client;

import org.w3c.dom.Node;

// For creating a TreeModel
import javax.swing.tree.*;
import javax.swing.event.*;
import java.util.*;
import javax.swing.*;
import java.awt.Component;

/** This class wraps a DOM node and returns the text we want to 
 * display in the tree. It also returns children, index values, 
 * and child counts.
 * 
 * Creation date: (1/30/02 9:57:02 AM)
 * @author: 
 */
public class DomTree extends javax.swing.JTree  { 

	//    static Document document; 

	static final int windowHeight = 460;
	static final int leftWidth = 300;
	static final int rightWidth = 340;
	static final int windowWidth = leftWidth + rightWidth;

	// An array of names for DOM node-types
	// (Array indexes = nodeType() values.)
	static final String[] typeName =
		{
			"none",
			"Element",
			"Attr",
			"Text",
			"CDATA",
			"EntityRef",
			"Entity",
			"ProcInstr",
			"Comment",
			"Document",
			"DocType",
			"DocFragment",
			"Notation",
			};

	private class AdapterNode {
		org.w3c.dom.Node domNode;

		// Construct an Adapter node from a DOM node
		public AdapterNode(org.w3c.dom.Node node) {
			domNode = node;
		}

		// Return a string that identifies this node in the tree
		// *** Refer to table at top of org.w3c.dom.Node ***
		public String toString() {

	        if (domNode == null)
	        	return "No additional info";
			String s = typeName[domNode.getNodeType()];
			String nodeName = domNode.getNodeName();
			if (!nodeName.startsWith("#")) {
				s += ": " + nodeName;
			}
			if (domNode.getNodeValue() != null) {
				if (s.startsWith("ProcInstr"))
					s += ", ";
				else
					s += ": ";
				// Trim the value to get rid of NL's at the front
				String t = domNode.getNodeValue().trim();
				int x = t.indexOf("\n");
				if (x >= 0)
					t = t.substring(0, x);
				s += t;
			}
			return s;
		}

		/*
		 * Return children, index, and count values
		 */
		public int index(AdapterNode child) {
			//System.err.println("Looking for index of " + child);
			if (domNode == null) 
				return 0;
			
			int count = childCount();
			for (int i = 0; i < count; i++) {
				AdapterNode n = this.child(i);
				if (child.domNode == n.domNode)
					return i;
			}
			return -1; // Should never get here.
		}

		public AdapterNode child(int searchIndex) {
			//Note: JTree index is zero-based.
			if (domNode == null)
				return null;
			
			org.w3c.dom.Node node = domNode.getChildNodes().item(searchIndex);
			return new AdapterNode(node);
		}

		public int childCount() {
	        if (domNode == null)
	        	return 0;
	        return domNode.getChildNodes().getLength();
		}
	}

	// This adapter converts the current Document (a DOM) into 
	// a JTree model. 
	private class DomToTreeModelAdapter implements javax.swing.tree.TreeModel {
		private Node document = null;
	    
		// Basic TreeModel operations
		public Object getRoot() {
			return new AdapterNode(document);
		}
		public boolean isLeaf(Object aNode) {
			// Determines whether the icon shows up to the left.
			// Return true for any node with no children
			AdapterNode node = (AdapterNode) aNode;
			if (node == null)
				return true;
			if (node.childCount() > 0)
				return false;
			return true;
		}
		public int getChildCount(Object parent) {
			AdapterNode node = (AdapterNode) parent;
			return node.childCount();
		}
		public Object getChild(Object parent, int index) {
			AdapterNode node = (AdapterNode) parent;
			return node.child(index);
		}
		public int getIndexOfChild(Object parent, Object child) {
			AdapterNode node = (AdapterNode) parent;
			return node.index((AdapterNode) child);
		}
		public void valueForPathChanged(TreePath path, Object newValue) {
			// Null. We won't be making changes in the GUI
			// If we did, we would ensure the new value was really new,
			// adjust the model, and then fire a TreeNodesChanged event.
		}

		/*
		 * Use these methods to add and remove event listeners.
		 * (Needed to satisfy TreeModel interface, but not used.)
		 */
		private List listenerList = new ArrayList();

		public void addTreeModelListener(TreeModelListener listener) {
			if (listener != null && !listenerList.contains(listener)) {
				listenerList.add(listener);
			}
		}
		public void removeTreeModelListener(TreeModelListener listener) {
			if (listener != null) {
				listenerList.remove(listener);
			}
		}

		public void setDocument(Node newDocument) {
	        document = newDocument;
//	        fireTreeStructureChanged(new TreeModelEvent(this, (Object[])null));
		}

		public Node getDocument() {
			return document;
		}
		
		/*
		 * Invoke these methods to inform listeners of changes.
		 * (Not needed for this example.)
		 * Methods taken from TreeModelSupport class described at 
		 *   http://java.sun.com/products/jfc/tsc/articles/jtree/index.html
		 * That architecture (produced by Tom Santos and Steve Wilson)
		 * is more elegant. I just hacked 'em in here so they are
		 * immediately at hand.
		 */
		public void fireTreeNodesChanged(TreeModelEvent e) {
			Iterator it = listenerList.iterator();
			while (it.hasNext()) {
				TreeModelListener listener = (TreeModelListener) it.next();
				listener.treeNodesChanged(e);
			}
		}
		public void fireTreeNodesInserted(TreeModelEvent e) {
			Iterator it = listenerList.iterator();
			while (it.hasNext()) {
				TreeModelListener listener = (TreeModelListener) it.next();
				listener.treeNodesInserted(e);
			}
		}
		public void fireTreeNodesRemoved(TreeModelEvent e) {
			Iterator it = listenerList.iterator();
			while (it.hasNext()) {
				TreeModelListener listener = (TreeModelListener) it.next();
				listener.treeNodesRemoved(e);
			}
		}
		public void fireTreeStructureChanged(TreeModelEvent e) {
			Iterator it = listenerList.iterator();
			while (it.hasNext()) {
				TreeModelListener listener = (TreeModelListener) it.next();
				listener.treeStructureChanged(e);
			}
		}
	}

	public class TheCellRenderer extends DefaultTreeCellRenderer {
		public TheCellRenderer() {
			super();
		}
		public Component getTreeCellRendererComponent(
				JTree tree, 
				Object value, 
				boolean selected, 
				boolean expanded, 
				boolean leaf, 
				int row, boolean hasFocus)
		{
			Component c = super.getTreeCellRendererComponent(
                    tree, value, selected,
                    expanded, leaf, row,
                    hasFocus);
			return this;
		}
	}

private TheCellRenderer cellRenderer = new TheCellRenderer();

/**
 * DomTree constructor comment.
 */
public DomTree() {
	super();
	setModel(new DomToTreeModelAdapter());
	setCellRenderer(cellRenderer);
	ToolTipManager.sharedInstance().registerComponent(this);
}
/**
 * DomTree constructor comment.
 * @param value java.util.Hashtable
 */
public DomTree(java.util.Hashtable value) {
	super(value);
	setCellRenderer(cellRenderer);
	ToolTipManager.sharedInstance().registerComponent(this);
}
  
/**
 * Returns the instance of the document displayed by this tree or null if empty.
 * Creation date: (1/30/02 10:53:29 AM)
 * @return org.w3c.dom.Document
 */
public Node getRootNode() {
	return ((DomToTreeModelAdapter)getModel()).getDocument();
}
/**
 * Insert the method's description here.
 * Creation date: (1/30/02 10:59:08 AM)
 * @param args java.lang.String[]
 */
public static void main(String[] args) {
	try {
		
	JFrame frame = new JFrame();

	DomTree dt = new DomTree();

	frame.getContentPane().setLayout(new java.awt.FlowLayout());
	frame.getContentPane().add(dt);

	} catch (Exception e) {
		e.printStackTrace();
	}
	
}
/**
 * Sets the new document to be displayed by this tree. Can be null to display empty tree.
 * Creation date: (1/30/02 10:46:35 AM)
 * @param newDocument org.w3c.dom.Document
 */
public void setRootNode(Node newDocument) {
	((DomToTreeModelAdapter)getModel()).setDocument(newDocument);
	setCellRenderer(cellRenderer);
	ToolTipManager.sharedInstance().registerComponent(this);
}



}
