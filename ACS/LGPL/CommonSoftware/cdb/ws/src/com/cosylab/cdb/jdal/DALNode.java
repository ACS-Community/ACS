package com.cosylab.cdb.jdal;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * Created on Jun 29, 2003
 * This class represents a node in a small tree model.
 * The node has name and eventually child list.
 * The <code>ArrayList</code> is used for child storage
 * and not a <code>HashMap</code> since it is expected that
 * number of childs is quite small.
 * The <code>loadNodes</code> curently scans file system at
 * given path to create the tree but it can be overriden for
 * differnt needs. 
 * This will be mainly used for the <code>DAL::list_nodes</code>
 * but it is generic enough for other purposes.
 * jDAL implementation will construct a root node by static getRoot()
 * function which will scan given file path and fill hierarchy of
 * nodes (curls) that jDAL can return. 
 * 
 * @author dvitas
 *
 */

public class DALNode {
	protected String name; // the name of this node.
	protected ArrayList childs; // the childs if any
	protected DALNode parent; // parent node. The root node has a null for this.

	
	/**
	 * Constructs a root node which holds the hierarchy of all
	 * valid curls given by path <code>filePath</code>
	 * 
	 * @param filePath the path where scan begins this is ususaly the DAL root path
	 * @return an instance of DALNode that has all hierarchy inside itself.
	 */
	public static DALNode getRoot(String filePath) {
		// create a root node 
		DALNode rootNode = new DALNode(filePath, null);
		// and recursively fills all its childs 
		rootNode.loadNodes(filePath);
		return rootNode;
	}

	/**
	 * Constructs this object.
	 *  
	 * @param name The name of this node
	 * @param parent parent node for this node
	 */
	public DALNode(String name, DALNode parent) {
		super();
		this.name = name;
		this.parent = parent;
	}

	/**
	 * Returns a string that holds child names of the given path
	 * @param path The path where to start i.e. MACI/Managers
	 * 
	 * @return a string of the child names delimited with a space
	 */
	public String list(String path) {
		// create at least an empty string as return val
		StringBuffer sbuf = new StringBuffer(50);

		DALNode node = findNode(path);
		// if node exists append its childs separated by a space
		for( int i=0; node!=null && node.childs!=null && i<node.childs.size(); i++ ) {
			DALNode currNode = (DALNode)node.childs.get(i);
			sbuf.append(currNode.name);
			sbuf.append(' ');
		}
		return new String(sbuf);
	}

	/**
	 * Returns the node for the given path or <code>null</code> if the 
	 * path does not exist under this tree.
	 * 
	 * @param path The path to find i.e. MACI/Managers
	 * @return The child node for path or <code>null</code> if it doesn't exist
	 */
	protected DALNode findNode(String path) {
		// use a StringTokenizer to simplifies the path parsing 
		StringTokenizer st = new StringTokenizer(path, "/");
		if( !st.hasMoreTokens() )
			return this;
		// we should have a child with the name of first part of path 
		// i.e. if we a root node and we get the path as /MACI/Managers/Manager
		// then we should have a child with name 'MACI' in this node
		String nodeName = st.nextToken();
		DALNode child = getNode(nodeName);
		// if we don't then we are unable to find this path 
		if( child == null )
			return null;
		// if we have more tokens i.e. 'Managers' then delegate to the child node
		// next part of the path and it will do the same for its childs and so for
		if( st.hasMoreTokens() )
			return child.findNode(path.substring(path.indexOf(nodeName)+nodeName.length()));
		// if no more parts in path then we have the node
		return child; 
	}

	/**
	 * Scans the path given by <code>filePath</code> and creates nodes
	 * as childs for this node for all XML files that has name as its 
	 * parent directory. 
	 *  
	 * @param filePath The path where to start scan i.e. $ACS_CDB
	 */
	protected void loadNodes(String filePath) {
		String fileName;
		File base = new File(filePath);
		File[] basefiles = base.listFiles();
		// scan a tree and add only xml files with the same name as its parent dir
		for (int i = 0; basefiles != null && i < basefiles.length; i++) {
			fileName = basefiles[i].getName();
			if(!fileName.endsWith(".xml")) {
				loadNodes(basefiles[i].getPath());
				continue;
			}
			// xmls are valid jDAL nodes if they have name as its parent dir
			String parentDirName = basefiles[i].getParentFile().getName();
			if( parentDirName.equals(fileName.substring(0,fileName.length()-4)))
				addNode(basefiles[i].getPath().substring(name.length()));
		}
	}
	
	/**
	 * Adds a child node to this node at the proper place given by <code>nodePath<code>
	 * The nodePath can be delimited by <code>File.separatorChar</code>.
	 * 
	 * @param nodePath The path of the node i.e. MACI/Managers/Manager/Manager.xml
	 */
	protected void addNode(String nodePath) {
		// if this node doesn't have childs jet, create them
		if(childs == null )
			childs = new ArrayList(1);
		// again use tokenizer to get rid of extra '/' in path
		// everything is as in findNode function.
		// If we get /MACI/Managers/Manager/Manager.xml we first create a node
		// 'MACI' in this node if it doesn't exists and delegete to it the rest
		// of the path.
		StringTokenizer st = new StringTokenizer(nodePath, String.valueOf(File.separatorChar));
		if( !st.hasMoreTokens() )
			return;
		String nodeName = st.nextToken();
		DALNode child = getNode(nodeName);
		if( child == null ) {
			child = new DALNode(nodeName, this);
			childs.add(child);
		}
		if( st.hasMoreTokens() )
			child.addNode(nodePath.substring(nodePath.indexOf(nodeName)+nodeName.length()));
	}
	
	/**
	 * Returns a sibling child node with given name
	 *  
	 * @param nodeName The name of child node
	 * @return The child node or <code>null</code> it it doesn't exists
	 */
	protected DALNode getNode(String nodeName) {
		// simply iterate through childs array since it is not expected
		// that we will have a lot of childs in node since we use hierarchy
		// to logicaly divide parts and therefore this shouldn't be a performance issue
		for( int i=0; childs != null && i<childs.size(); i++ ) {
			DALNode currNode = (DALNode)childs.get(i);
			if( currNode.name.equals(nodeName))
				return currNode;
		}
		return null;
	}

	/**
	 * Fill given list with nodes 
	 *  
	 * @param nodeName The name of child node
	 * @return The child node or <code>null</code> it it doesn't exists
	 */
	protected void getNodes(List list) {
		if(childs == null) {
			list.add(getCurl());
		}
		for( int i=0; childs != null && i<childs.size(); i++ ) {
			DALNode currNode = (DALNode)childs.get(i);
			currNode.getNodes(list);
		}
	}
	
	public String getCurl() {
		DALNode node = getCurlNode();
		if(node == null)
			return null;		
		// go up to the root and compose path
		String curl = "";
		DALNode curr = node.parent; 
		while(curr != null && curr.parent != null) {
			curl =  "/" + curr.name + curl;
			curr = curr.parent;
		}
		return curl;
	}

	/**
	 * Prints the hierarchy on System.out indenting child nodes
	 * This function is used for debuging purposes.
	 * 
	 * @param level The indentation level which will be recursevly increased
	 */
	public void print(int level) {
		// adjust the indent
		for( int j=0; j<level; j++)
			System.out.print( "  " );
		// print our name
		System.out.println(name);
		// and let the childs do the same
		for( int i=0; childs != null && i<childs.size(); i++ ) {
			DALNode currNode = (DALNode)childs.get(i);
			currNode.print(level+1);
		}
	}

	/**
	 * @return
	 */
	public DALNode getCurlNode() {
		// curl node is first node without childs
		for( int i=0; childs != null && i<childs.size(); i++ ) {
			DALNode currNode = (DALNode)childs.get(i);
			if(currNode.childs == null)
				return currNode;
		}
		return null;
	}

	/**
	 * @return
	 */
	public DALNode[] getChilds() {
		ArrayList list = new ArrayList();
		for( int i=0; childs != null && i<childs.size(); i++ ) {
			DALNode currNode = (DALNode)childs.get(i);
			if(currNode.childs == null)
				continue;
				list.add(currNode);
		}
		DALNode[] childs = new DALNode[list.size()];
		list.toArray(childs);
		return childs;
	}

	/**
	 * Returns true if this node is without any hierarchy - just plain node 
	 * @return
	 */
	public boolean isSimple() {
		if(childs==null || childs.size() > 1)
			return false;
		DALNode firstChild = (DALNode)childs.get(0); 
		if(firstChild.childs != null)
			return false;
		return true;
	}
	
	public boolean hasXmlChild() {
		boolean hasXmlChild = ( findNode(this.name + ".xml") != null );
		return hasXmlChild;
	}
}
