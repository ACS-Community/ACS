package com.cosylab.cdb;

import javax.naming.Context;
import javax.naming.NameClassPair;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.swing.tree.DefaultMutableTreeNode;

/*******************************************************************************
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
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class CDBTreeNode extends DefaultMutableTreeNode {
	private boolean areChildrenDefined = false;
	private String name;
	private Context parent = null;
	private Context context = null;

	public CDBTreeNode(String name, Context parent, Context context) {
		this.name = name;
		this.parent = parent;
		this.context = context;
	}

	public boolean isLeaf() {
		if (context == null && parent == null)
			return true;
		return false;
	}

	public int getChildCount() {
		if (!areChildrenDefined) {
			areChildrenDefined = true;
			try {
				if (context == null) {
					context = (Context) parent.lookup(name);
				}
				NamingEnumeration names = context.list("");
				while (names.hasMore()) {
					NameClassPair pair = (NameClassPair) names.next();
					if (pair.getClassName().equals(Context.class.getName())) {
						add(new CDBTreeNode(pair.getName(), context, null));
					} else {
						add(new CDBTreeNode(pair.getName(), null, null));
					}
					//System.out.println( "Enumerating " + pair.getName() + " class " + pair.getClassName() );
				}
			} catch (NamingException e) {
				e.printStackTrace();
			}
		}
		return (super.getChildCount());
	}

	public String getValue() {
		if (!isLeaf())
			return null;
		CDBTreeNode parent = (CDBTreeNode) getParent();
		String retVal = null;
		try {
			retVal = (String) parent.context.lookup(name);
		} catch (NamingException e) {
			e.printStackTrace();
		}
		return retVal;
	}

	public String toString() {
		return name;
	}
}
