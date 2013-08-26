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
package com.cosylab.logging.engine;

import java.util.Vector;

import org.w3c.dom.NodeList;
import org.w3c.dom.Node;

import java.util.Collection;

/**
 * This class provides a default implementation of <code>org.w3c.dom.NodeList</code>.
 */
public class VectorNodeList extends Vector<Node> implements NodeList {
/**
 * VectorNodeList constructor comment.
 */
public VectorNodeList() {
	super();
}
/**
 * VectorNodeList constructor comment.
 * @param initialCapacity int
 */
public VectorNodeList(int initialCapacity) {
	super(initialCapacity);
}
/**
 * VectorNodeList constructor comment.
 * @param initialCapacity int
 * @param capacityIncrement int
 */
public VectorNodeList(int initialCapacity, int capacityIncrement) {
	super(initialCapacity, capacityIncrement);
}
/**
 * VectorNodeList constructor comment.
 * @param c java.util.Collection
 */
public VectorNodeList(Collection<Node> c) {
	super(c);
}
/**
 * getLength method comment.
 */
public int getLength() {
	return size();
}
/**
 * item method comment.
 */
public Node item(int i) {
	return elementAt(i);
}

}
