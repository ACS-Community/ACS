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

import org.w3c.dom.DOMException;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.UserDataHandler; // only in J2SE 5

/**
 * This class is a wrapper for <code>org.w3c.dom.Node</code> with the
 * addition of a method, specific for the Data log entry
 * <code>getName()</code>. It is also required that the Data has
 * at least one child, the content.
 * <p>
 * TODO: once ACS has fully moved to J2SE 5, the new DOM Level 3 methods should be fully implemented
 * instead of the current mock impl. On first sight it seems that the getUserObject method could 
 * have something to do with the {@link #name} variable.
 */
public class DataNode implements Node {
	private Node node;
	private String name = null;
/**
 * DataNode constructor comment.
 */
public DataNode(Node node) throws DOMException {
	this.node = node;
	initialize();
}
/**
 * appendChild method comment.
 */
public org.w3c.dom.Node appendChild(org.w3c.dom.Node arg1) throws org.w3c.dom.DOMException {
	return node.appendChild(arg1);
}
/**
 * cloneNode method comment.
 */
public org.w3c.dom.Node cloneNode(boolean arg1) {
	return node.cloneNode(arg1);
}
/**
 * getAttributes method comment.
 */
public org.w3c.dom.NamedNodeMap getAttributes() {
	return node.getAttributes();
}
/**
 * getChildNodes method comment.
 */
public org.w3c.dom.NodeList getChildNodes() {
	return node.getChildNodes();
}
/**
 * getFirstChild method comment.
 */
public org.w3c.dom.Node getFirstChild() {
	return node.getFirstChild();
}
/**
 * getLastChild method comment.
 */
public org.w3c.dom.Node getLastChild() {
	return node.getLastChild();
}
/**
 * getLocalName method comment.
 */
public String getLocalName() {
	return node.getLocalName();
}
public String getName() {
	return name;
}
/**
 * getNamespaceURI method comment.
 */
public String getNamespaceURI() {
	return node.getNamespaceURI();
}
/**
 * getNextSibling method comment.
 */
public org.w3c.dom.Node getNextSibling() {
	return node.getNextSibling();
}
/**
 * getNodeName method comment.
 */
public String getNodeName() {
	return node.getNodeName();
}
/**
 * getNodeType method comment.
 */
public short getNodeType() {
	return node.getNodeType();
}
/**
 * getNodeValue method comment.
 */
public String getNodeValue() throws org.w3c.dom.DOMException {
	return node.getNodeValue();
}
/**
 * getOwnerDocument method comment.
 */
public org.w3c.dom.Document getOwnerDocument() {
	return node.getOwnerDocument();
}
/**
 * getParentNode method comment.
 */
public org.w3c.dom.Node getParentNode() {
	return node.getParentNode();
}
/**
 * getPrefix method comment.
 */
public String getPrefix() {
	return node.getPrefix();
}
/**
 * getPreviousSibling method comment.
 */
public org.w3c.dom.Node getPreviousSibling() {
	return node.getPreviousSibling();
}
/**
 * hasAttributes method comment.
 */
public boolean hasAttributes() {
	return node.hasAttributes();
}
/**
 * hasChildNodes method comment.
 */
public boolean hasChildNodes() {
	return node.hasChildNodes();
}
private void initialize() throws DOMException {
	NamedNodeMap nnm = node.getAttributes();
	Node attr;

	attr = nnm.getNamedItem("Name");
	if (attr == null) throw new DOMException(DOMException.NOT_FOUND_ERR, "Name attribute is missing for this Data sub-element.");
	name = attr.getNodeValue();

	if (!node.hasChildNodes()) throw new DOMException(DOMException.NOT_FOUND_ERR, "No content for this Data sub-element.");
}
/**
 * insertBefore method comment.
 */
public org.w3c.dom.Node insertBefore(org.w3c.dom.Node arg1, org.w3c.dom.Node arg2) throws org.w3c.dom.DOMException {
	return node.insertBefore(arg1, arg2);
}
/**
 * isSupported method comment.
 */
public boolean isSupported(String arg1, String arg2) {
	return node.isSupported(arg1, arg2);
}
/**
 * normalize method comment.
 */
public void normalize() {
	node.normalize();
}
/**
 * removeChild method comment.
 */
public org.w3c.dom.Node removeChild(org.w3c.dom.Node arg1) throws org.w3c.dom.DOMException {
	return node.removeChild(arg1);
}
/**
 * replaceChild method comment.
 */
public org.w3c.dom.Node replaceChild(org.w3c.dom.Node arg1, org.w3c.dom.Node arg2) throws org.w3c.dom.DOMException {
	return node.replaceChild(arg1, arg2);
}
/**
 * setNodeValue method comment.
 */
public void setNodeValue(String arg1) throws org.w3c.dom.DOMException {
	node.setNodeValue(arg1);
}
/**
 * setPrefix method comment.
 */
public void setPrefix(String arg1) throws org.w3c.dom.DOMException {
	node.setPrefix(arg1);
}
public String toString() {
	StringBuffer sb = new StringBuffer("Data Name: " + name + " Value: ");
	sb.append(node.toString());
	
	return sb.toString();
}

	/**
	 * DOM level 3 method, not implemented. Returns -1.
	 * @see org.w3c.dom.Node#compareDocumentPosition(org.w3c.dom.Node)
	 */
	public short compareDocumentPosition(Node other) throws DOMException {
		// TODO Auto-generated method stub
		return -1;
	}
	/**
	 * DOM level 3 method, not implemented. Returns "".
	 * @see org.w3c.dom.Node#getBaseURI()
	 */
	public String getBaseURI() {
		// TODO Auto-generated method stub
		return "";
	}
	/**
	 * DOM level 3 method, not implemented. Returns null.
	 * @see org.w3c.dom.Node#getFeature(java.lang.String, java.lang.String)
	 */
	public Object getFeature(String feature, String version) {
		// TODO Auto-generated method stub
		return null;
	}
	/**
	 * DOM level 3 method, not implemented. Returns "".
	 * @see org.w3c.dom.Node#getTextContent()
	 */
	public String getTextContent() throws DOMException {
		// TODO Auto-generated method stub
		return "";
	}
	/**
	 * DOM level 3 method, not implemented. Returns null.
	 * @see org.w3c.dom.Node#getUserData(java.lang.String)
	 */
	public Object getUserData(String key) {
		// TODO Auto-generated method stub
		return null;
	}
	/**
	 * DOM level 3 method, not implemented. Returns false.
	 * @see org.w3c.dom.Node#isDefaultNamespace(java.lang.String)
	 */
	public boolean isDefaultNamespace(String namespaceURI) {
		// TODO Auto-generated method stub
		return false;
	}
	/**
	 * DOM level 3 method, not implemented. Returns false.
	 * @see org.w3c.dom.Node#isEqualNode(org.w3c.dom.Node)
	 */
	public boolean isEqualNode(Node arg) {
		// TODO Auto-generated method stub
		return false;
	}
	/**
	 * DOM level 3 method, not implemented. Returns false.
	 * @see org.w3c.dom.Node#isSameNode(org.w3c.dom.Node)
	 */
	public boolean isSameNode(Node other) {
		// TODO Auto-generated method stub
		return false;
	}
	/**
	 * DOM level 3 method, not implemented. Returns "".
	 * @see org.w3c.dom.Node#lookupNamespaceURI(java.lang.String)
	 */
	public String lookupNamespaceURI(String prefix) {
		// TODO Auto-generated method stub
		return null;
	}
	/**
	 * DOM level 3 method, not implemented. Returns "".
	 * @see org.w3c.dom.Node#lookupPrefix(java.lang.String)
	 */
	public String lookupPrefix(String namespaceURI) {
		// TODO Auto-generated method stub
		return null;
	}
	/**
	 * DOM level 3 method, not implemented. 
	 * @see org.w3c.dom.Node#setTextContent(java.lang.String)
	 */
	public void setTextContent(String textContent) throws DOMException {
		// TODO Auto-generated method stub
		
	}
	/**
	 * DOM level 3 method, not implemented. Returns null.
	 * @see org.w3c.dom.Node#setUserData(java.lang.String, java.lang.Object, org.w3c.dom.UserDataHandler)
	 */
	public Object setUserData(String key, Object data, UserDataHandler handler) {
		// TODO Auto-generated method stub
		return null;
	}
}
