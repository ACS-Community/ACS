/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
 *    All rights reserved
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
package com.cosylab.acs.laser.dao;

import java.io.IOException;
import java.io.StringReader;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public final class DAOUtil
{
	public interface ElementHandler
	{
		public void handle(Element e);
	}
	
	public static final void extractElements(Node mama, String elName, Logger logger, ElementHandler handler)
	{
		Node kid=mama.getFirstChild();
		while (kid!=null) {
			if (kid.getNodeType()==Node.ELEMENT_NODE && kid.getNodeName().equals(elName)) {
				handler.handle((Element)kid);
			} else {
				if (logger!=null)
					logger.warning("Unexpected element of <"+mama.getNodeName()+"> encountered");
			}
			kid=kid.getNextSibling();
		}
	}
	
	public static final Document parseXML(String xml) throws SAXException, IOException, ParserConfigurationException
	{
		return DocumentBuilderFactory
					.newInstance()
					.newDocumentBuilder()
					.parse(new InputSource(new StringReader(xml)));
	}
	
	public static final String getTextOfEl(Node e)
	{
		if(e==null || e.getNodeType() != Node.ELEMENT_NODE) {
			throw new IllegalArgumentException();
		}
		
		Node n=e.getFirstChild();
		if (n==null)
			return "";
		
		if (n.getNodeType()==Node.TEXT_NODE) {
			String result=n.getNodeValue();
			if (n.getNextSibling()==null)
				return result;
		}

		throw new IllegalStateException("element contains data other than text");
	}
	
	public static final int[] parseIntIdList(Node e) throws Exception
	{
		if (e==null || e.getNodeType()!=Node.ELEMENT_NODE || !e.getNodeName().equals("id-list"))
			throw new IllegalArgumentException();
		
		Node cur=e.getFirstChild();
		int count=0;
		
		while (cur!=null) {
			if (cur.getNodeType()!=Node.ELEMENT_NODE)
				throw new Exception("<id-list> contains an unknown child");
			if (!cur.getNodeName().equals("id"))
				throw new Exception("<id-list> contains an unknown child");
			
			Node kid=cur.getFirstChild();
			if (kid==null)
				throw new Exception("<id> doesn't contain a value");
			if (kid.getNodeType()!=Node.TEXT_NODE)
				throw new Exception("<id> can only contain text");
			if (kid.getNextSibling()!=null)
				throw new Exception("<id> can only contain text");
			
			count++;
			cur=cur.getNextSibling();
		}
		
		int[] result=new int[count];
		cur=e.getFirstChild();
		int pos=0;
		
		while (cur!=null) {
			try {
				result[pos++]=Integer.parseInt(cur.getFirstChild().getNodeValue());
			} catch (NumberFormatException ex) {
				throw new Exception("Invalid ID", ex);
			}
			cur=cur.getNextSibling();
		}
		
		return result;		
	}
	
	public static final String[] parseStringIdList(Node e) throws Exception
	{
		if (e==null || e.getNodeType()!=Node.ELEMENT_NODE || !e.getNodeName().equals("id-list"))
			throw new IllegalArgumentException();
		
		Node cur=e.getFirstChild();
		int count=0;
		
		while (cur!=null) {
			if (cur.getNodeType()!=Node.ELEMENT_NODE)
				throw new Exception("<id-list> contains an unknown child");
			if (!cur.getNodeName().equals("id"))
				throw new Exception("<id-list> contains an unknown child");
			
			Node kid=cur.getFirstChild();
			if (kid==null)
				throw new Exception("<id> doesn't contain a value");
			if (kid.getNodeType()!=Node.TEXT_NODE)
				throw new Exception("<id> can only contain text");
			if (kid.getNextSibling()!=null)
				throw new Exception("<id> can only contain text");
			
			count++;
			cur=cur.getNextSibling();
		}
		
		String[] result=new String[count];

		cur=e.getFirstChild();
		int pos=0;
		
		while (cur!=null) {
			result[pos++]=cur.getFirstChild().getNodeValue();
			
			cur=cur.getNextSibling();
		}
		
		return result;		
	}
	
	static void encodeElemIf(StringBuffer out, String elName, Object value, int level)
	{
		if (value==null)
			return;
		
		encodeElem(out, elName, value.toString(), level);
	}
	
	static void encodeElem(StringBuffer out, String elName, String value, int level)
	{
		while ((level--)>0)
			out.append('\t');
		
		out.append('<');
		out.append(elName);
		out.append('>');
		escapeXMLEntities(out, value);
		out.append('<');
		out.append('/');
		out.append(elName);
		out.append('>');
		out.append('\n');
	}
	
	static void encodeAttr(StringBuffer out, String attrName, String attrVal)
	{
		out.append(' ');
		escapeXMLEntities(out, attrName);
		out.append('=');
		out.append('"');
		escapeXMLEntities(out, attrVal);
		out.append('"');
	}
	
	static void escapeXMLEntities(StringBuffer out, String ident)
	{
		int l=ident.length();
		for (int a=0; a<l; a++) {
			char c;
			switch(c=ident.charAt(l)) {
			case '&': out.append("&amp;"); break;
			case '"': out.append("&quot;"); break;
			case '<': out.append("&lt;"); break;
			case '>': out.append("&gt;"); break;
			case '\'': out.append("&apos;"); break;
			default: out.append(c);
			}
		}
	}

}
