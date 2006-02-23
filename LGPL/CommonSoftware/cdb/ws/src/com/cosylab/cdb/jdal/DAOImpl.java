package com.cosylab.cdb.jdal;

import java.util.ArrayList;
import java.util.StringTokenizer;

import org.omg.PortableServer.POA;

import com.cosylab.CDB.*;

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

public class DAOImpl extends DAOPOA {
	private String m_name;
	private XMLTreeNode m_rootNode = null;
	private POA m_poa;
	private boolean m_silent;

	public DAOImpl(String name, XMLTreeNode rootNode, POA poa) {
		this(name, rootNode, poa, false);
	}

	public DAOImpl(String name, XMLTreeNode rootNode, POA poa, boolean silent) {
		m_name = name;
		m_rootNode = rootNode;
		m_poa = poa;
		m_silent = silent;
	}

	public void destroy() {
		try {
			if (m_poa != null) {
				byte[] thisId = m_poa.servant_to_id(this);
				m_poa.deactivate_object(thisId);
			}
		} catch (Exception e) {
			if (!m_silent) {
				System.err.println("Exception destroying object " + this +" : " + e);
				e.printStackTrace();
			}
		}
	}

	private String getField(String strFieldName)
		throws FieldDoesNotExist {
		XMLTreeNode pNode = m_rootNode;
		if (strFieldName.length() == 0
			|| strFieldName.equals(m_rootNode.m_name)) {
			return pNode.getAttributeNames();
		}
		StringTokenizer st = new StringTokenizer(strFieldName, "/");
		String fieldName = st.nextToken();
		while (st.hasMoreTokens()) {
		        XMLTreeNode child = (XMLTreeNode) pNode.m_subNodesMap.get(fieldName);
			if (child == null)
			{
			    fieldName += "/" + st.nextToken();
			}
			else
			{
			    pNode = child;
			    fieldName = st.nextToken();
			}
		}

		if (pNode == null)
		    throw new FieldDoesNotExist("Field '" + strFieldName + "' does not exists.");

		String value;
		// backward compatibility
		if (fieldName.equals("_characteristics")) {
			value = pNode.getAttributeNames();
		} else {
			value = (String) pNode.m_fieldMap.get(fieldName);
		}
		if (value == null) {
			// we should try to get it as node
			XMLTreeNode node =
				(XMLTreeNode) pNode.m_subNodesMap.get(fieldName);
			if (node == null) {
				if (!m_silent)
					System.err.println("DAO:'" + m_name + "' Unable to return field: '" + strFieldName + "'");
				throw new FieldDoesNotExist("Field '" + strFieldName + "' does not exists.");

			}
			value = node.getAttributeNames();
		}
		if (!m_silent)
			System.err.println("DAO:'" + m_name + "' returned '" + strFieldName + "'=" + value);
		return value;
	}

	public int get_long(String propertyName)
		throws WrongDataType, FieldDoesNotExist {
		String stringValue = getField(propertyName);
		try {
			return Integer.parseInt(stringValue);
		} catch (NumberFormatException nfe) {
			if (!m_silent)
				System.err.println("Failed to cast '" + stringValue + "' to long: " + nfe);
			throw new WrongDataType("Failed to cast '" + stringValue + "' to long: " + nfe);
		}
	}

	public double get_double(String propertyName)
		throws WrongDataType, FieldDoesNotExist {
		String stringValue = getField(propertyName);
		try {
			return Double.parseDouble(stringValue);
		} catch (NumberFormatException nfe) {
			System.err.println("Failed to cast '" + stringValue + "' to double: " + nfe);
			throw new WrongDataType("Failed to cast '" + stringValue + "' to double: " + nfe);
		}
	}

	public String get_string(String propertyName)
		throws WrongDataType, FieldDoesNotExist {
		return getField(propertyName);
	}

	public String get_field_data(String propertyName)
		throws WrongDataType, FieldDoesNotExist {
		return getField(propertyName);
	}

	public String[] get_string_seq(String propertyName)
		throws WrongDataType, FieldDoesNotExist {
		String stringValue = getField(propertyName);

		ArrayList list = new ArrayList();
		StringTokenizer st = new StringTokenizer(stringValue, ",");
		while (st.hasMoreTokens())
			list.add(st.nextToken());

		String[] seq = new String[list.size()];
		list.toArray(seq);

		return seq;
	}

	public int[] get_long_seq(String propertyName)
		throws WrongDataType, FieldDoesNotExist {
		String stringValue = getField(propertyName);

		ArrayList list = new ArrayList();
		String val = null;
		try {
			StringTokenizer st = new StringTokenizer(stringValue, ",");
			while (st.hasMoreTokens()) {
				val = st.nextToken().trim();
				list.add(new Integer(val));
			}

		} catch (NumberFormatException nfe) {
			if (!m_silent)
				System.err.println(
						"Failed to cast element #"
						+ list.size()
						+ " of value '"
						+ val
						+ "' to long: "
						+ nfe);
			throw new WrongDataType(
				"Failed to cast element #"
					+ list.size()
					+ " of value '"
					+ val
					+ "' to long: "
					+ nfe);
		}

		int[] seq = new int[list.size()];
		for (int i = 0; i < list.size(); i++)
			seq[i] = ((Integer) list.get(i)).intValue();

		return seq;
	}

	public double[] get_double_seq(String propertyName)
		throws WrongDataType, FieldDoesNotExist {
		String stringValue = getField(propertyName);

		ArrayList list = new ArrayList();
		String val = null;
		try {
			StringTokenizer st = new StringTokenizer(stringValue, ",");
			while (st.hasMoreTokens()) {
				val = st.nextToken().trim();
				list.add(new Double(val));
			}

		} catch (NumberFormatException nfe) {
			if (!m_silent)
				System.err.println(
					"Failed to cast element #"
						+ list.size()
						+ " of value '"
						+ val
						+ "' to double: "
						+ nfe);
			throw new WrongDataType(
				"Failed to cast element #"
					+ list.size()
					+ " of value '"
					+ val
					+ "' to double: "
					+ nfe);
		}

		double[] seq = new double[list.size()];
		for (int i = 0; i < list.size(); i++)
			seq[i] = ((Double) list.get(i)).doubleValue();

		return seq;
	}

	/**
	 * @return
	 */
	public XMLTreeNode getRootNode() {
		return m_rootNode;
	}

	/**
	 * @return
	 */
	public String getName() {
		return m_name;
	}

	/**
	 * @return
	 */
	public POA getPOA() {
		return m_poa;
	}

}
