package com.cosylab.cdb.jdal;

import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import org.omg.PortableServer.POA;

import com.cosylab.CDB.DAOOperations;

import alma.acs.logging.AcsLogLevel;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.WrongCDBDataTypeEx;
import alma.cdbErrType.wrappers.AcsJCDBFieldDoesNotExistEx;
import alma.cdbErrType.wrappers.AcsJWrongCDBDataTypeEx;

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

public class DAOImpl implements DAOOperations {
	private final String m_name;
	private XMLTreeNode m_rootNode;
	private final POA m_poa;
	private boolean m_silent;
	private final Logger m_logger;

	public DAOImpl(String name, XMLTreeNode rootNode, POA poa, Logger logger) {
		this(name, rootNode, poa, logger, false);
	}

	public DAOImpl(String name, XMLTreeNode rootNode, POA poa, Logger logger, boolean silent) {
		m_name = name;
		m_rootNode = rootNode;
		m_poa = poa;
		m_silent = silent;
 		m_logger = logger;
	}

	public void destroy() {
		try {
			if (m_poa != null) {
				// we trust that m_name (the curl) was used to activate this DAO
				byte[] thisId = m_name.getBytes();
				m_poa.deactivate_object(thisId);
			}
		} catch (Exception e) {
			if (!m_silent) {
				m_logger.log(AcsLogLevel.NOTICE,"Exception destroying object "+ this +" : " + e);
				e.printStackTrace();
			}
		}
	}

	private String getField(String strFieldName)
		throws AcsJCDBFieldDoesNotExistEx {
		XMLTreeNode pNode = m_rootNode;
		if (strFieldName.length() == 0
			|| strFieldName.equals(m_rootNode.m_name)) {
			return pNode.getAttributeAndNodeNames();
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

		if (pNode == null){
			AcsJCDBFieldDoesNotExistEx e2 = new AcsJCDBFieldDoesNotExistEx();
			e2.setFieldName(strFieldName);
			throw e2;
		}
		String value;
		value = getFieldValue(pNode, fieldName);
		if (value == null) {
			// we should try to get it as node
			XMLTreeNode node =
				(XMLTreeNode) pNode.m_subNodesMap.get(fieldName);
			if (node == null) {
				
				// Components.xml Name="PARENT/CHILD/field" support
				int lpos = strFieldName.lastIndexOf('/');
		        if (lpos > 0)
		        {
		        	pNode = (XMLTreeNode) m_rootNode.m_subNodesMap.get(strFieldName.substring(0, lpos));
		        	if (pNode != null)
		        	{
			        	fieldName = strFieldName.substring(lpos+1);
		        		value = getFieldValue(pNode, fieldName);
		        		if (value != null) {
		        			if (!m_silent)
		        				m_logger.log(AcsLogLevel.DEBUG, "DAO:'" + m_name + "' returned '" + strFieldName + "'=" + value);  
		        			return value;
		        		}
		        	}
		        }
				
				if (!m_silent)
					m_logger.log(AcsLogLevel.NOTICE, "DAO:'" + m_name + "' Unable to return field: '" + strFieldName + "'");
				AcsJCDBFieldDoesNotExistEx e2 = new AcsJCDBFieldDoesNotExistEx();
				e2.setFieldName(strFieldName);
				throw e2;
			}
			value = node.getAttributeAndNodeNames();
		}
		if (!m_silent)
			m_logger.log(AcsLogLevel.DEBUG, "DAO:'" + m_name + "' returned '" + strFieldName + "'=" + value);  
		return value;
	}

	private String getFieldValue(XMLTreeNode pNode, String fieldName) {
		String value;
		// backward compatibility
		if (fieldName.equals("_characteristics")) {
			value = pNode.getAttributeAndNodeNames();
		} else if (fieldName.equals("_attributes")) {
			value = pNode.getAttributeNames();
		} else if (fieldName.equals("_elements")) {
			value = pNode.getElementNames();
		} else if (fieldName.equals("_subnodes")) {
			value = pNode.getSubNodeNames();
		} else if (fieldName.equals("_name")) {
			value = pNode.getName();
		} else {
			value = (String) pNode.m_fieldMap.get(fieldName);
		}
		return value;
	}

	public int get_long(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		String stringValue;
		try{
			stringValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){ 
			throw e.toCDBFieldDoesNotExistEx();
		}
		try {
			return Integer.parseInt(stringValue);
		} catch (NumberFormatException nfe) {
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + stringValue + "' to long: " + nfe); 
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx(nfe);
			e2.setValue(stringValue);
			e2.setDataType("long");
			throw e2.toWrongCDBDataTypeEx();
		}
	}

	public double get_double(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		String stringValue;
		try{
			stringValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){ 
			throw e.toCDBFieldDoesNotExistEx();
		}
		try {
			return Double.parseDouble(stringValue);
		} catch (NumberFormatException nfe) {
			m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + stringValue + "' to double: " + nfe); 
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx(nfe);
			e2.setValue(stringValue);
			e2.setDataType("double");
			throw e2.toWrongCDBDataTypeEx();
		}
	}

	public String get_string(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		try{
		return getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){ 
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	public String get_field_data(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		try{
		return getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){ 
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	public String[] get_string_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		String stringValue;
		try{
			stringValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){
				throw e.toCDBFieldDoesNotExistEx();
		}
		ArrayList list = new ArrayList();
		StringTokenizer st = new StringTokenizer(stringValue, ",");
		while (st.hasMoreTokens())
			list.add(st.nextToken());

		String[] seq = new String[list.size()];
		list.toArray(seq);

		return seq;
	}

	public int[] get_long_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		String stringValue;
		try{
			stringValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
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
				m_logger.log(AcsLogLevel.NOTICE,
						"Failed to cast element #"
						+ list.size()
						+ " of value '"
						+ val
						+ "' to long: "
						+ nfe);
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx(nfe);
			e2.setValue(val);
			e2.setDataType("long");
				throw e2.toWrongCDBDataTypeEx();
		}

		int[] seq = new int[list.size()];
		for (int i = 0; i < list.size(); i++)
			seq[i] = ((Integer) list.get(i)).intValue();

		return seq;
	}

	public double[] get_double_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		String stringValue;
		try{
		stringValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
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
				m_logger.log(AcsLogLevel.NOTICE,
					"Failed to cast element #"
						+ list.size()
						+ " of value '"
						+ val
						+ "' to double: "
						+ nfe);
			
			AcsJWrongCDBDataTypeEx e = new AcsJWrongCDBDataTypeEx(nfe);
			e.setValue(val);
			e.setDataType("double");
			throw e.toWrongCDBDataTypeEx();
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

	public void setRootNode(XMLTreeNode mRootNode) {
		m_rootNode = mRootNode;
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
