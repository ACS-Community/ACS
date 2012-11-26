package com.cosylab.cdb.jdal;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Map;
import java.util.logging.Logger;

import org.hibernate.Session;
import org.hibernate.Transaction;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.PortableServer.POA;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import alma.acs.logging.AcsLogLevel;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.CDBFieldIsReadOnlyEx;
import alma.cdbErrType.WrongCDBDataTypeEx;
import alma.cdbErrType.wrappers.AcsJCDBFieldDoesNotExistEx;
import alma.cdbErrType.wrappers.AcsJCDBFieldIsReadOnlyEx;
import alma.cdbErrType.wrappers.AcsJWrongCDBDataTypeEx;

import com.cosylab.CDB.WDAOOperations;
import com.cosylab.cdb.jdal.hibernate.ConvertToPrimitiveFeature;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector.NodeAndMutator;

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
 * @author msekoranja
 */
public class HibernateWDAOImpl implements WDAOOperations {
	private final Session m_session;
	private final String m_name;
	private volatile Object m_rootNode;
	private final POA m_poa;
	private final boolean m_silent;
	private final Logger m_logger;
	private final boolean m_autoCommit;

	public void setRootNode(Object mRootNode) {
		m_rootNode = mRootNode;
	}

	public HibernateWDAOImpl(Session session, String name, Object rootNode, POA poa, Logger logger) {
		this(session, name, rootNode, poa, logger, false, true);
	}

	public HibernateWDAOImpl(Session session, String name, Object rootNode, POA poa, Logger logger, boolean silent, boolean autoCommit) {
		m_session = session;
		m_name = name;
		m_rootNode = rootNode;
		m_poa = poa;
		m_silent = silent;
 		m_logger = logger;
 		m_autoCommit = autoCommit;
	}

	
	private org.omg.PortableServer.Servant servant;
	public void setSetvant(org.omg.PortableServer.Servant servant) {
		this.servant = servant;
	}
	
	public void destroy() {
		try {
			if (m_poa != null && servant != null) {
				// TODO now destroy is disabled, why:
				// destroy should also remove this instance from cache (daoMap, wdaoMap, etc.)
				// reference counting is needed, since we have multiple clients
				/*
				byte[] thisId = m_poa.servant_to_id(servant);
				m_poa.deactivate_object(thisId);
				 */
			}
		} catch (Exception e) {
			if (!m_silent) {
				m_logger.log(AcsLogLevel.NOTICE,"Exception destroying object "+ this +" : " + e);
				e.printStackTrace();
			}
		}
	}

	public Object getField(String path) 
		throws AcsJCDBFieldDoesNotExistEx
	{
		// backward compatibility
		final String CHARACTERISTICS_KEY = "_characteristics";
		final String ATTRIBUTES_KEY = "_attributes";
		final String ELEMENTS_KEY = "_elements";
		final String SUBNODES_KEY = "_subnodes";

		boolean subnodesRequest = false;
		boolean elementsRequest = false;
		boolean attributesRequest = false;
		if (path.endsWith(CHARACTERISTICS_KEY))
		{
			path = path.substring(0, path.length() - CHARACTERISTICS_KEY.length());
			attributesRequest = elementsRequest = subnodesRequest = true; 
		}
		else if (path.endsWith(ATTRIBUTES_KEY))
		{
			path = path.substring(0, path.length() - ATTRIBUTES_KEY.length());
			attributesRequest = true;
		}
		else if (path.endsWith(ELEMENTS_KEY))
		{
			path = path.substring(0, path.length() - ELEMENTS_KEY.length());
			elementsRequest = true;
		}
		else if (path.endsWith(SUBNODES_KEY))
		{
			path = path.substring(0, path.length() - SUBNODES_KEY.length());
			subnodesRequest = true;
		}

		Object field = DOMJavaClassIntrospector.getNode(path, m_rootNode);
		if (field == null)
		{
			AcsJCDBFieldDoesNotExistEx e2 = new AcsJCDBFieldDoesNotExistEx();
			e2.setFieldName(path);
			throw e2;
		}
		
		// request for elements/attibutes (empty name)
		if (path.length() == 0 || path.charAt(path.length() - 1) == '/')
		{
			// JDAL return attributes and subnodes
			String[] attributes = attributesRequest ? DOMJavaClassIntrospector.getFields(field) : new String[0];
			String[] elements = elementsRequest ? DOMJavaClassIntrospector.getElements(field) : new String[0];
			String[] subnodes = subnodesRequest ? DOMJavaClassIntrospector.getSubnodes(field) : new String[0];
			String[] concat = new String[attributes.length + elements.length + subnodes.length];
			System.arraycopy(attributes, 0, concat, 0, attributes.length);
			System.arraycopy(elements, 0, concat, attributes.length, elements.length);
			System.arraycopy(subnodes, 0, concat, attributes.length + elements.length, subnodes.length);
			field = concat;
		}
		
		// automatic conversion
		if (field instanceof ConvertToPrimitiveFeature)
			field = ((ConvertToPrimitiveFeature)field).convert();
	
		// array support
		if (field instanceof Element)
		{
			NodeList childList = ((Element)field).getChildNodes();
			int childCount = childList.getLength();
			StringBuffer strignifiedArray = new StringBuffer();
			for (int i = 0; i < childCount; i++)
			{
				Node childNode = childList.item(i);
				if (childNode instanceof Element && childNode.getAttributes().getLength() > 0)
				{
					if (strignifiedArray.length() > 0) strignifiedArray.append(',');
					strignifiedArray.append(childNode.getAttributes().item(0).getTextContent());
				}
			}
			field = strignifiedArray.toString();
		}
		
		if (!m_silent)
			m_logger.log(AcsLogLevel.NOTICE, "DAO: '" + m_name + "' returned '" + path + "' = '" +
					(field.getClass().isArray() ? DOMJavaClassIntrospector.stringifyArray(field) : field) + "'");  
		
		return field;
	}

	public int get_long(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		Object objectValue;
		try{
			objectValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){ 
			throw e.toCDBFieldDoesNotExistEx();
		}
		try {
			if (objectValue instanceof String)
				return Integer.parseInt((String)objectValue);
			else
				return ((Number)objectValue).intValue();
		} catch (ClassCastException nfe) {
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + objectValue + "' to long: " + nfe); 
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx(nfe);
			e2.setValue(objectValue.toString());
			e2.setDataType("long");
			throw e2.toWrongCDBDataTypeEx();
		}
	}

	public double get_double(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		Object objectValue;
		try{
			objectValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){ 
			throw e.toCDBFieldDoesNotExistEx();
		}
		try {
			if (objectValue instanceof String)
				return Double.parseDouble((String)objectValue);
			else
				return ((Number)objectValue).doubleValue();
		} catch (ClassCastException nfe) {
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + objectValue + "' to double: " + nfe); 
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx(nfe);
			e2.setValue(objectValue.toString());
			e2.setDataType("double");
			throw e2.toWrongCDBDataTypeEx();
		}
	}

	@SuppressWarnings("unchecked")
	public String get_string(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		Object objectValue;
		try{
			objectValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
		try {
			if (objectValue.getClass().isArray())
				return DOMJavaClassIntrospector.stringifyArray(objectValue);
			
			if (objectValue instanceof Map)
			{
				Object[] arr = ((Map)objectValue).keySet().toArray();
				StringBuilder sb = new StringBuilder();
				boolean first = true;
				for (Object a : arr)
				{
					if (first) first = false; else sb.append(',');
					sb.append((String)a);
				}
				return sb.toString();
			}

			Class<? extends Object> valueType = objectValue.getClass();
			if (!DOMJavaClassIntrospector.isPrimitive(valueType))
				throw new ClassCastException(valueType + " not a primitive/string/array");

			objectValue = DOMJavaClassIntrospector.handleInfinity(objectValue);
			
			return objectValue.toString();
		} catch (ClassCastException nfe) {
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + objectValue + "' to string: " + nfe); 
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx(nfe);
			e2.setValue(objectValue.toString());
			e2.setDataType("string");
			throw e2.toWrongCDBDataTypeEx();
		}
	}

	public String get_field_data(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		return get_string(propertyName);
	}

	@SuppressWarnings("unchecked")
	public String[] get_string_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		Object objectValue;
		try{
			objectValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
		
		// no conversion needed
		if (objectValue instanceof String[])
			return (String[])objectValue;

		// Map keys
		if (objectValue instanceof Map)
		{
			Map map = (Map)objectValue;
			String[] retVal = new String[map.size()]; int i = 0;
			for (Object obj : map.keySet())
				retVal[i++] = (String)obj;
			return retVal;
		}
		
		Class<? extends Object> type = objectValue.getClass();
		if (objectValue instanceof String)
		{
			String[] tokens = ((String)objectValue).split(",");
			for (int i = 0; i < tokens.length; i++)
				tokens[i] = tokens[i].trim();
			return tokens;
		}
		else if (!type.isArray() || !DOMJavaClassIntrospector.isPrimitive(type.getComponentType()))
		{
			if (!m_silent) {
				// TODO take out the dummy exception. Now needed for debugging an OMC/TMCDB issue.
				m_logger.log(AcsLogLevel.NOTICE, "DAO '" + m_name + "' failed to cast to String[] the property '" + propertyName 
						+ "' of type '" + type.toString() + "' with value "+ objectValue + "'.", new Exception("just for stack trace"));
			}
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx();
			e2.setValue(objectValue.getClass().toString());
			e2.setDataType("String[]");
			throw e2.toWrongCDBDataTypeEx();
		}
		
		// do fancy conversion
		int len = Array.getLength(objectValue);
		String[] seq = new String[len];
		for (int i = 0; i < len; i++)
			seq[i] = DOMJavaClassIntrospector.handleInfinity(Array.get(objectValue, i)).toString();

		return seq;
	}

	public int[] get_long_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		Object objectValue;
		try{
			objectValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
		
		// no conversion needed
		if (objectValue instanceof int[])
			return (int[])objectValue;

		Class<? extends Object> type = objectValue.getClass();
		if (objectValue instanceof String)
		{
			String[] tokens = ((String)objectValue).split(",");
			int[] retVal = new int[tokens.length]; 
			try
			{
				for (int i = 0; i < tokens.length; i++)
					retVal[i] = Integer.parseInt(tokens[i].trim());
				return retVal;
			} catch (NullPointerException npe) {
				if (!m_silent)
					m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + objectValue + "' to long[]."); 
				AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx();
				e2.setValue(objectValue.getClass().toString());
				e2.setDataType("long[]");
				throw e2.toWrongCDBDataTypeEx();
			}
		}
		else if (!type.isArray() || !type.getComponentType().isAssignableFrom(Number.class))
		{
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + objectValue + "' to long[]."); 
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx();
			e2.setValue(objectValue.getClass().toString());
			e2.setDataType("long[]");
			throw e2.toWrongCDBDataTypeEx();
		}
		
		// do fancy conversion
		int len = Array.getLength(objectValue);
		int[] seq = new int[len];
		for (int i = 0; i < len; i++)
			seq[i] = ((Number) Array.get(objectValue, i)).intValue();

		return seq;
	}

	public double[] get_double_seq(String propertyName)
		throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		Object objectValue;
		try{
			objectValue = getField(propertyName);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
		
		// no conversion needed
		if (objectValue instanceof double[])
			return (double[])objectValue;

		Class<? extends Object> type = objectValue.getClass();
		if (objectValue instanceof String)
		{
			String[] tokens = ((String)objectValue).split(",");
			double[] retVal = new double[tokens.length]; 
			try
			{
				for (int i = 0; i < tokens.length; i++)
					retVal[i] = Double.parseDouble(tokens[i].trim());
				return retVal;
			} catch (NullPointerException npe) {
				if (!m_silent)
					m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + objectValue + "' to dluble[]."); 
				AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx();
				e2.setValue(objectValue.getClass().toString());
				e2.setDataType("double[]");
				throw e2.toWrongCDBDataTypeEx();
			}
		}
		else if (!type.isArray() || !type.getComponentType().isAssignableFrom(Number.class))
		{
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to cast '" + objectValue + "' to double[]."); 
			AcsJWrongCDBDataTypeEx e2 = new AcsJWrongCDBDataTypeEx();
			e2.setValue(objectValue.getClass().toString());
			e2.setDataType("double[]");
			throw e2.toWrongCDBDataTypeEx();
		}
		
		// do fancy conversion
		int len = Array.getLength(objectValue);
		double[] seq = new double[len];
		for (int i = 0; i < len; i++)
			seq[i] = ((Number) Array.get(objectValue, i)).doubleValue();

		return seq;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_long(java.lang.String, int)
	 */
	@SuppressWarnings("unchecked")
	public void set_long(String propertyName, int value)
			throws CDBFieldIsReadOnlyEx, CDBFieldDoesNotExistEx {
		NodeAndMutator nodeAndMutator = DOMJavaClassIntrospector.getRecursiveMutatorMethod(propertyName, m_rootNode);
		if (nodeAndMutator == null) {
			AcsJCDBFieldDoesNotExistEx ex = new AcsJCDBFieldDoesNotExistEx();
			ex.setFieldName(propertyName);
			throw ex.toCDBFieldDoesNotExistEx();
		}

		Transaction tr = null;
		try
		{
			if (nodeAndMutator.mutator.getParameterTypes().length != 1) {
				AcsJCDBFieldIsReadOnlyEx acsex = new AcsJCDBFieldIsReadOnlyEx();
				acsex.setFieldName(propertyName);
				throw acsex.toCDBFieldIsReadOnlyEx();
			}

			Object toSet;
			Class parameterClass = nodeAndMutator.mutator.getParameterTypes()[0];
			if (parameterClass.isAssignableFrom(String.class))
				toSet = String.valueOf(value);
			else if (parameterClass.isAssignableFrom(Integer.class)
					|| parameterClass.isAssignableFrom(int.class))
				toSet = Integer.valueOf(value);
			else if (parameterClass.isAssignableFrom(Long.class)
					|| parameterClass.isAssignableFrom(long.class))
				toSet = Long.valueOf(value);
			else if (parameterClass.isAssignableFrom(Byte.class)
					|| parameterClass.isAssignableFrom(byte.class))
				toSet = Byte.valueOf((byte)value);
			else if (parameterClass.isAssignableFrom(Short.class)
					|| parameterClass.isAssignableFrom(short.class))
				toSet = Short.valueOf((short)value);
			else if (parameterClass.isAssignableFrom(Double.class)
					|| parameterClass.isAssignableFrom(double.class))
				toSet = Double.valueOf(value);
			else if (parameterClass.isAssignableFrom(Float.class)
					|| parameterClass.isAssignableFrom(float.class))
				toSet = Float.valueOf(value);
			else if (parameterClass.isAssignableFrom(Boolean.class)
					|| parameterClass.isAssignableFrom(boolean.class))
				toSet = (value != 0);
			else
				throw new NO_RESOURCES("cannot convert value");

			if (m_autoCommit) tr = m_session.beginTransaction();
			nodeAndMutator.mutator.invoke(nodeAndMutator.node, new Object[] { toSet });
			if (tr != null) tr.commit();

		} catch (Throwable th) {
			if (tr != null)
				tr.rollback();
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to set '" + value + "' to : " + (this.m_name + "/" + propertyName), th);
			throw new NO_RESOURCES(th.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.cosylab.CDB.WDAOOperations#set_string(java.lang.String,
	 * java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	public void set_string(String propertyName, String value)
			throws CDBFieldIsReadOnlyEx, CDBFieldDoesNotExistEx {
		NodeAndMutator nodeAndMutator = DOMJavaClassIntrospector.getRecursiveMutatorMethod(propertyName, m_rootNode);
		if (nodeAndMutator == null) {
			AcsJCDBFieldDoesNotExistEx ex = new AcsJCDBFieldDoesNotExistEx();
			ex.setFieldName(propertyName);
			throw ex.toCDBFieldDoesNotExistEx();
		}

		Transaction tr = null;
		try
		{
			if (nodeAndMutator.mutator.getParameterTypes().length != 1) {
				AcsJCDBFieldIsReadOnlyEx acsex = new AcsJCDBFieldIsReadOnlyEx();
				acsex.setFieldName(propertyName);
				throw acsex.toCDBFieldIsReadOnlyEx();
			}

			Object toSet;
			Class parameterClass = nodeAndMutator.mutator.getParameterTypes()[0];
			if (parameterClass.isAssignableFrom(String.class))
				toSet = String.valueOf(value);
			else if (parameterClass.isAssignableFrom(Integer.class)
					|| parameterClass.isAssignableFrom(int.class))
				toSet = Integer.valueOf(value);
			else if (parameterClass.isAssignableFrom(Long.class)
					|| parameterClass.isAssignableFrom(long.class))
				toSet = Long.valueOf(value);
			else if (parameterClass.isAssignableFrom(Byte.class)
					|| parameterClass.isAssignableFrom(byte.class))
				toSet = Byte.valueOf(value);
			else if (parameterClass.isAssignableFrom(Short.class)
					|| parameterClass.isAssignableFrom(short.class))
				toSet = Short.valueOf(value);
			else if (parameterClass.isAssignableFrom(Double.class)
					|| parameterClass.isAssignableFrom(double.class))
				toSet = Double.valueOf(value);
			else if (parameterClass.isAssignableFrom(Float.class)
					|| parameterClass.isAssignableFrom(float.class))
				toSet = Float.valueOf(value);
			else if (parameterClass.isAssignableFrom(Boolean.class)
					|| parameterClass.isAssignableFrom(boolean.class))
				toSet = Boolean.valueOf(value);
			else
				throw new NO_RESOURCES("cannot convert value");

			if (m_autoCommit) tr = m_session.beginTransaction();
			nodeAndMutator.mutator.invoke(nodeAndMutator.node, new Object[] { toSet });
			if (tr != null) tr.commit();

		} catch (Throwable th) {
			if (tr != null)
				tr.rollback();
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to set '" + value + "' to : " + (this.m_name + "/" + propertyName), th);
			throw new NO_RESOURCES(th.getMessage());
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_double(java.lang.String, double)
	 */
	@SuppressWarnings("unchecked")
	public void set_double(String propertyName, double value)
			throws CDBFieldIsReadOnlyEx, CDBFieldDoesNotExistEx {
		NodeAndMutator nodeAndMutator = DOMJavaClassIntrospector.getRecursiveMutatorMethod(propertyName, m_rootNode);
		if (nodeAndMutator == null) {
			AcsJCDBFieldDoesNotExistEx ex = new AcsJCDBFieldDoesNotExistEx();
			ex.setFieldName(propertyName);
			throw ex.toCDBFieldDoesNotExistEx();
		}

		Transaction tr = null;
		try
		{
			if (nodeAndMutator.mutator.getParameterTypes().length != 1) {
				AcsJCDBFieldIsReadOnlyEx acsex = new AcsJCDBFieldIsReadOnlyEx();
				acsex.setFieldName(propertyName);
				throw acsex.toCDBFieldIsReadOnlyEx();
			}

			Object toSet;
			Class parameterClass = nodeAndMutator.mutator.getParameterTypes()[0];
			if (parameterClass.isAssignableFrom(String.class))
				toSet = String.valueOf(value);
			else if (parameterClass.isAssignableFrom(Integer.class)
					|| parameterClass.isAssignableFrom(int.class))
				toSet = Integer.valueOf((int)value);
			else if (parameterClass.isAssignableFrom(Long.class)
					|| parameterClass.isAssignableFrom(long.class))
				toSet = Long.valueOf((long)value);
			else if (parameterClass.isAssignableFrom(Byte.class)
					|| parameterClass.isAssignableFrom(byte.class))
				toSet = Byte.valueOf((byte)value);
			else if (parameterClass.isAssignableFrom(Short.class)
					|| parameterClass.isAssignableFrom(short.class))
				toSet = Short.valueOf((short)value);
			else if (parameterClass.isAssignableFrom(Double.class)
					|| parameterClass.isAssignableFrom(double.class))
				toSet = Double.valueOf(value);
			else if (parameterClass.isAssignableFrom(Float.class)
					|| parameterClass.isAssignableFrom(float.class))
				toSet = Float.valueOf((float)value);
			else if (parameterClass.isAssignableFrom(Boolean.class)
					|| parameterClass.isAssignableFrom(boolean.class))
				toSet = (value != 0);
			else
				throw new NO_RESOURCES("cannot convert value");

			if (m_autoCommit) tr = m_session.beginTransaction();
			nodeAndMutator.mutator.invoke(nodeAndMutator.node, new Object[] { toSet });
			if (tr != null) tr.commit();

		} catch (Throwable th) {
			if (tr != null)
				tr.rollback();
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to set '" + value + "' to : " + (this.m_name + "/" + propertyName), th);
			throw new NO_RESOURCES(th.getMessage());
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_field_data(java.lang.String, java.lang.String)
	 */
	public void set_field_data(String propertyName, String value)
			throws CDBFieldIsReadOnlyEx, CDBFieldDoesNotExistEx,
			WrongCDBDataTypeEx {
		set_string(propertyName, value);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_string_seq(java.lang.String, java.lang.String[])
	 */
	@SuppressWarnings("unchecked")
	public void set_string_seq(String propertyName, String[] value)
			throws CDBFieldIsReadOnlyEx, CDBFieldDoesNotExistEx {
		NodeAndMutator nodeAndMutator = DOMJavaClassIntrospector.getRecursiveMutatorMethod(propertyName, m_rootNode);
		if (nodeAndMutator == null) {
			AcsJCDBFieldDoesNotExistEx ex = new AcsJCDBFieldDoesNotExistEx();
			ex.setFieldName(propertyName);
			throw ex.toCDBFieldDoesNotExistEx();
		}

		Transaction tr = null;
		try
		{
			if (nodeAndMutator.mutator.getParameterTypes().length != 1) {
				AcsJCDBFieldIsReadOnlyEx acsex = new AcsJCDBFieldIsReadOnlyEx();
				acsex.setFieldName(propertyName);
				throw acsex.toCDBFieldIsReadOnlyEx();
			}

			Object toSet;
			Class parameterClass = nodeAndMutator.mutator.getParameterTypes()[0];
			if (parameterClass.isAssignableFrom(String.class))
			{
				StringBuilder strValue = new StringBuilder(64);
				if (value.length > 0)
				{
					for (int i = 0; i < value.length-1; i++) {
						strValue.append(value[i]).append(',');
					}
					strValue.append(value[value.length-1]);
				}
				toSet = strValue.toString();
			}
			else if (parameterClass.isAssignableFrom(String[].class))
			{
				toSet = value;
			}
			else if (parameterClass.isAssignableFrom(int[].class))
			{
				int[] arr = new int[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = Integer.valueOf(value[i]);
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(long[].class))
			{
				long[] arr = new long[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = Long.valueOf(value[i]);
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(byte[].class))
			{
				byte[] arr = new byte[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = Byte.valueOf(value[i]);
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(short[].class))
			{
				short[] arr = new short[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = Short.valueOf(value[i]);
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(double[].class))
			{
				double[] arr = new double[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = Double.valueOf(value[i]);
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(float[].class))
			{
				float[] arr = new float[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = Float.valueOf(value[i]);
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(boolean[].class))
			{
				boolean[] arr = new boolean[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = Boolean.valueOf(value[i]);
				toSet = arr;
			}
			else
				throw new NO_RESOURCES("cannot convert value");

			if (m_autoCommit) tr = m_session.beginTransaction();
			nodeAndMutator.mutator.invoke(nodeAndMutator.node, new Object[] { toSet });
			if (tr != null) tr.commit();

		} catch (Throwable th) {
			if (tr != null)
				tr.rollback();
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to set '" + Arrays.toString(value) + "' to : " + (this.m_name + "/" + propertyName), th);
			throw new NO_RESOURCES(th.getMessage());
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_long_seq(java.lang.String, int[])
	 */
	@SuppressWarnings("unchecked")
	public void set_long_seq(String propertyName, int[] value)
			throws CDBFieldIsReadOnlyEx, CDBFieldDoesNotExistEx {
		NodeAndMutator nodeAndMutator = DOMJavaClassIntrospector.getRecursiveMutatorMethod(propertyName, m_rootNode);
		if (nodeAndMutator == null) {
			AcsJCDBFieldDoesNotExistEx ex = new AcsJCDBFieldDoesNotExistEx();
			ex.setFieldName(propertyName);
			throw ex.toCDBFieldDoesNotExistEx();
		}

		Transaction tr = null;
		try
		{
			if (nodeAndMutator.mutator.getParameterTypes().length != 1) {
				AcsJCDBFieldIsReadOnlyEx acsex = new AcsJCDBFieldIsReadOnlyEx();
				acsex.setFieldName(propertyName);
				throw acsex.toCDBFieldIsReadOnlyEx();
			}

			Object toSet;
			Class parameterClass = nodeAndMutator.mutator.getParameterTypes()[0];
			if (parameterClass.isAssignableFrom(String.class))
			{
				StringBuilder strValue = new StringBuilder(64);
				if (value.length > 0)
				{
					for (int i = 0; i < value.length-1; i++) {
						strValue.append(value[i]).append(',');
					}
					strValue.append(value[value.length-1]);
				}
				toSet = strValue.toString();
			}
			else if (parameterClass.isAssignableFrom(String[].class))
			{
				String[] arr = new String[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = String.valueOf(value[i]);
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(int[].class))
			{
				int[] arr = new int[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (int)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(long[].class))
			{
				toSet = value;
			}
			else if (parameterClass.isAssignableFrom(byte[].class))
			{
				byte[] arr = new byte[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (byte)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(short[].class))
			{
				short[] arr = new short[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (short)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(double[].class))
			{
				
				double[] arr = new double[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (double)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(float[].class))
			{
				float[] arr = new float[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (float)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(boolean[].class))
			{
				boolean[] arr = new boolean[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (value[i] != 0);
				toSet = arr;
			}
			else
				throw new NO_RESOURCES("cannot convert value");

			if (m_autoCommit) tr = m_session.beginTransaction();
			nodeAndMutator.mutator.invoke(nodeAndMutator.node, new Object[] { toSet });
			if (tr != null) tr.commit();

		} catch (Throwable th) {
			if (tr != null)
				tr.rollback();
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to set '" + Arrays.toString(value) + "' to : " + (this.m_name + "/" + propertyName), th);
			throw new NO_RESOURCES(th.getMessage());
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_double_seq(java.lang.String, double[])
	 */
	@SuppressWarnings("unchecked")
	public void set_double_seq(String propertyName, double[] value)
			throws CDBFieldIsReadOnlyEx, CDBFieldDoesNotExistEx {
		NodeAndMutator nodeAndMutator = DOMJavaClassIntrospector.getRecursiveMutatorMethod(propertyName, m_rootNode);
		if (nodeAndMutator == null) {
			AcsJCDBFieldDoesNotExistEx ex = new AcsJCDBFieldDoesNotExistEx();
			ex.setFieldName(propertyName);
			throw ex.toCDBFieldDoesNotExistEx();
		}

		Transaction tr = null;
		try
		{
			if (nodeAndMutator.mutator.getParameterTypes().length != 1) {
				AcsJCDBFieldIsReadOnlyEx acsex = new AcsJCDBFieldIsReadOnlyEx();
				acsex.setFieldName(propertyName);
				throw acsex.toCDBFieldIsReadOnlyEx();
			}

			Object toSet;
			Class parameterClass = nodeAndMutator.mutator.getParameterTypes()[0];
			if (parameterClass.isAssignableFrom(String.class))
			{
				StringBuilder strValue = new StringBuilder(64);
				if (value.length > 0)
				{
					for (int i = 0; i < value.length-1; i++) {
						strValue.append(value[i]).append(',');
					}
					strValue.append(value[value.length-1]);
				}
				toSet = strValue.toString();
			}
			else if (parameterClass.isAssignableFrom(String[].class))
			{
				String[] arr = new String[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = String.valueOf(value[i]);
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(int[].class))
			{
				int[] arr = new int[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (int)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(long[].class))
			{
				long[] arr = new long[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (long)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(byte[].class))
			{
				byte[] arr = new byte[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (byte)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(short[].class))
			{
				short[] arr = new short[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (short)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(double[].class))
			{
				toSet = value;
			}
			else if (parameterClass.isAssignableFrom(float[].class))
			{
				float[] arr = new float[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (float)value[i];
				toSet = arr;
			}
			else if (parameterClass.isAssignableFrom(boolean[].class))
			{
				boolean[] arr = new boolean[value.length];
				for (int i = 0; i < arr.length; i++)
					arr[i] = (value[i] != 0);
				toSet = arr;
			}
			else
				throw new NO_RESOURCES("cannot convert value");

			if (m_autoCommit) tr = m_session.beginTransaction();
			nodeAndMutator.mutator.invoke(nodeAndMutator.node, new Object[] { toSet });
			if (tr != null) tr.commit();

		} catch (Throwable th) {
			if (tr != null)
				tr.rollback();
			if (!m_silent)
				m_logger.log(AcsLogLevel.NOTICE, "Failed to set '" + Arrays.toString(value) + "' to : " + (this.m_name + "/" + propertyName), th);
			throw new NO_RESOURCES(th.getMessage());
		}
	}
	
}
