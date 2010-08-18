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
 */
/*
 * Created on Feb 9, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.cdb.jdal;

import java.util.HashMap;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import org.omg.PortableServer.POA;

import com.cosylab.CDB.WDAOPOA;

import alma.acs.logging.AcsLogLevel;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.CDBFieldIsReadOnlyEx;
import alma.cdbErrType.WrongCDBDataTypeEx;
import alma.cdbErrType.wrappers.AcsJCDBFieldDoesNotExistEx;

/**
 * @author dvitas
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class WDAOImpl extends WDAOPOA {
	private final DAOImpl daoImpl;
	private final WDALImpl wdal; // used for saving - nicer if it is an interface
	private final Logger m_logger;
	/**
	 * 
	 */
	public WDAOImpl(WDALImpl wdal, String name, DAOImpl daoImpl, POA poa, Logger logger) {
		super();
		if (wdal == null) {
			throw new NullPointerException("wdal must not be null");
		}
		if (daoImpl == null) {
			throw new NullPointerException("daoImpl must not be null");
		}
		this.wdal = wdal;
		this.daoImpl = daoImpl;
		m_logger = logger;
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_long(java.lang.String, int)
	 */
	public void set_long(String propertyName, int value) throws CDBFieldDoesNotExistEx, CDBFieldIsReadOnlyEx {
		try{
		setField(propertyName, String.valueOf(value));
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_double(java.lang.String, double)
	 */
	public void set_double(String propertyName, double value) throws CDBFieldDoesNotExistEx, CDBFieldIsReadOnlyEx {
		try{
		setField(propertyName, String.valueOf(value));
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_string(java.lang.String, java.lang.String)
	 */
	public void set_string(String propertyName, String value) throws CDBFieldDoesNotExistEx, CDBFieldIsReadOnlyEx {
		try{
		setField(propertyName, value);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_field_data(java.lang.String, java.lang.String)
	 */
	public void set_field_data(String propertyName, String value) throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx, CDBFieldIsReadOnlyEx {
		try{
		setField(propertyName, value);
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_string_seq(java.lang.String, java.lang.String[])
	 */
	public void set_string_seq(String propertyName, String[] value) throws CDBFieldDoesNotExistEx, CDBFieldIsReadOnlyEx {
		StringBuffer strValue = new StringBuffer(64);
		for (int i = 0; i < value.length; i++) {
			strValue.append(value[i]).append(',');
		}
		try{
		setField(propertyName, strValue.toString());
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_long_seq(java.lang.String, int[])
	 */
	public void set_long_seq(String propertyName, int[] value) throws CDBFieldDoesNotExistEx, CDBFieldIsReadOnlyEx {
		StringBuffer strValue = new StringBuffer(64);
		for (int i = 0; i < value.length; i++) {
			strValue.append(value[i]).append(',');
		}
		try{
		setField(propertyName, strValue.toString());
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.WDAOOperations#set_double_seq(java.lang.String, double[])
	 */
	public void set_double_seq(String propertyName, double[] value) throws CDBFieldDoesNotExistEx, CDBFieldIsReadOnlyEx {
		StringBuffer strValue = new StringBuffer(64);
		for (int i = 0; i < value.length; i++) {
			strValue.append(value[i]).append(',');
		}
		try{
		setField(propertyName, strValue.toString());
		}catch(AcsJCDBFieldDoesNotExistEx e){
			throw e.toCDBFieldDoesNotExistEx();
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DAOOperations#get_long(java.lang.String)
	 */
	public int get_long(String propertyName) throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		return daoImpl.get_long(propertyName);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DAOOperations#get_double(java.lang.String)
	 */
	public double get_double(String propertyName) throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		return daoImpl.get_double(propertyName);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DAOOperations#get_string(java.lang.String)
	 */
	public String get_string(String propertyName) throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		return daoImpl.get_string(propertyName);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DAOOperations#get_field_data(java.lang.String)
	 */
	public String get_field_data(String propertyName) throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		return daoImpl.get_field_data(propertyName);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DAOOperations#get_string_seq(java.lang.String)
	 */
	public String[] get_string_seq(String propertyName) throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		return daoImpl.get_string_seq(propertyName);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DAOOperations#get_long_seq(java.lang.String)
	 */
	public int[] get_long_seq(String propertyName) throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		return daoImpl.get_long_seq(propertyName);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DAOOperations#get_double_seq(java.lang.String)
	 */
	public double[] get_double_seq(String propertyName) throws WrongCDBDataTypeEx, CDBFieldDoesNotExistEx {
		return daoImpl.get_double_seq(propertyName);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.CDB.DAOOperations#destroy()
	 */
	public void destroy() {
		daoImpl.destroy();
		try {
			POA poa = getPOA();
			byte[] thisId = poa.servant_to_id(this);
			poa.deactivate_object(thisId);
		} catch (Exception e) {
			m_logger.log(AcsLogLevel.NOTICE,  "Exception destroying object " + this +" : " + e);
			e.printStackTrace();
		}
	}

	/**
	 * @return
	 */
	public XMLTreeNode getRootNode() {
		return daoImpl.getRootNode();
	}

	/**
	 * @return
	 */
	public String getName() {
		return daoImpl.getName();
	}

	/**
	 * @return
	 */
	public POA getPOA() {
		return daoImpl.getPOA();
	}

	private void setField(String strFieldName, String value) throws AcsJCDBFieldDoesNotExistEx {
		XMLTreeNode pNode = getRootNode();
		StringTokenizer st = new StringTokenizer(strFieldName, "/");
		String fieldName = st.nextToken();
		while (st.hasMoreTokens()) {
			pNode = (XMLTreeNode) pNode.m_subNodesMap.get(fieldName);
			if (pNode == null){
				AcsJCDBFieldDoesNotExistEx e = new AcsJCDBFieldDoesNotExistEx();
				e.setFieldName(strFieldName);
				throw e;
			}
			fieldName = st.nextToken();
		}
		String currentValue = (String) pNode.m_fieldMap.get(fieldName);
		if (currentValue == null) {
			m_logger.log(AcsLogLevel.NOTICE, "setField():'" + getName() + "' Unable to find field: '" + strFieldName + "'");
			AcsJCDBFieldDoesNotExistEx e = new AcsJCDBFieldDoesNotExistEx();
			e.setFieldName(strFieldName);
			throw e;
		}
		m_logger.log(AcsLogLevel.INFO,"setField():'" + getName() + "' set '" + strFieldName + "'=" + value);  
		// set value in memory
		m_logger.log(AcsLogLevel.DEBUG,"setField(): - Set value in memory");  
		pNode.m_fieldMap.put(fieldName, value);
		// and on disk
		m_logger.log(AcsLogLevel.DEBUG,"setField(): - Set value in disk");  
		HashMap map = new HashMap();
		map.put(strFieldName, value);
		try {
		        m_logger.log(AcsLogLevel.DEBUG,"setField(): - Saving Changes");  
			wdal.saveChanges(getName(), map);
		}
		catch(AcsJCDBFieldDoesNotExistEx e) {
			throw e;
		}
		catch(Exception e) {
			// TODO do something with this exception
			e.printStackTrace();
		}
	}
}
