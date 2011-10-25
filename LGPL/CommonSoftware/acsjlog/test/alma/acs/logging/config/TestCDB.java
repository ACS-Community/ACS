/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.logging.config;

import java.util.HashMap;
import java.util.Map;

import com.cosylab.CDB.DALChangeListener;
import com.cosylab.CDB.DALOperations;
import com.cosylab.CDB.DAO;

import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.wrappers.AcsJCDBRecordDoesNotExistEx;

/**
 * 
 * @author hsommer
 */
public class TestCDB implements DALOperations {

	private Map<String, String> curlToXmlMap = new HashMap<String, String>();
	private boolean throwEx = false;
	
    ////////////////////////////////////////////////////
    // Test setup methods
    ////////////////////////////////////////////////////

	String addCurlToXmlMapping(String curl, String xml) {
		return curlToXmlMap.put(curl, xml);
	}
	
	String removeCurl(String curl) {
		return curlToXmlMap.remove(curl);
	}
	
	void setThrowException(boolean throwEx) {
		this.throwEx = throwEx;
	}
	
    ////////////////////////////////////////////////////
    // Implementation of DALOperations
    ////////////////////////////////////////////////////

	/** 
	 * Test impl of the only CDB method which actually gets used by the logging config classes.
	 */
	public String get_DAO(String curl) throws CDBRecordDoesNotExistEx {
		if (throwEx) {
			AcsJCDBRecordDoesNotExistEx ex = new AcsJCDBRecordDoesNotExistEx();
		//	ex.setStringMessage("This is a test exception.");
			throw ex.toCDBRecordDoesNotExistEx();
		}
		return curlToXmlMap.get(curl);
	}

    ////////////////////////////////////////////////////
    // Dummy impl of unused methods from the interface
    ////////////////////////////////////////////////////

	public DAO get_DAO_Servant(String curl) {
		throw new IllegalStateException("Operation not implemented!");
	}

	public void shutdown() {
		throw new IllegalStateException("Operation not implemented!");
	}

	public int add_change_listener(DALChangeListener listener) {
		throw new IllegalStateException("Operation not implemented!");
	}

	public void listen_for_changes(String curl, int listenerID) {
		throw new IllegalStateException("Operation not implemented!");
	}

	public void remove_change_listener(int listenerID) {
		throw new IllegalStateException("Operation not implemented!");
	}

	public String list_nodes(String name) {
		throw new IllegalStateException("Operation not implemented!");
	}

	public String configuration_name() {
		throw new IllegalStateException("Operation not implemented!");
	}

	public String list_daos(String name) {
		throw new IllegalStateException("Operation not implemented!");
	}
}
