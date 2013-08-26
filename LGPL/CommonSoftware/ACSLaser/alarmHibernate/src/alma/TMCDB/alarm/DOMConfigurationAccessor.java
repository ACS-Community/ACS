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
/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.HashMap;
import java.util.Map;

import org.hibernate.Session;

import alma.cdbErrType.wrappers.AcsJCDBRecordDoesNotExistEx;

import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.cdb.jdal.hibernate.DOMJavaClassIntrospector;

/**
 * @author msekoranja
 *
 */
public class DOMConfigurationAccessor implements ConfigurationAccessor {

	private Session session;
	private Map<String, Object> objectMap = new HashMap<String, Object>();
	
	public void put(String key, Object value) {
		objectMap.put(key, value);
	}
	
	public Object get(String key) {
		return objectMap.get(key);
	}
	
	public Session getSession() {
		return session;
	}

	public void setSession(Session session) {
		this.session = session;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.laser.dao.ConfigurationAccessor#getConfiguration(java.lang.String)
	 */
	@Override
	public String getConfiguration(String curl) throws Exception {
		Object obj = objectMap.get(curl);
		if (obj == null) {
			AcsJCDBRecordDoesNotExistEx ex = new AcsJCDBRecordDoesNotExistEx();
			ex.setCurl(curl);
			throw ex.toCDBRecordDoesNotExistEx();
		}
		
		if (obj instanceof String)
			return (String)obj;

		// get node name only
		String name;
		int pos = curl.lastIndexOf('/');
		if (pos == -1)
			name = curl;
		else
			name = curl.substring(pos+1, curl.length());

		// root
		if (name.length() == 0)
			name = "root";
		
		return "<?xml version='1.0' encoding='ISO-8859-1'?>" +
				DOMJavaClassIntrospector.toXML(DOMJavaClassIntrospector.getNodeXMLName(name, obj), obj);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.laser.dao.ConfigurationAccessor#isWriteable()
	 */
	@Override
	public boolean isWriteable() {
		return true;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.laser.dao.ConfigurationAccessor#setConfiguration(java.lang.String, java.lang.String)
	 */
	@Override
	public void setConfiguration(String path, String data) throws Exception {
		objectMap.put(path, data);
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.laser.dao.ConfigurationAccessor#deleteConfiguration(java.lang.String)
	 */
	@Override
	public void deleteConfiguration(String path) throws Exception {
		if (objectMap.remove(path) == null)
			throw new RuntimeException("configuration '" + path + "' does not exist");
	}

}
