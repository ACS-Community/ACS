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
package com.cosylab.logging.engine.log;

import java.util.Date;
import java.util.Vector;

import java.io.Serializable;

/**
 * The interface for all the log entry.
 * There are two kinds of log entries, one holding an XML node
 * and another one lighter that has to be used whenever a Node
 * is not required.
 * The interface contains the common methods of each log entry class.
 * 
 *  The interface extends the Serializable whose methods are used 
 *  to read/write the logs from the cache
 * 
 * @author acaproni
 *
 */
public interface ILogEntry extends Serializable {
	
	/**
	 * Each additional data is a couple <name,value>
	 * The list of additional datas (see below) stores
	 * objects of this class
	 * 
	 * @author acaproni
	 *
	 */
	public class AdditionalData {
		public final  String name;
		public final  String value;
		
		public AdditionalData(String name, String value) {
			this.name=name;
			this.value=value;
		}
		
		@Override
		public boolean equals(Object o) {
			if (o==null || !(o instanceof AdditionalData)) {
				return super.equals(o);
			}
			AdditionalData d=(AdditionalData)o;
			return this.name.equals(d.name) && this.value.equals(d.value); 
		}
		
	}
	
	public static final String DATA_ELEMENT_TAG_NAME = "Data";
	public static final String HEADER_ELEMENT_TAG_NAME = "Header";
	public static final String LOG_ELEMENT_TAG_NAME = "Log";
	public static final String NAME_ATTRIBUTE_NAME = "Name";
	    
	/**
	 * 
	 * @return The XML string representing the object
	 */
	public String toXMLString();
	
	/**
	 * 
	 * @return A string to dump the log
	 */
	public String toString();
	
	/**
	 * 
	 * @return true if the log has additional data
	 */
	public boolean hasDatas();
	
	/**
	 * 
	 * @param fieldIndex
	 * @return Return the object in the field of the passed index
	 */
	public Object getField(LogField field);
	
	/**
	 * 
	 * @return The type of the log
	 */
	public LogTypeHelper getType();
	
	/**
	 *	Add data to this log
	 *  It is a couple, <name,value>
	 * 
	 * @param name The name, i.e. the key of the pair
	 * @param value The value of the field
	 */
	public void addData(String name, String value);
	
	/**
	 * @return a Vector of AdditionalData
	 *         null if if the log does not contain any additional data
	 */
	public Vector<AdditionalData> getAdditionalData();
}
