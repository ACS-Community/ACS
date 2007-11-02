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
		private String name;
		private String value;
		
		public AdditionalData(String name, String value) {
			this.name=name;
			this.value=value;
		}
		
		/**
		 * Getter method
		 */ 
		public String getValue() {
			return this.value;
		}
		
		/** 
		 * Getter method
		 */ 
		public String getName() {
			return this.name;
		}
		
	}
	
	/**
	 * An enumerated describing each field of the log.
	 * This allows to have all the attributes describing the fields
	 * written together limiting the chance of typos, errors and so on.
	 * 
	 * NOTE: before introducing this enum each filed has an integer, the entryType.
	 * This integer can be obtained with the ordinal() method of enum.
	 * 
	 * @author acaproni
	 *
	 */
	public enum Field {
		TIMESTAMP("TimeStamp",Date.class,"TimeStamp"),
		ENTRYTYPE("Entry Type",Integer.class,null),
	    SOURCEOBJECT("Source Object",String.class,"SourceObject"),
		FILE("File",String.class,"File"),
		LINE("Line",Integer.class,"Line"),
		ROUTINE("Routine",String.class,"Routine"),
		HOST("Host",String.class,"Host"),
		PROCESS("Process",String.class,"Process"),
		CONTEXT("Context",String.class,"Context"),
		THREAD("Thread",String.class,"Thread"),
		LOGID("Log ID",String.class,"LogId"),
		PRIORITY("Priority",Integer.class,"Priority"),
		URI("URI",String.class,"URI"),
		STACKID("Stack ID",String.class,"StackId"),
		STACKLEVEL("Stack Level",Integer.class,"StackLevel"),
		LOGMESSAGE("Log Message",String.class,null),
	    AUDIENCE("Audience",String.class, "Audience"),
	    ARRAY("Array",String.class,"Array"),
	    ANTENNA("Antenna",String.class,"Antenna");
		
		// The name of the field
		private String name;
		
		// The class of the field
		private Class fieldClass;
		
		// The name of the XML attribute containing this field 
		// 
		// It is null for non attribute fields (like the entry type or the log message)
		private String tagAttribute;
		
		/**
		 * Constructor 
		 * 
		 * @param type The type of this fields
		 * @param name The name of the field
		 * @param fClass The class of the field
		 * @param tag The name of the XML tag containing this attribute
		 *           (<code>null</code> for non tag attributes like the log message)
		 */
		Field(String name, Class fClass, String tag) {
			if (name==null || fClass==null) {
				throw new IllegalArgumentException("Invalid null initializer");
			}
			this.name=name;
			fieldClass=fClass;
			tagAttribute=tag;
		}
		
		/**
		 * Getter
		 * 
		 * @return The name of the field
		 * 
		 */
		public String getName() {
			return name;
		}
		
		/**
		 * Getter
		 * 
		 * @return The class of the field
		 * 
		 */
		public Class getType() {
			return fieldClass;
		}
		
		/**
		 * Getter
		 * 
		 * @return The XML tag name of the field
		 * 
		 */
		public String getTagAttribute() {
			return tagAttribute;
		}
	}
	
	public static final String DATA_ELEMENT_TAG_NAME = "Data";
	public static final String HEADER_ELEMENT_TAG_NAME = "Header";
	public static final String LOG_ELEMENT_TAG_NAME = "Log";
	public static final String NAME_ATTRIBUTE_NAME = "Name";
	    
	public static final String TIME_FORMAT = "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS";
	
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
	public Object getField(Field field);
	
	/**
	 * 
	 * @return The type of the log
	 */
	public Integer getType();
	
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
