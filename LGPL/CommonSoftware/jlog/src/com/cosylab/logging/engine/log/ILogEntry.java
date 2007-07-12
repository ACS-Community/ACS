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
	
	public static final short FIELD_TIMESTAMP = 0;
	public static final short FIELD_ENTRYTYPE = 1;
    public static final short FIELD_SOURCEOBJECT=2;
	public static final short FIELD_FILE = 3;
	public static final short FIELD_LINE = 4;
	public static final short FIELD_ROUTINE = 5;
	public static final short FIELD_HOST = 6;
	public static final short FIELD_PROCESS = 7;
	public static final short FIELD_CONTEXT = 8;
	public static final short FIELD_THREAD = 9;
	public static final short FIELD_LOGID = 10;
	public static final short FIELD_PRIORITY = 11;
	public static final short FIELD_URI = 12;
	public static final short FIELD_STACKID = 13;
	public static final short FIELD_STACKLEVEL = 14;
	public static final short FIELD_LOGMESSAGE = 15;
        public static final short FIELD_AUDIENCE=16;
        public static final short FIELD_ARRAY=17;
        public static final short FIELD_ANTENNA=18;
	
	public static final String DATA_ELEMENT_TAG_NAME = "Data";
	public static final String HEADER_ELEMENT_TAG_NAME = "Header";
	public static final String LOG_ELEMENT_TAG_NAME = "Log";
	public static final String NAME_ATTRIBUTE_NAME = "Name";
	
	public static final String[] fieldNames =
	{
		"TimeStamp",
		"Entry Type",
        "Source Object",
		"File",
		"Line",
		"Routine",
		"Host",
		"Process",
		"Context",
		"Thread",
		"Log ID",
		"Priority",
		"URI",
		"Stack ID",
		"Stack Level",
		"Log Message",
                "Audience",
                "Array",
                "Antenna"} ;

	public static final Class[] fieldClasses = { 
	    Date.class, // Time Stamp
		Integer.class, //EntryType
	    String.class, //Source Object
		String.class, //File
		Integer.class, //Line
		String.class, //Routine 
		String.class, //Host
		String.class, //Process
		String.class, //Context
		String.class, //Thread
		String.class, //LogID
		Integer.class, //Priority
		String.class, //URI
		String.class, //Stack ID
		Integer.class, //Stack Level
		String.class, // Log Message
                String.class,//Audience
                String.class,//Array
                String.class};//Antenna
	
	// The name of the attributes in XML files
	public static final String[] tagAttributes =
	{
		"TimeStamp",
		"", // Place holeder: not an attribute in the XML
	    "SourceObject",
		"File",
		"Line",
		"Routine",
		"Host",
		"Process",
		"Context",
		"Thread",
		"LogId",
		"Priority",
		"URI",
		"StackId",
		"StackLevel",
		"", // Place holeder: not an attribute in the XML
                "Audience",
                "Array",
                "Antenna"};
	    
	public static final String TIME_FORMAT = "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS";
	
	public static final int NUMBER_OF_FIELDS = fieldClasses.length;
	
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
	public Object getField(int fieldIndex);
	
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
