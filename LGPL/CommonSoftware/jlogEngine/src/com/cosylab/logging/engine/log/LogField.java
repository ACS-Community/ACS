/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2009
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2009, All rights reserved
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

/**
 * An enumerated describing each field of the log.
 * This allows to have all the attributes describing the fields
 * written together limiting the chance of typos, errors and so on.
 * 
 * NOTE: before introducing this enum each filed had an integer, the entryType.
 * This integer can be obtained with the ordinal() method of enum.
 * 
 * @author acaproni
 *
 */
public enum LogField {
	TIMESTAMP("TimeStamp",Long.class,"TimeStamp",'0'),
	ENTRYTYPE("Entry Type",LogTypeHelper.class,null,'1'),
    SOURCEOBJECT("Source Object",String.class,"SourceObject",'2'),
	FILE("File",String.class,"File",'3'),
	LINE("Line",Integer.class,"Line",'4'),
	ROUTINE("Routine",String.class,"Routine",'5'),
	HOST("Host",String.class,"Host",'6'),
	PROCESS("Process",String.class,"Process",'7'),
	CONTEXT("Context",String.class,"Context",'8'),
	THREAD("Thread",String.class,"Thread",'9'),
	LOGID("Log ID",String.class,"LogId",'A'),
	PRIORITY("Priority",Integer.class,"Priority",'B'),
	URI("URI",String.class,"URI",'C'),
	STACKID("Stack ID",String.class,"StackId",'D'),
	STACKLEVEL("Stack Level",Integer.class,"StackLevel",'E'),
	LOGMESSAGE("Log Message",String.class,null,'F'),
    AUDIENCE("Audience",String.class, "Audience",'G'),
    ARRAY("Array",String.class,"Array",'H'),
    ANTENNA("Antenna",String.class,"Antenna",'I');
	
	/**
	 * The name of the field
	 */
	public final String name;
	
	/**
	 * The class of the field
	 */
	public final Class fieldClass;
	
	/**
	 * The name of the XML attribute containing this field 
	 * 
	 * It is null for non attribute fields (like the entry type or the log message)
	 */
	public final String tagAttribute;
	
	/**
	 * An identifier of the field used by converter tools like the acsLogAssistant 
	 */
	public final char id;
	
	/**
	 * Constructor 
	 * 
	 * @param type The type of this fields
	 * @param name The name of the field
	 * @param fClass The class of the field
	 * @param tag The name of the XML tag containing this attribute
	 *           (<code>null</code> for non tag attributes like the log message)
	 */
	LogField(String name, Class fClass, String tag, char id) {
		if (name==null || fClass==null) {
			throw new IllegalArgumentException("Invalid null initializer");
		}
		this.name=name;
		fieldClass=fClass;
		tagAttribute=tag;
		this.id=Character.toUpperCase(id);
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
	public Class<?> getType() {
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
	
	/**
	 * Return a <code>LogField</code> from its name
	 * 
	 * @return The field with the given name
	 *         or <code>null</code> if a field with that name does not exist
	 */
	public static LogField fromName(String fieldName) {
		for (LogField f: LogField.values()) {
			if (f.name.equals(fieldName)) {
				return f;
			}
		}
		return null;
	}
	
	/**
	 * Return a <code>LogField</code> from its id
	 * 
	 * @return The field with the given id
	 * @throw {@link IllegalArgumentException} if the id does not exist
	 */
	public static LogField fromID(char id) {
		for (LogField f: LogField.values()) {
			if (Character.toUpperCase(id)==Character.toUpperCase(f.id)) {
				return f;
			}
		}
		throw new IllegalArgumentException("Unknown LogField ID: "+id);
	}
}
