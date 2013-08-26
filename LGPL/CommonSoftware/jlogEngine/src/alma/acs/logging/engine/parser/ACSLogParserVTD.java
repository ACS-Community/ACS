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
package alma.acs.logging.engine.parser;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import alma.acs.util.IsoDateFormat;
import alma.acs.util.XmlNormalizer;

import com.cosylab.logging.engine.ACS.LogParseException;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;

/**
 * ACSLogParserVTD uses the VTD XML parser to parse XML logs. VTD is used for
 * performance reasons. For more information on VTD, see their web site.
 * 
 * @author sharring
 * 
 * @see http://vtd-xml.sourceforge.net/
 * @see ACSLogParser
 */
public class ACSLogParserVTD implements ACSLogParser {
	
	/**
	 * Support field to pass null while reflection invokes methods having no params 
	 */
	private final static Object[] nullObj = new Object[0];
	
	/** 
	 * private instance of VTDGen used for parsing XML
	 */
	private Object vtdGen=null;
	
	/**
	 * VTDGen.clear();
	 */
	private Method VTDGen_clear=null;
	
	/**
	 * VTDGen.setDoc(byte[]);
	 */
	private Method VTDGen_setDoc=null;
	
	/**
	 * VTDGen.parse(boolean);
	 */
	private Method VTDGen_parse=null;
	
	/**
	 * VTDGen.getNav();
	 */
	private Method VTDGen_getNav=null;
	
	/**
	 * GETNav.ROOT
	 */
	private int NAV_ROOT;
	
	/**
	 * GETNav.FIRST_CHILD
	 */
	private int NAV_FIRST_CHILD;
	
	/**
	 * GETNav.NEXT_SIBLING
	 */
	private int NAV_NEXT_SIBLING;
	
	/**
	 * VTDNav.toElement(int)
	 */
	private Method VTDNav_toElement=null;
	
	/**
	 * VTDNav.toElement(int,String)
	 */
	private Method VTDNav_toElement_String=null;
	
	/**
	 * VTDNav.matchElement(String)
	 */
	private Method VTDNav_matchElement=null;
	
	/**
	 * VTDNav.hasAttr(String)
	 */
	private Method VTDNav_hasAttr=null;
	
	/**
	 * VTDNav.toString(int)
	 */
	private Method VTDNav_toString=null;
	
	/**
	 * VTDNav.getText();
	 */
	private Method VTDNav_getText=null;
	
	/**
	 * VTDNav.getAttrVal(String);
	 */
	private Method VTDNav_getAttrVal=null;
	
	/**
	 * VTDNav.getAttrCount();
	 */
	private Method VTDNav_getAttrCount=null;
	
	/**
	 * VTDNav.getTokenOffset(int);
	 */
	private Method VTDNav_getTokenOffset=null;
	
	/**
	 * VTDNav.getTokenCount();
	 */
	private Method VTDNav_getTokenCount=null;
	
	/**
	 * VTDNav.getTokenLength(int);
	 */
	private Method VTDNav_getTokenLength=null;
	
	/**
	 * VTDNav.toNormalizedStringgetTokenLength(int);
	 */
	private Method VTDNav_toNormalizedString=null;

	/**
	 * Constructor.
	 */
	public ACSLogParserVTD() throws Exception {
		initReflection();
	}
	
	/**
	 * Initialize all the fields and methods with reflection.
	 *  
	 * @throws Exception If fails instantiating fields and methods
	 */
	private void initReflection() throws Exception {
		// VTDGen class 
		Class vtdGenClass = Class.forName("com.ximpleware.VTDGen");
		
		// VTDNav class 
		Class vtdNavClass = Class.forName("com.ximpleware.VTDNav");
		
		// VTDGEn constructor
		Class[] paramsClasses = new Class[0];
		Constructor constructor = vtdGenClass.getConstructor(paramsClasses);
		Object[] params = new Object[0];
		vtdGen = constructor.newInstance(params);
		
		// Get VTDGen.clear();
		VTDGen_clear = vtdGenClass.getMethod("clear", new Class[0]);
		
		// Get VTDGen.setDoc();
		Class[] setDocParmsClasses = new Class[] {
				byte[].class
		};
		VTDGen_setDoc= vtdGenClass.getMethod("setDoc", setDocParmsClasses);
		
		// Get VTDGen.parse();
		Class[] parseParamsClasses = new Class[] {
				boolean.class
		};
		VTDGen_parse = vtdGenClass.getMethod("parse", parseParamsClasses);
		
		// Get VTDGen.getNav();
		VTDGen_getNav = vtdGenClass.getMethod("getNav", new Class[0]);
		
		// Get VTDNav.ROOT
		Field rootField = vtdNavClass.getField("ROOT");
		NAV_ROOT=rootField.getInt(vtdNavClass);
		
		// Get VTDNav.FIRST_CHILD
		Field firstChildField = vtdNavClass.getField("FIRST_CHILD");
		NAV_FIRST_CHILD=firstChildField.getInt(vtdNavClass);
		
		// Get VTDNav.NEXT_SIBLING
		Field nextSiblingField = vtdNavClass.getField("NEXT_SIBLING");
		NAV_NEXT_SIBLING=nextSiblingField.getInt(vtdNavClass);
		
		// Get VTDNav.toElement(int)
		Class[] toElementsParamsClasses = new Class[] {
				int.class
		};
		VTDNav_toElement = vtdNavClass.getMethod("toElement", toElementsParamsClasses);
		
		// Get VTDNav.matchElement(String)
		Class[] matchElementsParamsClasses = new Class[] {
				java.lang.String.class
		};
		VTDNav_matchElement = vtdNavClass.getMethod("matchElement", matchElementsParamsClasses);
		
		// Get VTDNav.hasAttr(String)
		Class[] hasAttrParamsClasses = new Class[] {
				java.lang.String.class
		};
		VTDNav_hasAttr = vtdNavClass.getMethod("hasAttr", hasAttrParamsClasses);
		
		// Get VTDNav.getAttrCount();
		VTDNav_getAttrCount=vtdNavClass.getMethod("getAttrCount", new Class[0]);
		
		// Get VTDNav.toString(int)
		Class[] toStringParamsClasses = new Class[] {
				int.class
		};
		VTDNav_toString=vtdNavClass.getMethod("toString", toStringParamsClasses);
		
		// Get VTDNav.getText();
		VTDNav_getText=vtdNavClass.getMethod("getText", new Class[0]);
		
		// Get VTDNav.getAttrVal(String);
		Class[] getAttrValParamsClasses = new Class[] {
				java.lang.String.class
		};
		VTDNav_getAttrVal=vtdNavClass.getMethod("getAttrVal", getAttrValParamsClasses);
		
		// Get VTDNav.getTokenOffset(int)
		Class[] getTokenOffsetParamsClasses = new Class[] {
				int.class
		};
		VTDNav_getTokenOffset=vtdNavClass.getMethod("getTokenOffset", getTokenOffsetParamsClasses);
		
		// Get VTDNav.getTokenLength(int);
		Class[] getTokenLengthParamsClasses = new Class[] {
				int.class
		};
		VTDNav_getTokenLength=vtdNavClass.getMethod("getTokenLength", getTokenLengthParamsClasses);
		
		// Get VTDNav.getTokenCount();
		VTDNav_getTokenCount=vtdNavClass.getMethod("getTokenCount", new Class[0]);
		
		// Get VTDNav.toNormalizedString(int)
		Class[] toNormalizedStringParamsClasses = new Class[] {
				int.class
		};
		VTDNav_toNormalizedString=vtdNavClass.getMethod("toNormalizedString", toNormalizedStringParamsClasses);
		
		// Get VTDNav.toElement(int,String)
		Class[] toElement_Str_dStringParamsClasses = new Class[] {
				int.class,
				java.lang.String.class
		};
		VTDNav_toElement_String=vtdNavClass.getMethod("toElement", toElement_Str_dStringParamsClasses);
	}

	/**
	 * Gets a Vector<AdditionalData> from VTD XML parser (using VTDNav navigation)
	 * 
	 * @param vtdNav the navigator to use to navigate the parsed xml
	 * @param os output stream to use for conversion of bytes to useful formatted data.
	 * @param bytesXML the bytes (containing XML) that are being referenced by the navigator.
	 * @return Vector<AdditionalData> populated with the additional data for the log message, if any, else null
	 * @throws NavException if navigation encounters problems
	 */
	private Vector<AdditionalData> getAdditionalData(Object vNav, ByteArrayOutputStream os, byte[] bytesArray) 
	throws Exception {
		Vector<AdditionalData> retVal = null;
		if((Boolean)VTDNav_toElement_String.invoke(vNav, new Object[] {
				NAV_FIRST_CHILD, 
				ILogEntry.DATA_ELEMENT_TAG_NAME})) {
			do {
				String dataName = null;
				String dataValue = null;
				if ((Boolean)VTDNav_hasAttr.invoke(vNav,ILogEntry.NAME_ATTRIBUTE_NAME)) {
					dataName = getString(vNav, os, ILogEntry.NAME_ATTRIBUTE_NAME, bytesArray);
				} 
				int valueIndex = (Integer)VTDNav_getText.invoke(vNav,nullObj);
				if(valueIndex != -1) {
					dataValue = (String)VTDNav_toNormalizedString.invoke(vNav,valueIndex);
				}
				if(null != dataName && null != dataValue) {
					if(null == retVal) {
						retVal = new Vector<AdditionalData>();
					}
					retVal.add(new AdditionalData(dataName, dataValue));
				}
		    }
		    while ((Boolean)VTDNav_toElement.invoke(vNav, NAV_NEXT_SIBLING)); // navigate to next sibling
		}
		return retVal;
	}
	
	/**
	 * Gets a String from VTD XML parser (using VTDNav navigation)
	 * 
	 * @param vtdNav the navigator to use to navigate the parsed xml
	 * @param os output stream to use for conversion of bytes to useful formatted data.
	 * @param attrName the name of the field we are searching for
	 * @param bytesXML the bytes (containing XML) that are being referenced by the navigator.
	 * @return String populated with the attribute's value
	 * @throws NavException if navigation encounters problems
	 */
	private String getString(Object vtdNav, ByteArrayOutputStream os, 
			String attrName, byte[] bytesXML) throws Exception
	{
		String retVal = null;
		int tIndex = (Integer)VTDNav_getAttrVal.invoke(vtdNav, attrName);
		int tOffset = (Integer)VTDNav_getTokenOffset.invoke(vtdNav, tIndex);
		int tLen = (Integer)VTDNav_getTokenLength.invoke(vtdNav,tIndex);
		os.reset();
		os.write(bytesXML, tOffset, tLen); //write the fragment out
		retVal = os.toString();
		return retVal;
	}
	
	/**
	 * Gets an Integer from VTD XML parser (using VTDNav navigation)
	 * 
	 * @param vtdNav the navigator to use to navigate the parsed xml
	 * @param os output stream to use for conversion of bytes to useful formatted data.
	 * @param attrName the name of the field we are searching for
	 * @param bytesXML the bytes (containing XML) that are being referenced by the navigator.
	 * @return Integer populated with the attribute's value
	 * @throws NavException if navigation encounters problems
	 */	
	private Integer getInteger(Object vtdNav, ByteArrayOutputStream os,
			String attrName, byte[] bytesXML) throws Exception 	{
		Integer retVal = null;
		int tIndex = (Integer)VTDNav_getAttrVal.invoke(vtdNav, attrName);
		int tOffset = (Integer)VTDNav_getTokenOffset.invoke(vtdNav, tIndex);
		int tLen = (Integer)VTDNav_getTokenLength.invoke(vtdNav,tIndex);
		os.reset();
		os.write(bytesXML, tOffset, tLen); //write the fragment out
		retVal = new Integer(os.toString());
		return retVal;
	}

	/**
	 * Gets a Long from VTD XML parser (using VTDNav navigation)
	 * 
	 * @param vtdNav the navigator to use to navigate the parsed xml
	 * @param os output stream to use for conversion of bytes to useful formatted data.
	 * @param attrName the name of the field we are searching for
	 * @param bytesXML the bytes (containing XML) that are being referenced by the navigator.
	 * @return Long populated with the attribute's value
	 * @throws NavException if navigation encounters problems
	 * @throws LogParseException if parsing fails
	 */	
	private Long getLongFromTimestamp(Object vtdNav, ByteArrayOutputStream os,
			String attrName, byte[] bytesXML) throws LogParseException, Exception
	{
		Long retVal = null;
		int tIndex = (Integer)VTDNav_getAttrVal.invoke(vtdNav, attrName);
		int tOffset = (Integer)VTDNav_getTokenOffset.invoke(vtdNav, tIndex);
		int tLen = (Integer)VTDNav_getTokenLength.invoke(vtdNav,tIndex);
		os.reset();
		os.write(bytesXML, tOffset, tLen); //write the fragment out
		try {
			SimpleDateFormat dateFormat = new IsoDateFormat();
			Date date = dateFormat.parse(os.toString());
			retVal = Long.valueOf(date.getTime());
		}
		catch(java.text.ParseException pEx) {
			throw new LogParseException("Error parsing date", pEx);
		}
		return retVal;
	}
	
	/**
	 * Implements required method of ACSLogParser interface.
	 * 
	 * @param xmlString the XML string to parse
	 * @throws LogParseException when problems are encountered parsing an XML message.
	 * @see ACSLogParser
	 */
	public synchronized ILogEntry parse(String xmlString) throws LogParseException {
		if (xmlString==null || xmlString.length()==0) {
			throw new IllegalArgumentException("Invalid string to parse");
		}
		LogEntry retVal = null; 
		byte[] bytesArray = xmlString.getBytes();	
		try {
			try {
				VTDGen_clear.invoke(vtdGen, nullObj);
				VTDGen_setDoc.invoke(vtdGen, bytesArray);
				VTDGen_parse.invoke(vtdGen, false); // set namespace awareness to false for now
			}
			catch (Exception e) {
				/* There was an exception parsing the log, but before giving up 
				 * we try to fix markup issues inside the text that is contained in the XML */
				VTDGen_clear.invoke(vtdGen, nullObj);
				xmlString = XmlNormalizer.normalizeXMLEmbeddedTextOnly(xmlString);
				bytesArray = xmlString.getBytes();
				VTDGen_setDoc.invoke(vtdGen, xmlString.getBytes());
				VTDGen_parse.invoke(vtdGen, false);
			}
			retVal = makeLogEntryFromParsedXML(bytesArray, xmlString);
		}
		catch(Exception ex) {
			throw new LogParseException("Error parsing with VTD!", ex);
		}
			
		return retVal;
	}

	/**
	 * Returns the entry type as an Integer for the current log that is being parsed.
	 * 
	 * @param vn an instance of a VTDNav object to use in ascertaining the entry type of the log that is being parsed.
	 * @return a <code>LogTypeHelper</code> denoting the type of entry (e.g. INFO, DEBUG, WARNING, etc.)<BR>
	 *         <code>null</code> if the entry does not match with any log type
	 * @throws NavException if the VTDNav navigation fails for some reason.
	 * @see LogTypeHelper
	 */
	private LogTypeHelper determineEntryType(Object vtdNav) throws Exception {
		for (LogTypeHelper logType: LogTypeHelper.values()) {
			if ((Boolean)VTDNav_matchElement.invoke(vtdNav,logType.logEntryType)) {
				return logType;
			}
		}
		return null;
	}


	/**
	 * Creates a LogEntry from raw XML, using a VTD XML parser. 
	 * 
	 * @param xmlString the XML string that is being parsed.
	 * @param bytesArray the array of bytes (also containing the xml string that we are parsing, in byte form) 
	 *                   to be used by VTD.
	 * @param vtdGen the instance of VTDGen to use for parsing the XML
	 * @return A LogEntry populated with the data for the log entry contained in the XML string passed in.
	 * @throws LogParseException if the parsing fails
	 */
	private LogEntry makeLogEntryFromParsedXML(byte[] bytesArray, String xmlString) throws LogParseException
	{
		// TODO: this method, though relatively simple, is a bit long; consider making it shorter
		LogEntry retVal = null;
		Object vtdNav;
		try 
		{
			vtdNav = VTDGen_getNav.invoke(vtdGen, nullObj);
			if ((Boolean)VTDNav_toElement.invoke(vtdNav, NAV_ROOT)) // to root element
			{ 
				if ((Boolean)VTDNav_matchElement.invoke(vtdNav,ILogEntry.LOG_ELEMENT_TAG_NAME)) 
				{ 
					// navigate to child 
					if ((Boolean)VTDNav_toElement.invoke(vtdNav, NAV_FIRST_CHILD)) {
						if((Boolean)VTDNav_matchElement.invoke(vtdNav,ILogEntry.HEADER_ELEMENT_TAG_NAME)) {
							VTDNav_toElement.invoke(vtdNav, NAV_NEXT_SIBLING); // navigate to sibling
						}
					}
				}
				if((Boolean)VTDNav_matchElement.invoke(vtdNav,ILogEntry.HEADER_ELEMENT_TAG_NAME)) {
					VTDNav_toElement.invoke(vtdNav, NAV_NEXT_SIBLING); // navigate to sibling
				}
				
				Long milliseconds = null;
				Integer line = null;
				Integer priority = null;
				Integer stackLevel = null;
				String fileName = null;
				String routineName = null;
				String hostName = null;
				String processName = null;
				String contextName = null;
				String threadName = null;
				String logId = null;
				String uri = null;
				String stackId = null;
				String logMessage = null;
				String srcObjectName = null;
				String audience = null;
				String array = null;
				String antenna = null;

				ByteArrayOutputStream os = new ByteArrayOutputStream();
				LogTypeHelper entryType = determineEntryType(vtdNav);

				// test for timestamp attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.TIMESTAMP.getTagAttribute())){
					milliseconds = getLongFromTimestamp(vtdNav, os, 
							LogField.TIMESTAMP.getTagAttribute(), bytesArray);
				}
				
				// test for File attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.FILE.getTagAttribute())){
					fileName = this.getString(vtdNav, os, LogField.FILE.getTagAttribute(), bytesArray);
				}
				
				// test for Line attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.LINE.getTagAttribute())){
					line = getInteger(vtdNav, os, 
							LogField.LINE.getTagAttribute(), bytesArray);
				}
				
				// test for Routine attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.ROUTINE.getTagAttribute())){
					routineName = this.getString(vtdNav, os, 
							LogField.ROUTINE.getTagAttribute(), bytesArray);
				}
				
				// test for host attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.HOST.getTagAttribute())){
					hostName = this.getString(vtdNav, os, 
							LogField.HOST.getTagAttribute(), bytesArray);
				}
				
				// test for process attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.PROCESS.getTagAttribute())){
					processName = this.getString(vtdNav, os, 
							LogField.PROCESS.getTagAttribute(), bytesArray);
				}
				
				// test for context attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.CONTEXT.getTagAttribute())){
					contextName = this.getString(vtdNav, os, 
							LogField.CONTEXT.getTagAttribute(), bytesArray);
				}
				
				// test for thread attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.THREAD.getTagAttribute())){
					threadName = this.getString(vtdNav, os, 
							LogField.THREAD.getTagAttribute(), bytesArray);
				}
				
				// test for logid attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.LOGID.getTagAttribute())){
					logId = this.getString(vtdNav, os, 
							LogField.LOGID.getTagAttribute(), bytesArray);
				}
				
				// test for priority attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.PRIORITY.getTagAttribute())){
					priority = getInteger(vtdNav, os, 
							LogField.PRIORITY.getTagAttribute(), bytesArray);
				}
				
				// test for uri attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.URI.getTagAttribute())){
					uri = this.getString(vtdNav, os, 
							LogField.URI.getTagAttribute(), bytesArray);
				}
				
				// test for stackid attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.STACKID.getTagAttribute())){
					stackId = this.getString(vtdNav, os, 
							LogField.STACKID.getTagAttribute(), bytesArray);
				}
				
				// test for stacklevel attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.STACKLEVEL.getTagAttribute())){
					stackLevel = getInteger(vtdNav, os, 
							LogField.STACKLEVEL.getTagAttribute(), bytesArray);
				}
				
				// test for srcObject attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.SOURCEOBJECT.getTagAttribute())){
					srcObjectName = getString(vtdNav, os, LogField.SOURCEOBJECT.getTagAttribute(), bytesArray);
				} 
				
				// test for Audience attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.AUDIENCE.getTagAttribute())){
					audience = getString(vtdNav, os, LogField.AUDIENCE.getTagAttribute(), bytesArray);
				} 
				
				// test for Array attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.ARRAY.getTagAttribute())){
					array = getString(vtdNav, os, LogField.ARRAY.getTagAttribute(), bytesArray);
				} 
				
				// test for Antenna attribute
				if ((Boolean)VTDNav_hasAttr.invoke(vtdNav,LogField.ANTENNA.getTagAttribute())){
					antenna = getString(vtdNav, os, LogField.ANTENNA.getTagAttribute(), bytesArray);
				} 
				
				// Get the body of the message
				int tIndex = (Integer)VTDNav_getText.invoke(vtdNav,nullObj);
				if (tIndex!=-1) {
//					int tOffset = vn.getTokenOffset(tIndex);
//					int tLen = vn.getTokenLength(tIndex);
					logMessage = (String)VTDNav_toString.invoke(vtdNav, tIndex);
				}
				
				// get the additional data, if present
				Vector<AdditionalData> extraDataList = getAdditionalData(vtdNav, os, bytesArray);
				
				// If the logMessage is null then it could be that the data section has
				// been written before the body of the log so in this case we try to
				// get again the body of the message.
				if (logMessage==null) {
					try {
						logMessage=getLogMessage(vtdNav);
					} catch (Exception e) {
						logMessage=null;
					}
						
				}
				
				retVal = new LogEntry(milliseconds,	entryType.ordinal(), fileName,
						line, routineName, hostName, processName,
						contextName, threadName, logId, priority,
						uri, stackId, stackLevel, logMessage, srcObjectName,
						audience,array,antenna,
						extraDataList);
			}
			else {
				throw new LogParseException("Error: VTD cannot find root element; string is: " + xmlString);
			}
		}
		catch (Exception navEx) {
			navEx.printStackTrace();
			throw new LogParseException("Error navigating parsed XML with VTD navigator!", navEx);
		}
		return retVal;
	}
	
	/**
	 * Get the body of a log.
	 * <P>
	 * The log message must be read with this method because of the mixed content
	 * 
	 * @param vn The VTDNav
	 * @return The message of the log
	 * @throws Exception
	 */
	private String getLogMessage(Object vn) throws Exception {
		Class textIterClass = Class.forName("com.ximpleware.TextIter");

		Constructor textIterCtor = textIterClass.getConstructor(new Class[0]);
		Object textIter = textIterCtor.newInstance(new Object[0]);
		
		Method ti_getNext = textIterClass.getMethod("getNext", new Class[0]);
		
		Class[] paramsClasses = new Class[1];
		paramsClasses[0]=Class.forName("com.ximpleware.VTDNav");
		Method ti_touch = textIterClass.getMethod("touch", paramsClasses);
		
		VTDNav_toElement.invoke(vn, 0);
		ti_touch.invoke(textIter, vn);
		int idx;
		
		do {
			idx = (Integer)ti_getNext.invoke(textIter, nullObj);
			if (idx!=-1) {
				Object[] pars = new Object[1];
				pars[0]=idx;
				String str = (String)VTDNav_toString.invoke(vn,pars);
				return str;
			} 
		} while (idx!=-1);
		return null;
	}
}
