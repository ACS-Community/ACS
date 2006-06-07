package com.cosylab.logging.engine.ACS;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import alma.acs.util.XmlNormalizer;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;
import com.ximpleware.NavException;
import com.ximpleware.ParseException;
import com.ximpleware.VTDGen;
import com.ximpleware.VTDNav;

/**
 * ACSLogParserVTD uses the VTD XML parser to parse XML logs. VTD is used for
 * performance reasons. For more information on VTD, see their web site.
 * 
 * @author sharring
 * 
 * @see http://vtd-xml.sourceforge.net/
 * @see ACSLogParser
 */
public class ACSLogParserVTD implements ACSLogParser 
{
	
	private Vector<AdditionalData> getAdditionalData(VTDNav vNav, ByteArrayOutputStream os, byte[] bytesArray) throws NavException
	{
		/**
		 * Gets a Vector<AdditionalData> from VTD XML parser (using VTDNav navigation)
		 * @param vtdNav the navigator to use to navigate the parsed xml
		 * @param os output stream to use for conversion of bytes to useful formatted data.
		 * @param bytesXML the bytes (containing XML) that are being referenced by the navigator.
		 * @return Vector<AdditionalData> populated with the additional data for the log message, if any, else null
		 * @throws NavException if navigation encounteres problems
		 */
		Vector<AdditionalData> retVal = null;
		if(vNav.toElement(VTDNav.FIRST_CHILD, ILogEntry.DATA_ELEMENT_TAG_NAME)) {
			do {
				String dataName = null;
				String dataValue = null;
				if (vNav.hasAttr(ILogEntry.NAME_ATTRIBUTE_NAME)) {
					dataName = getString(vNav, os, ILogEntry.NAME_ATTRIBUTE_NAME, bytesArray);
				}
				int valueIndex = vNav.getText();
				if(valueIndex != -1) {
					dataValue = vNav.toNormalizedString(valueIndex);
				}
				if(null != dataName && null != dataValue) {
					if(null == retVal) {
						retVal = new Vector<AdditionalData>();
					}
					retVal.add(new AdditionalData(dataName, dataValue));
				}
		    }
		    while (vNav.toElement(VTDNav.NEXT_SIBLING)); // navigate to next sibling
		}
		return retVal;
	}
	
	/**
	 * Gets a String from VTD XML parser (using VTDNav navigation)
	 * @param vtdNav the navigator to use to navigate the parsed xml
	 * @param os output stream to use for conversion of bytes to useful formatted data.
	 * @param attrName the name of the field we are searching for
	 * @param bytesXML the bytes (containing XML) that are being referenced by the navigator.
	 * @return String populated with the attribute's value
	 * @throws NavException if navigation encounteres problems
	 */
	private String getString(VTDNav vtdNav, ByteArrayOutputStream os, 
			String attrName, byte[] bytesXML) throws NavException
	{
		String retVal = null;
		int tIndex = vtdNav.getAttrVal(attrName);
		int tOffset = vtdNav.getTokenOffset(tIndex);
		int tLen = vtdNav.getTokenLength(tIndex);
		os.reset();
		os.write(bytesXML, tOffset, tLen); //write the fragment out
		retVal = os.toString();
		return retVal;
	}
	
	/**
	 * Gets an Integer from VTD XML parser (using VTDNav navigation)
	 * @param vtdNav the navigator to use to navigate the parsed xml
	 * @param os output stream to use for conversion of bytes to useful formatted data.
	 * @param attrName the name of the field we are searching for
	 * @param bytesXML the bytes (containing XML) that are being referenced by the navigator.
	 * @return Integer populated with the attribute's value
	 * @throws NavException if navigation encounteres problems
	 */	
	private Integer getInteger(VTDNav vtdNav, ByteArrayOutputStream os,
			String attrName, byte[] bytesXML) throws NavException
	{
		Integer retVal = null;
		int tIndex = vtdNav.getAttrVal(attrName);
		int tOffset = vtdNav.getTokenOffset(tIndex);
		int tLen = vtdNav.getTokenLength(tIndex);
		os.reset();
		os.write(bytesXML, tOffset, tLen); //write the fragment out
		retVal = new Integer(os.toString());
		return retVal;
	}

	/**
	 * Gets a Long from VTD XML parser (using VTDNav navigation)
	 * @param vtdNav the navigator to use to navigate the parsed xml
	 * @param os output stream to use for conversion of bytes to useful formatted data.
	 * @param attrName the name of the field we are searching for
	 * @param bytesXML the bytes (containing XML) that are being referenced by the navigator.
	 * @return Long populated with the attribute's value
	 * @throws NavException if navigation encounteres problems
	 * @throws LogParseException if parsing fails
	 */	
	private Long getLongFromTimestamp(VTDNav vtdNav, ByteArrayOutputStream os,
			String attrName, byte[] bytesXML) throws LogParseException, NavException
	{
		Long retVal = null;
		int tIndex = vtdNav.getAttrVal(attrName);
		int tOffset = vtdNav.getTokenOffset(tIndex);
		int tLen = vtdNav.getTokenLength(tIndex);
		os.reset();
		os.write(bytesXML, tOffset, tLen); //write the fragment out
		try {
			SimpleDateFormat dateFormat = new SimpleDateFormat(ILogEntry.TIME_FORMAT);
			Date date = dateFormat.parse(os.toString());
			retVal = new Long(date.getTime());
		}
		catch(java.text.ParseException pEx) {
			throw new LogParseException("Error parsing date!", pEx);
		}
		return retVal;
	}
	
	/**
	 * Implements required method of ACSLogParser interface.
	 * @param xmlString the XML string to parse
	 * @throws LogParseException when problems are encountered parsing an XML message.
	 * @see ACSLogParser
	 */
	public ILogEntry parse(String xmlString) throws LogParseException 
	{
		LogEntry retVal = null; 
		byte[] bytesArray = xmlString.getBytes();	
		
		VTDGen vg = null;
		try {
			try {
				vg = new VTDGen();
				vg.setDoc(bytesArray);
				vg.parse(false); // set namespace awareness to false for now
			}
			catch (Exception e) {
				/* There was an exception parsing the log, but before giving up 
				 * we try to fix markup issues inside the text that is contained in the XML */
				vg.clear();
				xmlString = XmlNormalizer.normalizeXMLEmbeddedTextOnly(xmlString);
				bytesArray = xmlString.getBytes();
				vg.setDoc(xmlString.getBytes());
				vg.parse(false);
			}
			retVal = makeLogEntryFromParsedXML(vg, bytesArray, xmlString);
		}
		catch(ParseException ex) {
			ex.printStackTrace();
			throw new LogParseException("Error parsing with VTD!", ex);
		}
			
		return retVal;
	}


	private LogEntry makeLogEntryFromParsedXML(VTDGen vtdGen, byte[] bytesArray, String xmlString)
		throws LogParseException
	{
		LogEntry retVal = null;
		try 
		{
			VTDNav vn = vtdGen.getNav();
			if (vn.toElement(VTDNav.ROOT)) // to root element
			{
				if (vn.matchElement(ILogEntry.LOG_ELEMENT_TAG_NAME)) 
				{
					// navigate to child 
					if(vn.toElement(VTDNav.FIRST_CHILD)) {
						if(vn.matchElement(ILogEntry.HEADER_ELEMENT_TAG_NAME)) {
							vn.toElement(VTDNav.NEXT_SIBLING); // navigate to sibling
						}
					}
				}
				if(vn.matchElement(ILogEntry.HEADER_ELEMENT_TAG_NAME)) {
					vn.toElement(VTDNav.NEXT_SIBLING); // navigate to sibling
				}
			
				Long milliseconds = null;
				Integer entryType = null; 
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
				ByteArrayOutputStream os = new ByteArrayOutputStream();
				
				if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_INFO))) {
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_INFO);
				}
				else if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_TRACE))) {
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_TRACE);
				}
				else if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_DEBUG))) {
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_DEBUG);
				}
				else if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_WARNING))) {
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_WARNING);
				}
				else if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_ERROR))) {
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_ERROR);
				}
				else if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_EMERGENCY)))
				{
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_EMERGENCY);
				}
				else if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_ALERT)))
				{
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_ALERT);
				}
				else if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_CRITICAL)))
				{
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_CRITICAL);
				}
				else if(vn.matchElement(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_NOTICE)))
				{
					entryType = new Integer(LogTypeHelper.ENTRYTYPE_NOTICE);
				}
				// test for timestamp attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_TIMESTAMP])){
					milliseconds = getLongFromTimestamp(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_TIMESTAMP], bytesArray);
				}
				// test for File attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_FILE])){
					fileName = this.getString(vn, os, LogEntry.fieldNames[LogEntry.FIELD_FILE], bytesArray);
				}
				// test for Line attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_LINE])){
					line = getInteger(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_LINE], bytesArray);
				}
				// test for Routine attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_ROUTINE])){
					routineName = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_ROUTINE], bytesArray);
				}
				// test for host attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_HOST])){
					hostName = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_HOST], bytesArray);
				}
				// test for process attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_PROCESS])){
					processName = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_PROCESS], bytesArray);
				}				         
				// test for context attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_CONTEXT])){
					contextName = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_CONTEXT], bytesArray);
				}
				// test for thread attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_THREAD])){
					threadName = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_THREAD], bytesArray);
				}
				// test for logid attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_LOGID])){
					logId = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_LOGID], bytesArray);
				}
				// test for priority attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_PRIORITY])){
					priority = getInteger(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_PRIORITY], bytesArray);
				}
				// test for uri attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_URI])){
					uri = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_URI], bytesArray);
				}
				// test for stackid attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_STACKID])){
					stackId = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_STACKID], bytesArray);
				}
				// test for stacklevel attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_STACKLEVEL])){
					stackLevel = getInteger(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_STACKLEVEL], bytesArray);
				}
				// test for logMessage attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_LOGMESSAGE])){
					logMessage = this.getString(vn, os, 
							LogEntry.fieldNames[LogEntry.FIELD_LOGMESSAGE], bytesArray);
				}
				else if(-1 != vn.getText()){
					// if logMessage isn't an attribute, it's CDATA, so check there
					int tIndex = vn.getText();
					int tOffset = vn.getTokenOffset(tIndex);
					int tLen = vn.getTokenLength(tIndex);
					os.reset();
					os.write(bytesArray, tOffset, tLen); //write the fragment out
					logMessage = os.toString();
				}
				// test for srcObject attribute
				if (vn.hasAttr(LogEntry.fieldNames[LogEntry.FIELD_SOURCEOBJECT])){
					srcObjectName = getString(vn, os, LogEntry.fieldNames[LogEntry.FIELD_SOURCEOBJECT], bytesArray);
				}
				
				// get the additional data, if present
				Vector<AdditionalData> extraDataList = getAdditionalData(vn, os, bytesArray);
				
				retVal = new LogEntry(milliseconds,	entryType, fileName,
						line, routineName, hostName, processName,
						contextName, threadName, logId, priority,
						uri, stackId, stackLevel, logMessage, srcObjectName, extraDataList);
			}
			else {
				throw new LogParseException("Error: VTD cannot find root element; string is: " + xmlString);
			}
		}
		catch (NavException navEx) {
			navEx.printStackTrace();
			throw new LogParseException("Error navigating parsed XML with VTD navigator!", navEx);
		}
		return retVal;
	}
}
