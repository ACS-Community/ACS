/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.logging.config;

import java.io.Reader;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import alma.acs.logging.ACSCoreLevel;

/**
 * Represents a full set of logging config data,
 * to be used either as default (per container), or (once specified) per component.
 * <p>
 * One instance of {@link alma.acs.logging.config.LogConfig} 
 * can hold several instances of this class, 
 * to represent config data for different loggers.
 * Methods {@link #takeCdbContainerXml(String)} and {@link #takeCdbComponentXml(String)}
 * can parse the XML configuration for logging as it comes from the CDB.
 * Any other functionality such as CDB access and updates etc should be kept outside of this class. 
 * 
 * @author hsommer
 * created 06.05.2006 17:23:45
 */
public class LogConfigData {
	
    // names of CDB attributes for logger config 
	public static final String CDBNAME_CENTRALIZED_LOGGER = "CentralizedLogger";
	public static final String CDBNAME_MIN_LOG_LEVEL_LOCAL = "MinLogLevelLocal"; // not official yet
	public static final String CDBNAME_MIN_LOG_LEVEL = "MinCachePriority";
	public static final String CDBNAME_EXPEDITED_DISPATCH_LEVEL = "MaxCachePriority";
	public static final String CDBNAME_DISPATCH_PACKETSIZE = "CacheSize";
	public static final String CDBNAME_FLUSHPERIOD_SECONDS = "FlushPeriodSeconds";

	// for the often-used min levels we offer properties, to avoid having an otherwise not needed CDB configuration 
	public final static String PROPERTYNAME_MIN_LOG_LEVEL_LOCAL = "ACS.logstdout"; // todo: rename to "ACS.log.minlevel.local"
	public final static String PROPERTYNAME_MIN_LOG_LEVEL = "ACS.log.minlevel.remote";

	private String centralizedLoggerName; // CDB: CentralizedLogger
    private int dispatchPacketSize; // CDB: CacheSize
    private int minLogLevel; // CDB: MinCachePriority
    private int minLogLevelLocal; 
    private int expeditedDispatchLevel; // CDB: MaxCachePriority
	private int flushPeriodSeconds;

    private DocumentBuilder domBuilder;

    
    LogConfigData() {
        setDefaultValues();
    }

    /**
     * Sets the default values for the various fields.
     * The values can later be changed, e.g. using methods {@link #takeCdbContainerXml(String)} or {@link #takeCdbComponentXml(String)}. 
     */
    void setDefaultValues() {
        centralizedLoggerName = "Log";
        dispatchPacketSize = 30;
        expeditedDispatchLevel = ACSCoreLevel.ACS_LEVEL_ALERT;
        flushPeriodSeconds = 10;
        
        // local min level: env variable ACS_LOG_STDOUT (mapped to property ACS.logstdout by start scripts) overrides the hardcoded default
    	Integer minLevelLocalValue = Integer.getInteger(PROPERTYNAME_MIN_LOG_LEVEL_LOCAL);
    	if (minLevelLocalValue != null) {
    		minLogLevelLocal = minLevelLocalValue.intValue();
    	}
    	else {
            minLogLevelLocal = ACSCoreLevel.ACS_LEVEL_DEBUG;
    	}
        // remote min level: should only be used for tests etc where need low-level logs but don't have a CDB at hand.
    	Integer minLevelValue = Integer.getInteger(PROPERTYNAME_MIN_LOG_LEVEL);
    	if (minLevelValue != null) {
    		minLogLevel = minLevelValue.intValue();
    	}
    	else {
            minLogLevel = ACSCoreLevel.ACS_LEVEL_DEBUG;
    	}
    }
    
    
    /**
     * Copy constructor
     */
    LogConfigData(LogConfigData other) {
        this.centralizedLoggerName = other.centralizedLoggerName;
        this.dispatchPacketSize = other.dispatchPacketSize;
        this.minLogLevel = other.minLogLevel;
        this.minLogLevelLocal = other.minLogLevelLocal;
        this.expeditedDispatchLevel = other.expeditedDispatchLevel;
        this.flushPeriodSeconds = other.flushPeriodSeconds;
    }

    public boolean equals(Object obj) {
    	if (obj == null) return false;
    	if (this == obj) return true;
    	try {
    		LogConfigData other = (LogConfigData) obj;
    		return (
    				this.centralizedLoggerName.equals(other.centralizedLoggerName) &&
    				this.dispatchPacketSize == other.dispatchPacketSize &&
    				this.minLogLevel == other.minLogLevel &&
    				this.minLogLevelLocal == other.minLogLevelLocal &&
    				this.expeditedDispatchLevel == other.expeditedDispatchLevel && 
    				this.flushPeriodSeconds == other.flushPeriodSeconds
    				);
    	}
    	catch (ClassCastException ex) {
    		return false;
    	}
    }
    
    /**
     * Gets the name of the central log service to which logs will be sent.
     */
    public String getCentralizedLoggerName() {
        return centralizedLoggerName;
    }

    /**
     * Gets the number of log records that will be sent at once to the log service,
     * notwithstanding any shipment in smaller bunches controlled by shipment timeouts
     * and immediate sending of more important logs.
     */
    public int getDispatchPacketSize() {
        return dispatchPacketSize;
    }

    /**
     * Gets the log level below which messages should be suppressed for remote logging.
     */
    public int getMinLogLevel() {
        return minLogLevel;
    }

    /**
     * Gets the log level below which messages should be suppressed for local (stdout) logging.
     */
    public int getMinLogLevelLocal() {
        return minLogLevelLocal;
    }

    /**
     * Gets the log level from which on messages are sent off immediately to the remote logging service,
     * even if fewer log records than given by {@link #getDispatchPacketSize()} are available.
     */
    public int getExpeditedDispatchLevel() {
        return expeditedDispatchLevel;
    }
    
	public int getFlushPeriodSeconds() {
		return flushPeriodSeconds;
	}

    /**
     * Parses the provided XML string, which should be a &lt;Container&gt; element from the CDB.
     * @param xml
     * @throws LogConfigException if XML parsing fails, the wrong root element or namespace were found,
     *                              or if some of the config values contained in the XML were not usable
     *                              (while all readable values were actually ingested sucessfully).
     */
    void takeCdbContainerXml(String xml) throws LogConfigException {
        Element rootElement;
        try {
            if (domBuilder == null) {
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                factory.setNamespaceAware(true);
                domBuilder = factory.newDocumentBuilder();
            }
            Reader xmlReader = new StringReader(xml);
            rootElement = domBuilder.parse(new InputSource(xmlReader)).getDocumentElement();
        } catch (Exception ex) {
            throw new LogConfigException("failed to parse config xml: " + xml, ex);
        }
        // check if we got the correct XML 
        if (rootElement == null || rootElement.getTagName() == null ||
            !rootElement.getTagName().equals("Container") ||
            !rootElement.getNamespaceURI().equals("urn:schemas-cosylab-com:Container:1.0") ) {
            throw new LogConfigException("Expected root node 'Container' with namespace 'urn:schemas-cosylab-com:Container:1.0' in config xml " + xml);
        }
        
        StringBuffer errMsg = new StringBuffer(); // to gather error info and defer throwing the exception till the end         
        String cdbCentralizedLogger = rootElement.getAttribute("CentralizedLogger");
        if (cdbCentralizedLogger != null && cdbCentralizedLogger.trim().length() > 0) {
            centralizedLoggerName = cdbCentralizedLogger.trim();
        }
        dispatchPacketSize = getIntAttribute(rootElement, CDBNAME_DISPATCH_PACKETSIZE, errMsg, dispatchPacketSize);
        minLogLevel = getIntAttribute(rootElement, CDBNAME_MIN_LOG_LEVEL, errMsg, minLogLevel);
        minLogLevelLocal = getIntAttribute(rootElement, CDBNAME_MIN_LOG_LEVEL_LOCAL, errMsg, minLogLevelLocal);
        expeditedDispatchLevel = getIntAttribute(rootElement, CDBNAME_EXPEDITED_DISPATCH_LEVEL, errMsg, expeditedDispatchLevel);
        flushPeriodSeconds = getIntAttribute(rootElement, CDBNAME_FLUSHPERIOD_SECONDS, errMsg, flushPeriodSeconds);        
        
        if (errMsg.length() > 0) {
            throw new LogConfigException("Problems during configuration: " + errMsg.toString());
        }
    }
    
    /**
     * Parses the provided XML string, which should be a &lt;Component&gt; element from the CDB.
     * This method is not yet implemented, because the CDB currently does not configure logging
     * for individual components.
     * @param xml
     * @throws LogConfigException always, to remind us that this is not yet implemented.
     */
    void takeCdbComponentXml(String xml) throws LogConfigException {
        throw new LogConfigException("Logging configuration per component not yet supported by the CDB. " + 
                "This method must be implemented once the CDB offers such XML config.");
    }
    
    
    /**
     * Helper method to extract an integer attribute from a DOM element.
     * @param domElem the DOM element
     * @param attrName name of attribute of <code>domElem</code>
     * @param errMsg a buffer to which an error message gets added if applicable
     * @param defaultRet default return value, used in case of error.
     * @return the attribute value, or <code>defaultRet</code> if there was an error or the attribute was missing.
     */
    private int getIntAttribute(Element domElem, String attrName, StringBuffer errMsg, int defaultRet) { 
        int ret = defaultRet;
        String cdbAttr = domElem.getAttribute(attrName);
        if (cdbAttr != null && cdbAttr.trim().length() > 0) {
            try {
                ret = Integer.parseInt(cdbAttr);
            } catch (NumberFormatException ex) {
                errMsg.append("Attribute " + attrName + ": ignored invalid value " + cdbAttr + ". ");
            }
        }
        return ret;
    }

}