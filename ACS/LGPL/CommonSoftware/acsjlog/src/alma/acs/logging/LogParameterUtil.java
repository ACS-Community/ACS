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
package alma.acs.logging;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.LogRecord;

/**
 * Class that encapsulates access to log record parameters, and handling of a special Property-Map parameter.
 * Some conventions are necessary because we "smuggle" some extra information from the plain-JDK loggers
 * to the ACS-aware log dispatcher. The alternative would be to expose ACS custom loggers to applications, 
 * which seems too ugly for the benefit it provides. 
 * <p>
 * The rules are:
 * <ul>
 * <li>Parameters of type <code>Map</code> are interpreted as name-value pairs 
 *     and show up in the XML log output as <code>&lt;Data Name="key"&gt;value&lt;/Data&gt;</code>   
 * <li>Single-valued parameters show up in the XML log output as 
 *     <code>&lt;Data Name="LoggedParameter"&gt;value&lt;/Data&gt;</code>   
 * <li>There is a special properties <code>Map</code> parameter that contains the key <code>isAcsPropertiesMap</code>.
 *     Those values are recognized by the log formatter and result in special fields being set, instead of
 *     <code>&lt;Data/&gt;</code> elements being constructed.
 *     When adding this map as a log parameter, it should be created using method {@link #createPropertiesMap()}.
 * </ul>
 * Note that this class is stateful: returned values come from the last given <code>LogRecord</code>,
 * which can be set in the constructor or overwritten in {@link #setCurrentLogRecord(LogRecord)}.
 */
public class LogParameterUtil {

    public static final String IS_ACS_PROPERTIES_MAP_KEYNAME = "isAcsPropertiesMap";
	public static final String PARAM_THREAD_NAME = "ThreadName";
    public static final String PARAM_LINE = "Line";				
    public static final String PARAM_HOSTNAME = "HostName"; 	
    public static final String PARAM_STACK_ID = "StackId"; 	
    public static final String PARAM_STACK_LEVEL = "StackLevel"; 	
    public static final String PARAM_PRIORITY = "Priority"; 	
    public static final String PARAM_URI = "Uri"; 	
    public static final String PARAM_PROCESSNAME = "ProcessName"; 	
    public static final String PARAM_SOURCEOBJECT = "SourceObject"; 	
    private LogRecord currentLogRecord;
    
    /** 
     * A <code>Map</code>that is used as an optional special log parameter. 
     * It transports information from the application to the log formatter
     * which could otherwise not be transported using the JDK's <code>LogRecord</code>. 
     */ 
    private Map<String, Object> specialProperties;
    
    private List<Object> otherParameters;


    public LogParameterUtil(LogRecord currentLogRecord) {
        setCurrentLogRecord(currentLogRecord);
    }
    
    /**
     * Creates a new special properties map.
     * @return
     */
    public static Map<String, Object> createPropertiesMap() {
    	Map<String, Object> propMap = new HashMap<String, Object>();
    	propMap.put(IS_ACS_PROPERTIES_MAP_KEYNAME, Boolean.TRUE);
    	return propMap;
    }
    
    /**
     * Tries to find the special properties map among the log parameters. 
     * @return the existing properties map from the current log record, or <code>null</code> if none is found.
     */
    Map<String, Object> extractSpecialPropertiesMap() {
    	if (specialProperties != null) {
    		return specialProperties;
    	}
    	
    	Map<String, Object> propMap = null;    	
        Object[] parameters = currentLogRecord.getParameters();
		if (parameters != null) {
			// the map could be among the parameters
			for (int i = 0; i < parameters.length; i++) {				
				if (parameters[i] instanceof Map && ((Map) parameters[i]).containsKey(IS_ACS_PROPERTIES_MAP_KEYNAME)) {
					if (propMap == null) {
						propMap = (Map) parameters[i];
					}
					else {
						// @todo error handling (this case of having more than one special properties map should never happen though)
					}
				}
			}
		}
    	
		return propMap;
    }
    
    
    /**
     * Sets the log record from which the other methods can extract information.
     * Also parses the special properties map and other parameters.
     * @param logRecord
     */
    public void setCurrentLogRecord(LogRecord logRecord) {
        
        if (currentLogRecord == logRecord) {
            return;
        }
        
        currentLogRecord = logRecord;
        
       	specialProperties = extractSpecialPropertiesMap(); // might be null
       	otherParameters = new ArrayList<Object>();
       	if (currentLogRecord.getParameters() != null) {
	        otherParameters.addAll(Arrays.asList(currentLogRecord.getParameters()));
	        if (specialProperties != null) {
	        	otherParameters.remove(specialProperties);
	        }
       	}
    }

    
    /**
    * Returns parameters of the current log record which are different from the special Properties <code>Map</code>.
    * These parameters may still be of type Map.
    * <p>
    * The returned list is "live", so don't muck with it.
    * @return array of parameters, possibly empty, but never null. 
    */
    public List<Object> getNonSpecialPropertiesMapParameters() {
        return otherParameters;
    }
    
    
	/**
	 * Extracts property with specified name of type long from the special properties map.
	 * @param name			name of the property
	 * @param defaultValue value returned if failed to obtain property value
	 * @return				value, <code>defaultValue</code> on failure
	 */
	public long extractLongProperty(String name, long defaultValue)
	{
		long retVal = defaultValue;
		if (specialProperties != null)
		{
			Object lv = specialProperties.get(name);
			if (lv != null && lv instanceof Long)
				retVal = ((Long) lv).longValue();
		}

		return retVal;
	}

	/**
	 * Extracts property with specified name of type String from the special properties map.
	 * @param name			name of the property
	 * @param defaultValue value returned if failed to obtain property value
	 * @return				value, <code>defaultValue</code> on failure
	 */
    public String extractStringProperty(String name, String defaultValue)
	{
		String retVal = defaultValue;
		if (specialProperties != null)
		{
			Object sv = specialProperties.get(name);
			if (sv != null)
				retVal = sv.toString();
		}

		return retVal;
	}

//	/**
//	 * Extract property with specified name of type Map from the <code>Map</code>
//	 * contained in the object of type <code>java.util.Map</code>.
//	 * @param name			name of the property
//	 * @param defaultValue value returned if failed to obtain property value
//	 * @return				value, <code>defaultValue</code> on failure 
//	 */
//    public Map extractMapProperty(String name, Map defaultValue)
//	{
//		Map retVal = defaultValue;
//		if (specialProperties != null)
//		{
//			Object mv = specialProperties.get(name);
//			if (mv != null && mv instanceof Map)
//				retVal = (Map) mv;
//		}
//
//		return retVal;
//	}
	    
}
