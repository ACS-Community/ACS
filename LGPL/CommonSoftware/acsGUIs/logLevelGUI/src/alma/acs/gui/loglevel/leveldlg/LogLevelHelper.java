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
package alma.acs.gui.loglevel.leveldlg;

import si.ijs.maci.LoggingConfigurablePackage.LogLevels;

import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * An helper class to manage log levels
 * 
 * @author acaproni
 *
 */
public class LogLevelHelper {
	
	// The LogLevels
	// The value of this property can be changed by the user
	private LogLevels levels;
	
	// The original level i.e. the level set up
	// in the constructor.
	// This can't be changed by the user and is useful to know
	// if something has been changed before applying
	private LogLevels originalLevel;
	
	/**
	 * The name of the logger
	 */
	private final String name;
	
	/**
	 * Constructor 
	 * 
	 * @param name The name of the logger
	 * @param levels The log levels
	 */
	public LogLevelHelper(String name, LogLevels levels) {
		if (name==null) {
			throw new IllegalArgumentException("Invalid null logger name in constructor");
		}
		if (levels==null) {
			throw new IllegalArgumentException("Invalid null LogLevels in constructor");
		}
		//System.out.println("Building s LogLevelHelper for "+name+": <"+levels.useDefault+", "+levels.minLogLevel+", "+levels.minLogLevelLocal+">");
		this.levels=levels;
		resetChanges();
		this.name=name;
	}
	
	/**
	 * Constructor 
	 * 
	 * @param name The name of the logger
	 * @param levels The log levels
	 * @param defaults The default log levels -- yatagai[30-Apr-2009] this is not used any more
	 */
	@Deprecated
	public LogLevelHelper(String name, LogLevels levels, LogLevels defaults) {
		this(name, levels);
	}
	
	/**
	 * Copy level in <code>originalLevel</code> in such a way the modified() will return false.
	 * This is needed after applying changes otherwise it seems that there are
	 * still changes to be applied.
	 *
	 */
	public void resetChanges() {
		this.originalLevel= new LogLevels(levels.useDefault,levels.minLogLevel,levels.minLogLevelLocal);
	}

	public int getGlobalLevel() {
		return levels.minLogLevel;
	}

	public void setGlobalLevel(LogTypeHelper globalLevel) {
		if (globalLevel==null) {
			throw new IllegalArgumentException("Global log level out of range: "+globalLevel);
		}
		levels.minLogLevel = (short)globalLevel.acsCoreLevel.value;
	}

	public int getLocalLevel() {
		return levels.minLogLevelLocal;
	}

	public void setLocalLevel(LogTypeHelper localLevel) {
		if (localLevel==null) {
			throw new IllegalArgumentException("Invalid null log level");
		}
		levels.minLogLevelLocal = (short)localLevel.acsCoreLevel.value;
	}

	public String getName() {
		return name;
	}

	public boolean isUsingDefault() {
		return levels.useDefault;
	}

	public void setUseDefault(boolean useDefault) {
		levels.useDefault = useDefault;
	}
	
	
	public LogLevels getLogLevels() {
		return levels;
	}
	
	/**
	 * Check if the user changed some value
	 * 
	 * @return true if the user changed the log levels
	 */
	public boolean modified() {
		boolean equal = originalLevel.useDefault==levels.useDefault;
		equal = equal && originalLevel.minLogLevel==levels.minLogLevel;
		equal = equal && originalLevel.minLogLevelLocal==levels.minLogLevelLocal;
		return !equal;
	}
}
