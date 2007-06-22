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

import javax.swing.JComboBox;
import javax.swing.JLabel;

import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

import si.ijs.maci.LoggingConfigurablePackage.LogLevels;

/**
 * An helper class to manage log levels
 * 
 * @author acaproni
 *
 */
public class LogLevelHelper {
	
	// The LogLevels
	// The value of this property can be changed by the user
	private LogLevels level;
	
	// The original level i.e. the level set up
	// in the constructor.
	// This can't be changed by the user and is useful to know
	// if something has been changed before applying
	private LogLevels originalLevel;
	
	// The name of the logger
	private String name;
	
	public LogLevelHelper(String name, LogLevels level) {
		if (name==null) {
			throw new IllegalArgumentException("Invalid null logger name in constructor");
		}
		if (level==null) {
			throw new IllegalArgumentException("Invalid null LogLevels in constructor");
		}
		this.level=level;
		resetChanges();
		this.name=name;
	}
	
	/**
	 * Copy level in originalLevel in such a way the modified() will return false.
	 * This is needed after applying changes otherwise itseems that there are
	 * still changes to be applied.
	 *
	 */
	public void resetChanges() {
		this.originalLevel= new LogLevels(level.useDefault,level.minLogLevel,level.minLogLevelLocal);
	}

	public int getGlobalLevel() {
		return level.minLogLevel;
	}

	public void setGlobalLevel(int globalLevel) {
		if (globalLevel<0 || globalLevel>=LogTypeHelper.getNumberOfTypes()) {
			throw new IllegalArgumentException("Global log level out of range: "+globalLevel);
		}
		level.minLogLevel = (short)globalLevel;
	}

	public int getLocalLevel() {
		return level.minLogLevelLocal;
	}

	public void setLocalLevel(int localLevel) {
		if (localLevel<0 || localLevel>=LogTypeHelper.getNumberOfTypes()) {
			throw new IllegalArgumentException("Local log level out of range: "+localLevel);
		}
		level.minLogLevelLocal = (short)localLevel;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		if (name==null) {
			throw new IllegalArgumentException("Invalid null logger name in constructor");
		}
		this.name = name;
	}

	public boolean isUsingDefault() {
		return level.useDefault;
	}

	public void setUseDefault(boolean useDefault) {
		level.useDefault = useDefault;
	}
	
	
	public LogLevels getLogLevels() {
		return level;
	}
	
	/**
	 * Check if the user changed some value
	 * 
	 * @return true if the user changed the log levels
	 */
	public boolean modified() {
		boolean equal = originalLevel.useDefault==level.useDefault;
		equal = equal && originalLevel.minLogLevel==level.minLogLevel;
		equal = equal && originalLevel.minLogLevelLocal==level.minLogLevelLocal;
		return !equal;
	}
}
