/*
 * ALMA - Atacama Large Millimeter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * Copyright by AUI (in the framework of the ALMA collaboration),
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY, without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 * File Component.java
 */
package alma.TMCDB.maci;

@SuppressWarnings("serial")
public class ComponentLogger implements java.io.Serializable {
    static private final String newline = System.getProperty("line.separator");

    private int minLogLevel;
    private int minLogLevelLocal;

    /**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public ComponentLogger () {
    }

    public String toString() {
    	String s =  "ComponentLogger:" + newline;

        s += "\tminLogLevel: " + minLogLevel + newline;

        s += "\tminLogLevelLocal: " + minLogLevelLocal + newline;

    	return s;
    }

	/**
	 * @return the minLogLevel
	 */
	public int getMinLogLevel() {
		return minLogLevel;
	}

	/**
	 * @param minLogLevel the minLogLevel to set
	 */
	public void setMinLogLevel(int minLogLevel) {
		this.minLogLevel = minLogLevel;
	}

	/**
	 * @return the minLogLevelLocal
	 */
	public int getMinLogLevelLocal() {
		return minLogLevelLocal;
	}

	/**
	 * @param minLogLevelLocal the minLogLevelLocal to set
	 */
	public void setMinLogLevelLocal(int minLogLevelLocal) {
		this.minLogLevelLocal = minLogLevelLocal;
	}


}