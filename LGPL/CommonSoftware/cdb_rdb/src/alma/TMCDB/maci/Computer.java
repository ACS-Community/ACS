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
 * File Computer.java
 */
package alma.TMCDB.maci;

@SuppressWarnings("serial")
public class Computer implements java.io.Serializable {
    static private final String newline = System.getProperty("line.separator");

    @SuppressWarnings("unused")
	private int ComputerId;
    
    private String HostName;

    /**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public Computer () {
    }

    public String toString() {
    	String s =  "ComputerId:" + newline;

        s += "\tHostName: " + HostName + newline;

    	return s;
    }

	/**
	 * @return the hostName
	 */
	public String getHostName() {
		return HostName;
	}

	/**
	 * @param hostName the hostName to set
	 */
	public void setHostName(String hostName) {
		HostName = hostName;
	}

}