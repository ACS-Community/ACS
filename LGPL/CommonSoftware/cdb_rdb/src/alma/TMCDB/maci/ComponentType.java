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
public class ComponentType extends ComponentNode {
    static private final String newline = System.getProperty("line.separator");

    @SuppressWarnings("unused")
	private int ComponentTypeId;
    
    private String Type;
  
    /**
     * Default Constructor for ComponentType.  Setter methods must be used to insert data.
     */
    public ComponentType () {
    }

    public String toString() {
    	String s =  "Component:" + newline;

        s += "\tType: " + Type + newline;

    	return s;
    }

	/**
	 * @return the type
	 */
	public String getType() {
		return Type;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(String type) {
		Type = type;
	}

}