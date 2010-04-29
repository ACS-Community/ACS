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
package alma.TMCDB.baci;

import java.util.Map;

import org.w3c.dom.Element;

import com.cosylab.cdb.jdal.hibernate.ExtraDataFeature;
import com.cosylab.cdb.jdal.hibernate.ExtraDataFeatureUtil;
import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;

/**
 * Node only - only holds a list of properties.
 */
@SuppressWarnings("serial")
public class ComponentData implements java.io.Serializable, ExtraDataFeature {
    static private final String newline = System.getProperty("line.separator");

    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
    @SuppressWarnings("unused")
	public Map<String, Object> _ = new InternalElementsMap<String, Object>();

    // extra data support
    private Element _extraData;
    
    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.ExtraDataFeature#getExtraData()
	 */
	public Element getExtraData() {
		return _extraData;
	}

	public void setData(String xmlData) throws Throwable
	{
		_extraData = ExtraDataFeatureUtil.getExtraDataMap(xmlData);
	}

	/**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public ComponentData () {
    }

    public String toString() {
    	String s =  "ComponentData:" + newline;

    	s += "\t(properties): " + _ + newline;

    	return s;
    }

}