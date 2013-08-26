/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.cosylab.cdb.jdal.hibernate.ElementValue;
import com.cosylab.cdb.jdal.hibernate.ExtraDataFeature;
import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 *
 */
public class FaultFamily implements ExtraDataFeature, NameOverrideFeature {
	
	private String name;
	
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
	public Map<String, Object> _ = new InternalElementsMap<String, Object>();

    // extra data support
    private Element _extraData;
    
    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.ExtraDataFeature#getExtraData()
	 */
	public Element getExtraData() {
		return _extraData;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		//return "fault-family";
		return name;
	}
	
	public FaultFamily(String name, String alarmSource, String helpURL, Contact contact)
	{
		this.name = name;

		// add xmlns attributes
		try
		{
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			DOMImplementation impl = builder.getDOMImplementation();
			Document xmldoc = impl.createDocument(null, "data", null);
			_extraData = xmldoc.getDocumentElement();
			_extraData.setAttribute("xmlns", "urn:schemas-cosylab-com:acsalarm-fault-family:1.0");
			_extraData.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
		}
		catch (Throwable th)
		{
			th.printStackTrace();
		}
		
		_.put("alarm-source", new ElementValue(alarmSource));
		if (helpURL != null)
			_.put("help-url", new ElementValue(helpURL));
		_.put("contact", contact);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

}
