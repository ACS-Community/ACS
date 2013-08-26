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

import org.w3c.dom.Element;

import com.cosylab.cdb.jdal.hibernate.ExtraDataFeature;
import com.cosylab.cdb.jdal.hibernate.ExtraDataFeatureUtil;


@SuppressWarnings("serial")
public class Component extends ComponentNode implements ExtraDataFeature {
    static private final String newline = System.getProperty("line.separator");

    @SuppressWarnings("unused")
	public int ComponentId;
    @SuppressWarnings("unused")
	private int ConfigurationId;
    
    private String Name;
    private String Code;
    private String Type;

    private String Container;
    
    private String ImplLang;
 
    @SuppressWarnings("unused")
    private Container ContainerInstance;
    
    @SuppressWarnings("unused")
    private ComponentType ComponentTypeInstance;

    private int KeepAliveTime;
    private boolean Autostart;
    private boolean Default;

    private ComponentLogger ComponentLogger;

    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    public String Path;

    // is control device or not? 
    public boolean Control;

    // extra data support
    private String Data;
    //private Element extraData;
    //private boolean extraDataParsed = false;
 
    // non-control devices support
    // must be public to be accessible, but should not have getter to be come visible as node
    public String XMLDoc;
    
    // must be public to be accessible, but should not have getter to be come visible as node
    public String URN;

    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.ExtraDataFeature#getExtraData()
	 */
	public Element getExtraData() {
		if (Data == null || Data.isEmpty())
			return null;
		//else if (!extraDataParsed)
		{
			try {
				/* extraData = */ return ExtraDataFeatureUtil.getExtraDataMap(Data);
			} catch (Throwable th) {
				System.err.println("Failed to parse extra data for component: " + Name);
				th.printStackTrace();
				return null;
			}
			//extraDataParsed = true;
		}
		
		//return extraData;
	}

	/**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public Component () {
    }

    public String toString() {
    	String s =  "Component:" + newline;

        s += "\tName: " + Name + newline;

        s += "\tType: " + getType() + newline;

        s += "\tCode: " + Code + newline;

        s += "\tImplLang: " + ImplLang + newline;

        s += "\tContainer: " + getContainer() + newline;

        s += "\tAutostart: " + Autostart + newline;

        s += "\tDefault: " + Default + newline;

        s += "\tKeepAliveTime: " + KeepAliveTime + newline;

        s += "\tComponentLogger: " + ComponentLogger + newline;

    	return s;
    }

	/**
	 * @return the autostart
	 */
	public Boolean isAutostart() {
		if (Name == null) return null;
		return Autostart;
	}

	/**
	 * @param autostart the autostart to set
	 */
	public void setAutostart(boolean autostart) {
		Autostart = autostart;
	}

	/**
	 * @return the code
	 */
	public String getCode() {
		return Code;
	}

	/**
	 * @param code the code to set
	 */
	public void setCode(String code) {
		Code = code;
	}

	/**
	 * @return the componentLogger
	 */
	public ComponentLogger getComponentLogger() {
		return ComponentLogger;
	}

	/**
	 * @param componentLogger the componentLogger to set
	 */
	public void setComponentLogger(ComponentLogger componentLogger) {
		ComponentLogger = componentLogger;
	}

	/**
	 * @return the container
	 */
	public String getContainer() {
		if (ContainerInstance == null)
			return "*";
		else
		{
			// must be always non-null
			if (ContainerInstance.Path.length() == 0 || ContainerInstance.Path.equals("/"))
				return ContainerInstance.Name;
			else
				return ContainerInstance.Path + "/" + ContainerInstance.Name;
		}
	}

	/**
	 * @return the default
	 */
	public Boolean isDefault() {
		return Default;
	}

	/**
	 * @param default1 the default to set
	 */
	public void setDefault(boolean default1) {
		Default = default1;
	}

	/**
	 * @return the keepAliveTime
	 */
	public Integer getKeepAliveTime() {
		return KeepAliveTime;
	}

	/**
	 * @param keepAliveTime the keepAliveTime to set
	 */
	public void setKeepAliveTime(int keepAliveTime) {
		KeepAliveTime = keepAliveTime;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return Name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		Name = name;
	}

	/**
	 * @return the type
	 */
	public String getType() {
		return ComponentTypeInstance.getType();
	}

	/**
	 * @return the implLang
	 */
	public String getImplLang() {
		return ImplLang;
	}

	/**
	 * @param implLang the implLang to set
	 */
	public void setImplLang(String implLang) {
		ImplLang = implLang;
	}

}
