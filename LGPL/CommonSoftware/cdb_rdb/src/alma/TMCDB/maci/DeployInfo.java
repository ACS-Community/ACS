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
 * File DeployInfo.java
 */
package alma.TMCDB.maci;

import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;

import alma.acs.tmcdb.ContainerStartupOption;
import alma.acs.tmcdb.logic.ContainerStartupOptionHelper;

import com.cosylab.cdb.jdal.hibernate.HibernateUtil;
import com.cosylab.cdb.jdal.logging.AcsLoggerHelper;


@SuppressWarnings("serial")
public class DeployInfo implements java.io.Serializable {
    static private final String newline = System.getProperty("line.separator");

    private String Type;
    private String TypeModifiers;
    
    /**
     * The CDB "Flags" are not mapped to a single database attribute, but get constructed from {@link #ContainerStartupOption}.
     */
    private String Flags;
    
    /**
     * All {@link ContainerStartupOption}s for this container are mapped by hibernate to this Set.
     * They get converted to {@link #Flags} inside {@link #getFlags()}.
     */
    private Set ContainerStartupOption;
    
    private Integer KeepAliveTime;
    private Boolean StartOnDemand;

    
    private Computer Computer;
    // mapped via Computer (probably could be done via hibernate)
    @SuppressWarnings("unused")
    private String Host;
    

    public String toString() {
    	String s =  "DeployInfo:" + newline;

        s += "\tType: " + Type + newline;

        s += "\tTypeModifiers: " + TypeModifiers + newline;

        s += "\tHost: " + getHost() + newline;

        s += "\tFlags: " + getFlags() + newline;

        s += "\tKeepAliveTime: " + KeepAliveTime + newline;

        s += "\tStartOnDemand: " + StartOnDemand + newline;

        return s;
    }

	/**
	 * @return the flags, fetching them from the DB on demand using a custom query and conversion to flat string.
	 */
	public synchronized String getFlags() {
		if (Flags == null) {
			Logger logger = AcsLoggerHelper.getInstance().getSharedLogger();
			try {
				Session session = HibernateUtil.getInstance(logger).getSession();
				
				ContainerStartupOptionHelper optionHelper = new ContainerStartupOptionHelper(logger);
				Flags = optionHelper.convertContainerStartupOptions(ContainerStartupOption);
			} catch (Exception ex) { // HibernateException, HibernateUtilException
				logger.log(Level.WARNING, "Failed to convert options for Computer " + Computer, ex);
			}
		}
		return Flags;
	}

	/**
	 * @param flags the flags to set
	 */
	public synchronized void setFlags(String flags) {
		Flags = flags;
		//@TODO: convert flags to the Set this.ContainerStartupOption
	}

	/**
	 * @return the host
	 */
	public String getHost() {
		if (Computer == null)
			return null;
		else
			return Computer.getHostName();
	}

	/**
	 * @param host the host to set
	 *
	public void setHost(String host) {
		if (Computer != null)
			Computer.setHostName(host);
	}*/

	/**
	 * @return the keepAliveTime
	 */
	public Integer getKeepAliveTime() {
		return KeepAliveTime;
	}

	/**
	 * @param keepAliveTime the keepAliveTime to set
	 */
	public void setKeepAliveTime(Integer keepAliveTime) {
		KeepAliveTime = keepAliveTime;
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

	/**
	 * @return the typeModifiers
	 */
	public String getTypeModifiers() {
		return TypeModifiers;
	}

	/**
	 * @param typeModifiers the typeModifiers to set
	 */
	public void setTypeModifiers(String typeModifiers) {
		TypeModifiers = typeModifiers;
	}

	/**
	 * @return the startOnDemand
	 */
	public Boolean isStartOnDemand() {
		return StartOnDemand;
	}

	/**
	 * @param startOnDemand the startOnDemand to set
	 */
	public void setStartOnDemand(Boolean startOnDemand) {
		StartOnDemand = startOnDemand;
	}

}
