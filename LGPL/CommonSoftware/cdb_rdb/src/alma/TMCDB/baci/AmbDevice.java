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

import org.w3c.dom.Element;

import com.cosylab.cdb.jdal.hibernate.ExtraDataFeatureUtil;


/**
 * Component w/ amb:Address
 */
@SuppressWarnings("serial")
public class AmbDevice extends ComponentData {

    public static class AmbAddress {
    	// TODO any missing properties?
    	private int NodeNumber;
    	private int BaseAddress;
    	private int ChannelNumber;

 		/**
		 * @return the baseAddress
		 */
		public int getBaseAddress() {
			return BaseAddress;
		}
		/**
		 * @param baseAddress the baseAddress to set
		 */
		public void setBaseAddress(int baseAddress) {
			BaseAddress = baseAddress;
		}
		/**
		 * @return the channelNumber
		 */
		public int getChannelNumber() {
			return ChannelNumber;
		}
		/**
		 * @param channelNumber the channelNumber to set
		 */
		public void setChannelNumber(int channelNumber) {
			ChannelNumber = channelNumber;
		}
		/**
		 * @return the nodeNumber
		 */
		public int getNodeNumber() {
			return NodeNumber;
		}
		/**
		 * @param nodeNumber the nodeNumber to set
		 */
		public void setNodeNumber(int nodeNumber) {
			NodeNumber = nodeNumber;
		}
    	
    }

    public static class EthernetAddress {
    	private String hostname;
    	private int port;
    	private String macAddress;
    	private int retries;
    	private double timeoutRxTx;
    	private int lingerTime;
    	
		public String getHostname() {
			return hostname;
		}
		
		public void setHostname(String hostname) {
			this.hostname = hostname;
		}
		
		public int getPort() {
			return port;
		}
		
		public void setPort(int port) {
			this.port = port;
		}
		
		public String getMacAddress() {
			return macAddress;
		}
		
		public void setMacAddress(String macAddress) {
			this.macAddress = macAddress;
		}
		
		public int getRetries() {
			return retries;
		}
		
		public void setRetries(int retries) {
			this.retries = retries;
		}
		
		public double getTimeoutRxTx() {
			return timeoutRxTx;
		}
		
		public void setTimeoutRxTx(double timeoutRxTx) {
			this.timeoutRxTx = timeoutRxTx;
		}
		
		public int getLingerTime() {
			return lingerTime;
		}
		
		public void setLingerTime(int lingerTime) {
			this.lingerTime = lingerTime;
		}
    }
    
    private AmbAddress Address;
    
    private EthernetAddress EthernetConfig;
    
    private Element ControlCdbExtraData;
    
    /**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public AmbDevice () {
    }

	/**
	 * @return the address
	 */
	public AmbAddress getAddress() {
		return Address;
	}

	/**
	 * @param address the address to set
	 */
	public void setAddress(AmbAddress address) {
		Address = address;
	}

	public EthernetAddress getEthernetConfig() {
		return EthernetConfig;
	}

	public void setEthernetConfig(EthernetAddress ethernetConfig) {
		EthernetConfig = ethernetConfig;
	}
	
	public void setControlCdbExtraData(String xmldoc) {
		try {
			ControlCdbExtraData = ExtraDataFeatureUtil.getExtraDataMap(xmldoc);
		} catch (Throwable th) {
			th.printStackTrace();
		}
	}

	public Element getControlCdbExtraData() {
		return ControlCdbExtraData;
	}
}