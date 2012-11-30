/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2012
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

package acs.benchmark.nc.client;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;

import alma.acs.pubsubtest.config.ContainerSpecT;
import alma.acs.pubsubtest.config.PubSubInfrastructureSpec;

/**
 * Encapsulates an XML-defined {@link PubSubInfrastructureSpec},
 * and in addition offers setting (or replacing) host names where to run containers,
 * which typically cannot be specified statically in the XML if a test
 * should run on different machines.
 * 
 * @author hsommer
 */
public class PubSubScenario
{
	private final Logger logger;
	private final PubSubInfrastructureSpec spec;

	/**
	 * Loads the test spec (castor classes) from an xml file.
	 */
	public PubSubScenario(Logger logger, File xmlFile, boolean validateXml) 
					throws FileNotFoundException, IOException, MarshalException, ValidationException {
		this.logger = logger;
		if (xmlFile.exists() && !xmlFile.isDirectory() && xmlFile.canRead()) {
			Unmarshaller unm = new Unmarshaller(PubSubInfrastructureSpec.class);
			unm.setValidation(validateXml);
			spec = (PubSubInfrastructureSpec) unm.unmarshal(new FileReader(xmlFile));
		}
		else {
			throw new FileNotFoundException(xmlFile.getAbsolutePath());
		}
	}
	
	public PubSubScenario(Logger logger, PubSubInfrastructureSpec spec) {
		this.logger = logger;
		this.spec = spec;
	}
	

	/**
	 * Sets container host names in the given <code>PubSubTestSpec</code>.
	 * @param spec  Pub/sub test spec. Host names for containers will be overwritten by matching data in <code>containerHostMapping</code>.
	 * @param containerHostMapping  Container-host mapping in the form <code>container1=host1:container2=host2</code>
	 */
	public void setHostNames(String containerHostMapping) {
		
		logger.finer("Will process container host mapping '" + containerHostMapping + "'.");
		Map<String, String> containerHostMappingMap = new HashMap<String, String>();
		
		for (String mapping : containerHostMapping.split(":+")) {
			String[] tmp = mapping.split("=");
			if (tmp.length == 2) {
				containerHostMappingMap.put(tmp[0], tmp[1]);
			}
			else {
				logger.info("Failed to process container-host mapping '" + mapping + "'.");
			}
		}
		
		setHostNames(containerHostMappingMap);
	}
	
	
	/**
	 * Sets container host names in the given <code>PubSubTestSpec</code>.
	 * @param spec  Pub/sub test spec. Host names for containers will be overwritten by matching data in <code>containerHostMapping</code>.
	 * @param containerHostMapping  key = container name, value = host name.
	 */
	public void setHostNames(Map<String, String> containerHostMapping) {
		ContainerSpecT[] containerSpecs = spec.getContainer();
		
		for (ContainerSpecT containerSpec : containerSpecs) {
			String hostName = containerHostMapping.get(containerSpec.getContainerName());
			if (hostName != null && !hostName.trim().isEmpty()) {
				if (containerSpec.getHostName() == null || containerSpec.getHostName().isEmpty()) {
					logger.fine("Mapping container '" + containerSpec.getContainerName() + "' to host '" + hostName + "'.");
				}
				else {
					logger.info("Mapping container '" + containerSpec.getContainerName() + "' to host '" + hostName 
							+ "', overwriting old host name '" + containerSpec.getHostName() + "'.");
				}
				containerSpec.setHostName(hostName);
			}
			else {
				logger.fine("No host mapping found for container '" + containerSpec.getContainerName() + "'. Will use localhost.");
				containerSpec.setHostName(null);
			}
		}
	}

	public PubSubInfrastructureSpec getSpec() {
		return spec;
	}
}
