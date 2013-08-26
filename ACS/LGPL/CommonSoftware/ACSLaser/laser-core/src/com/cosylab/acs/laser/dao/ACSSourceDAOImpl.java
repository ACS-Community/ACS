/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
 *    All rights reserved
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
package com.cosylab.acs.laser.dao;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.alarmmessage.generated.SourceDefinition;
import alma.alarmsystem.alarmmessage.generated.SourceDefinitionListType;
import alma.alarmsystem.alarmmessage.generated.SourceDefinitions;
import cern.laser.business.LaserObjectNotFoundException;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.ResponsiblePersonDAO;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.Source;

/**
 * The sources are now defined together with the alarms and read by
 * ACSAlarmDAOImpl i.e. object from this class do not need to read
 * again the sources from the CDB.
 * The sources definition are queried to ACSAlarmDAOImpl.
 * 
 * @author acaproni
 *
 */
public class ACSSourceDAOImpl implements SourceDAO
{
	ConfigurationAccessor conf;
	AlarmDAO alarmDAO;
	ResponsiblePersonDAO responsiblePersonDAO;
	
	String laserSourceID;
	
	ConcurrentHashMap<String, Source> sourceDefs;
	
	// The path in the CDB of the definitions of sources
	private static final String SOURCE_PATH="/Alarms/SourceDefinitions";
	
	// The logger
	private Logger logger;
		
	/**
	 * Constructor.
	 * 
	 * The constructor takes the sources as parameter.
	 * After the refactory of the CDB the sources are read by the ACSAlarmDAOImpl because
	 * they are defined together with the alarms.
	 * By passing the sources as parameter, objects from this class avoid to read again the
	 * sources from the CDB.
	 * 
	 * @param log The logger
	 * @param sources The sources
	 */
	public ACSSourceDAOImpl (Logger log, ConcurrentHashMap<String, Source> sources)
	{
		if (log==null) {
			throw new IllegalArgumentException("Invalid null logger");
		}
		if (sources==null) {
			throw new IllegalArgumentException("Invalid null sources in constructor");
		}
		logger =log;
		if (sources.size()<1) {
			logger.log(AcsLogLevel.WARNING,"No alarm sources defined");
		}
		sourceDefs=sources;
	}
	
	public void setAlarmDAO(AlarmDAO alarmDAO)
	{
		this.alarmDAO=alarmDAO;
	}
	
	public void setResponsiblePersonDAO(ResponsiblePersonDAO r)
	{
		responsiblePersonDAO=r;
	}
	
	public Source findSource(String sourceId)
	{
		Source s=getSource(sourceId);
		if (s==null)
			throw new LaserObjectNotFoundException("Couldn't find source "+sourceId);
		
		return s;
	}

	public Source getSource(String sourceId)
	{
		return (Source)sourceDefs.get(sourceId);
	}

	public Source[] findAllSources()
	{
		Collection c=sourceDefs.values();

		Source[] result=new Source[c.size()];
		
		c.toArray(result);
		
		return result;
	}

	public Source findByLaserSource()
	{
		return findSource(laserSourceID);
	}

	public void saveSource(Source source)
	{
		sourceDefs.put(source.getSourceId(), source);
		saveSources();
	}

	public void deleteSource(Source source)
	{
		sourceDefs.remove(source.getSourceId());
		saveSources();
	}

	public void updateSource(Source source)
	{
		sourceDefs.put(source.getSourceId(), source);
		saveSources();
	}

	private void saveSources()
	{
		if (conf==null)
			throw new IllegalStateException("null configuration accessor");
		if (!conf.isWriteable())
			throw new RuntimeException("Configuration accessor not writeable");
		
		SourceDefinitions out=new SourceDefinitions();
		SourceDefinitionListType stc=new SourceDefinitionListType();
		out.setSourcesToCreate(stc);
		
		Iterator<Source> i=sourceDefs.values().iterator();
		while (i.hasNext()) {
			Source s = i.next();
			SourceDefinition sd=new SourceDefinition();
			sd.setConnectionTimeout(s.getConnectionTimeout().intValue());
			sd.setDescription(s.getDescription());
			sd.setName(s.getSourceId());
			sd.setResponsibleId(s.getResponsiblePerson().getResponsibleId().intValue());
			stc.addSourceDefinition(sd);
		}
		
		StringWriter sw=new StringWriter();
		try {
			out.marshal(sw);
			conf.setConfiguration(SOURCE_PATH, sw.toString());
		} catch (Exception e) {
			throw new RuntimeException("Failed to write configuration", e);
		}
	}
	
	public String[] getAlarms(String sourceId)
	{
		if (alarmDAO==null) {
			throw new IllegalStateException("alarmDAO not set");
		}
		
		if (!(alarmDAO instanceof ACSAlarmDAOImpl))
			throw new UnsupportedOperationException();
		
		String[] aids=((ACSAlarmDAOImpl)alarmDAO).getAllAlarmIDs();
		ArrayList result=new ArrayList();
		
		for (int a=0; a<aids.length; a++) {
			Alarm al=alarmDAO.getAlarm(aids[a]);
			if (al.getSource().getSourceId().equals(sourceId))
				result.add(aids[a]);
		}
		
		String[] rids=new String[result.size()];
		result.toArray(rids);
		return rids;
	}

	public void setConfAccessor(ConfigurationAccessor conf)
	{
		this.conf=conf;
	}

	public void setLaserSourceId(String laserSourceID)
	{
		this.laserSourceID=laserSourceID;
	}

	public String[] getAllSourceIDs()
	{
		Set s=sourceDefs.keySet();
		String[] result=new String[s.size()];
		s.toArray(result);
		return result;
	}
}