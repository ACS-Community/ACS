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

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import cern.laser.business.LaserObjectNotFoundException;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.ResponsiblePersonDAO;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.ResponsiblePerson;
import cern.laser.business.data.Source;

import com.cosylab.acs.laser.dao.xml.SourceDefinition;
import com.cosylab.acs.laser.dao.xml.SourceDefinitions;
import com.cosylab.acs.laser.dao.xml.SourcesToCreate;

public class ACSSourceDAOImpl implements SourceDAO
{
	ConfigurationAccessor conf;
	AlarmDAO alarmDAO;
	ResponsiblePersonDAO responsiblePersonDAO;
	
	String laserSourceID;
	
	HashMap sourceDefs=new HashMap();
		
	static String getSourcesPath()
	{
		return "/Alarms/SourceDefinitions";
	}
	
	public ACSSourceDAOImpl ()
	{
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
		SourcesToCreate stc=new SourcesToCreate();
		out.setSourcesToCreate(stc);
		
		Iterator i=sourceDefs.values().iterator();
		while (i.hasNext()) {
			Source s=(Source)i.next();
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
			conf.setConfiguration(getSourcesPath(), sw.toString());
		} catch (Exception e) {
			throw new RuntimeException("Failed to write configuration", e);
		}
	}
	
	public void loadSources()
	{
		if (conf==null)
			throw new IllegalStateException("No configuration accessor");
		String xml;
		try {
			xml=conf.getConfiguration(getSourcesPath());
		} catch (Exception e) {
			throw new RuntimeException("Couldn't load "+getSourcesPath());
		}
		
		SourceDefinitions sds;
		try {
			sds=(SourceDefinitions) SourceDefinitions.unmarshal(new StringReader(xml));
		} catch (Exception e1) {
			throw new RuntimeException("Couldn't parse "+getSourcesPath());
		}
		
		SourcesToCreate stc=sds.getSourcesToCreate();
		if (stc==null)
			throw new RuntimeException("Source definition list must contain sources-to-create");
		
		SourceDefinition[] sdefs=stc.getSourceDefinition();
		if (sdefs==null || sdefs.length<1)
			throw new RuntimeException("no sources found");
		
		for (int a=0; a<sdefs.length; a++) {
			SourceDefinition sd=sdefs[a];
			
			cern.laser.business.definition.data.SourceDefinition sourceDef =
				new cern.laser.business.definition.data.SourceDefinition(sd.getName());
			
			cern.laser.business.data.ResponsiblePerson respP = responsiblePersonDAO.getResponsiblePerson(new Integer(sd.getResponsibleId()));
			cern.laser.business.data.Source sii = new Source(sourceDef,respP);
			sii.setDescription(sd.getDescription());
			sii.setSourceId(sd.getName());
			sii.setConnectionTimeout(new Integer(sd.getConnectionTimeout()));
			
			sourceDefs.put(sii.getSourceId(), sii);
		}
	}
	
	public String[] getAlarms(String sourceId)
	{
		if (alarmDAO==null)
			throw new IllegalStateException("alarmDAO not set");
		
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