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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import cern.laser.business.LaserObjectNotFoundException;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.ResponsiblePersonDAO;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmImpl;
import cern.laser.business.data.Building;
import cern.laser.business.data.Location;
import cern.laser.business.data.ResponsiblePerson;
import cern.laser.business.data.Source;
import cern.laser.business.data.Status;
import cern.laser.business.data.Triplet;

import com.cosylab.acs.laser.dao.xml.AlarmDefinition;
import com.cosylab.acs.laser.dao.xml.AlarmDefinitionList;
import com.cosylab.acs.laser.dao.xml.Child;
import com.cosylab.acs.laser.dao.xml.LinksToCreate;
import com.cosylab.acs.laser.dao.xml.Parent;
import com.cosylab.acs.laser.dao.xml.ReductionDefinitions;
import com.cosylab.acs.laser.dao.xml.ReductionLink;

class HardcodedBuilding extends Building
{
	private HardcodedBuilding()
	{
		super("B/1", "Site", new Integer(1), "Map");
	}
	
	static final HardcodedBuilding instance = new HardcodedBuilding(); 
}

class HardcodedLocation extends Location
{
	private HardcodedLocation()
	{
		super("L/1", "1", "L/1/1", "1", "1");
		this.setBuilding(HardcodedBuilding.instance);
	}
	
	static final HardcodedLocation instance = new HardcodedLocation();
}

class AlarmRefMatcher
{
	final String family;
	final Pattern member;
	final int minCode, maxCode;
	
	public AlarmRefMatcher(String family, String memberPattern, String codeSpec) throws IllegalArgumentException
	{
		if (family==null || memberPattern==null || codeSpec==null)
			throw new IllegalArgumentException();

		this.family=family;
		this.member=processRef(null, memberPattern);
		
		try {
			int minus=codeSpec.indexOf('-');
			if (codeSpec.lastIndexOf('-')!=minus)
				throw new IllegalArgumentException("Only a single - allowed in code spec");
			if (minus<0) {
				minCode=maxCode=Integer.parseInt(codeSpec);
			} else {
				if (minus==0) {
					minCode=Integer.MIN_VALUE;
					maxCode=Integer.parseInt(codeSpec.substring(1));
				} else
				if (minus==codeSpec.length()-1) {
					minCode=Integer.parseInt(codeSpec.substring(0, codeSpec.length()-1));
					maxCode=Integer.MAX_VALUE;
				} else {
					minCode=Integer.parseInt(codeSpec.substring(0, minus));
					maxCode=Integer.parseInt(codeSpec.substring(minus+1));
				}
			}
		} catch (NumberFormatException e) {
			throw new IllegalArgumentException("Invalid code spec");
		}
	}
	
	static Pattern processRef(String abase, String ref) throws PatternSyntaxException
	{
		Stack stack;
		String[] tmp;
		if (ref.startsWith("/") || abase==null) {
			if (ref.startsWith("/")) {
				tmp=ref.substring(1).split("/");
			} else {
				tmp=ref.split("/");
			}
			stack=new Stack();
		} else {
			tmp=ref.split("/");
			stack=new Stack();
			String[] orig=abase.substring(1).split("/");
			for (int a=0; a<orig.length; a++)
				stack.push(orig[a]);
		}			
		
		for (int a=0; a<tmp.length; a++) {
			String t=tmp[a];
			if (".".equals(t)) {
				// ignore
			} else
			if ("..".equals(t)) {
				if (stack.size()>0) {
					String removed=(String)stack.pop();
					if ("**".equals(removed))
						throw new PatternSyntaxException(".. can't follow **", ref, -1);
				}
			} else {
				stack.push(t);
			}
		}
		
		StringBuffer sb=new StringBuffer();
		sb.append('^');
		int s=stack.size();
		for (int a=0; a<s; a++) {
			if (a>0)
				sb.append("/");
			
			String t=(String)stack.get(a);
			if (t.indexOf("**")>=0) {
				if ("**".equals(t)) {
					sb.append(".*");
				} else {
					throw new PatternSyntaxException("** can't appear as a substring", t, -1);
				}
			} else {
				int l=t.length();
				for (int b=0; b<l; b++) {
					char c=t.charAt(b);
					if (c=='*') {
						sb.append("[^/]*");
					} else {
						DAOUtil.regexEncodeChar(sb, c);
					}
				}
			}
		}
		sb.append('$');
		
		return Pattern.compile(sb.toString());
	}
	
	public boolean isMatch(AlarmImpl a)
	{
		if (a==null)
			throw new IllegalArgumentException();

		// checks in order of speed, so failure happens asap
		Triplet t=a.getTriplet();
		
		int acde=t.getFaultCode().intValue();
		if (acde<minCode || acde>maxCode)
			return false;
		
		if (!t.getFaultFamily().equals(this.family))
			return false;
		
		return member.matcher(t.getFaultMember()).matches();
	}
}

class LinkSpec
{
	final AlarmRefMatcher parent, child;
	final boolean isMultiplicity;
	
	public LinkSpec(AlarmRefMatcher parent, AlarmRefMatcher child, boolean isMultiplicity)
	{
		if (parent==null || child==null)
			throw new IllegalArgumentException();
		
		this.parent=parent;
		this.child=child;
		this.isMultiplicity=isMultiplicity;
	}
	
	public boolean matchParent(AlarmImpl parent)
	{
		return this.parent.isMatch(parent);
	}
	
	public boolean matchChild(AlarmImpl child)
	{
		return this.child.isMatch(child);
	}
	
	public boolean isMultiplicity()
	{
		return isMultiplicity;
	}
}

public class ACSAlarmDAOImpl implements AlarmDAO
{
	static final HardcodedBuilding theBuilding=HardcodedBuilding.instance;
	static final HardcodedLocation theLocation=HardcodedLocation.instance;
	
	ConfigurationAccessor conf;
	Logger logger;
	String surveillanceAlarmId;
	ResponsiblePersonDAO responsiblePersonDAO;
	SourceDAO sourceDAO;
	
	HashMap alarmDefs=new HashMap();
	HashSet loadedMembers=new HashSet();
	
	static String getAlarmDefPath(String alarmId)
	{
		if (alarmId==null)
			throw new IllegalArgumentException("null alarmId");
		
		String[] tmp=alarmId.split("[:]");
		if (tmp.length!=3)
			throw new IllegalStateException("Unrecognized alarm ID: "+alarmId);

		String dev=tmp[1];
		int hash=dev.indexOf('#');
		if (hash>=0)
			dev=dev.substring(0, hash);
		
		if (dev.startsWith("/")) {
			return "/Alarms/AlarmDefinitions"+dev;
		} else {
			return "/Alarms/AlarmDefinitions/"+dev;
		}
	}
	
	static String getAlarmListPath()
	{
		return "/Alarms/List";
	}
	
	static String getAlarmLinksPath(String alarmId)
	{
		return "/Alarms/ReductionDefinitions";
	}
	
	public void loadAlarms()
	{		
		if (conf==null)
			throw new IllegalStateException("Missing dal");
		
		String mama;
		try {
			mama=conf.getConfiguration(getAlarmListPath());
		} catch (Exception e) {
			throw new RuntimeException("Couldn't read alarm list", e);
		}
		
		AlarmDefinitionList adl;
		try {
			adl=(AlarmDefinitionList) AlarmDefinitionList.unmarshal(new StringReader(mama));
		} catch (Exception e1) {
			throw new RuntimeException("Unable to read "+getAlarmListPath(), e1);
		}
		
		HashSet allAlarmIDs=new HashSet();
		HashSet allAlarmDefFiles=new HashSet();
		HashSet allLinkFiles=new HashSet();
		
		AlarmDefinition[] defs=adl.getAlarmDefinition();
		for (int a=0; a<defs.length; a++) {
			String member=defs[a].getFaultMember();
			if (member==null) {
				member="";
			}
			int code=defs[a].getFaultCode();
			String family=defs[a].getFaultFamily();
			if (family==null) {
				family="";
			}
			
			String alarmId=Triplet.toIdentifier(family, member, new Integer(code));
			allAlarmIDs.add(alarmId);
			allAlarmDefFiles.add(getAlarmDefPath(alarmId));
			allLinkFiles.add(getAlarmLinksPath(alarmId));
		}
		
		alarmDefs=new HashMap();
		
		Iterator i=allAlarmDefFiles.iterator();
		while (i.hasNext()) {
			String path=(String)i.next();
			String xml;
			try {
				xml=conf.getConfiguration(path);
			} catch (Exception e) {
				throw new RuntimeException("Failed to read "+path, e);
			}
			AlarmDefinitionList realadl;
			try {
				realadl=(AlarmDefinitionList) AlarmDefinitionList.unmarshal(new StringReader(xml));
			} catch (Exception e) {
				throw new RuntimeException("Failed to read "+path, e);
			}
			
			AlarmDefinition[] realdefs=realadl.getAlarmDefinition();
			for (int a=0; a<realdefs.length; a++) {
				AlarmDefinition ad=realdefs[a];
				AlarmImpl ai=new AlarmImpl();
				
				ai.setMultiplicityChildrenIds(new HashSet());
				ai.setMultiplicityParentIds(new HashSet());
				ai.setNodeChildrenIds(new HashSet());
				ai.setNodeParentIds(new HashSet());
				
				ai.setAction(ad.getAction());
				ai.setTriplet(new Triplet(ad.getFaultFamily(), ad.getFaultMember(), new Integer(ad.getFaultCode())));
				ai.setCategories(new HashSet());
				ai.setCause(ad.getCause());
				ai.setConsequence(ad.getConsequence());
				try {
					ai.setHelpURL(new URL(ad.getHelpUrl()));
				} catch (MalformedURLException e) {
					ai.setHelpURL(null);
				}
				//ai.setDefinition(null);
				ai.setInstant(ad.getInstant()?Boolean.TRUE:Boolean.FALSE);
				ai.setLocation(theLocation);
				ai.setPiquetEmail(ad.getPiquetEmail());
				ai.setPiquetGSM(ad.getPiquetGSM());
				ai.setPriority(new Integer(ad.getPriority()));
				ai.setResponsiblePerson(responsiblePersonDAO.getResponsiblePerson(new Integer(ad.getResponsibleId())));
				ai.setSource(sourceDAO.findSource(ad.getSourceName()));
				
				alarmDefs.put(ai.getAlarmId(), ai);
			}
		}
		
		ArrayList links=new ArrayList();
		
		i=allLinkFiles.iterator();
		while (i.hasNext()) {
			String path=(String)i.next();
			String xml;
			try {
				xml=conf.getConfiguration(path);
			} catch (Exception e) {
				throw new RuntimeException("Couldn't read "+path, e);
			}
			
			ReductionDefinitions rds;
			try {
				rds=(ReductionDefinitions) ReductionDefinitions.unmarshal(new StringReader(xml));
			} catch (Exception e) {
				throw new RuntimeException("Couldn't parse "+path, e);
			}
			
			LinksToCreate ltc=rds.getLinksToCreate();
			
			if (ltc!=null) { 
				ReductionLink[] rls=ltc.getReductionLink();
				for (int a=0; a<rls.length; a++) {
					ReductionLink link=rls[a];
					Parent p=link.getParent();
					Child c=link.getChild();
					if (p==null || c==null)
						throw new RuntimeException("Missing child or parent in a reduction link");
					
					boolean isMulti;
					if ("MULTIPLICITY".equals(link.getType())) {
						isMulti=true;
					} else
					if ("NODE".equals(link.getType())) {
						isMulti=false;
					} else {
						throw new RuntimeException("Unknown reduction-link type: "+link.getType());
					}
					
					if (p.getAlarmDefinition()==null || c.getAlarmDefinition()==null)
						throw new RuntimeException("Missing alarm-definition in child or parent");
					
					links.add(new LinkSpec(toMatcher(p.getAlarmDefinition()), toMatcher(c.getAlarmDefinition()), isMulti));
				}
			}
		}
		
		Collection cc=alarmDefs.values();
		AlarmImpl[] allAlarms=new AlarmImpl[cc.size()];
		cc.toArray(allAlarms);
		
		LinkSpec[] ls=new LinkSpec[links.size()];
		links.toArray(ls);
		
		int num=allAlarms.length;
		int numls=ls.length;
		
		for (int a=0; a<num; a++) {
			AlarmImpl parent=allAlarms[a];
			for (int b=0; b<numls; b++) {
				LinkSpec lsb;
				if ((lsb=ls[b]).matchParent(parent)) {
					AlarmRefMatcher childMatcher=lsb.child;
					boolean isMulti=lsb.isMultiplicity();
					for (int c=0; c<num; c++) {
						if (a==c)
							continue;
						AlarmImpl aic;
						if (childMatcher.isMatch(aic=allAlarms[c])) {
							if (isMulti) {
								parent.addMultiplicityChild(aic);
							} else {
								parent.addNodeChild(aic);
							}
						}
					}
				}
			}
		}
	}
	
	static AlarmRefMatcher toMatcher(AlarmDefinition def)
	{
		String family=def.getFaultFamily();
		if (family==null || family.length()<1)
			throw new IllegalArgumentException("Missing fault-family");
		
		String member=def.getFaultMember();
		if (member==null || member.length()<1)
			member="*";
		
		int code=def.getFaultCode();
		
		return new AlarmRefMatcher(family, member, String.valueOf(code));
	}
	
	private void saveAllIDs()
	{
		if (conf==null) {
			throw new IllegalStateException("null ConfigurationAccessor");
		}

		if (!conf.isWriteable()) {
			throw new RuntimeException("ConfigurationAccessor is not writeable");
		}
		
		StringBuffer result=new StringBuffer();
		result.append("<?xml version=\"1.0\"?>\n");
		result.append("<alarm-definition-list>\n");
		
		Iterator i=alarmDefs.values().iterator();
		while (i.hasNext()) {
			Alarm a=(Alarm)i.next();
			Triplet t=a.getTriplet();
			result.append("\t<alarm-definition fault-family=\"");
			DAOUtil.escapeXMLEntities(result, t.getFaultFamily());
			result.append("\" fault-member=\"");
			DAOUtil.escapeXMLEntities(result, t.getFaultMember());
			result.append("\" fault-code=\"");
			result.append(t.getFaultCode().intValue());
			result.append("\" />\n");
		}
		
		result.append("</alarm-definition-list>\n");
		
		try {
			conf.setConfiguration("/alarms", result.toString());
		} catch (Exception e) {
			throw new RuntimeException("Could not write alarm list", e);
		}
	}
	
	static String memberFromAlarmID(String alarmId)
	{
		int prev=alarmId.indexOf(':');
		int next=alarmId.lastIndexOf(':');
		
		if (!(prev>0 && next>0 && next>prev))
			throw new IllegalStateException();
		
		return alarmId.substring(prev+1, next);
	}

	public Alarm findAlarm(String alarmId)
	{
		Alarm res=getAlarm(alarmId);
		
		if (res==null)
			throw new LaserObjectNotFoundException("Alarm ID "+alarmId+" expected, but not found");

		return res;
	}

	public Alarm getAlarm(String alarmId)
	{
		if (conf==null)
			throw new IllegalStateException("Missing dal");

		// get(id)!=null is not OK - there could be a null value 
		// if (alarmDefs.containsKey(alarmId))
			return (Alarm)alarmDefs.get(alarmId);
		
		//throw new UnsupportedOperationException("Incremental loading not implemented yet");
	}
	
	Building loadBuilding(String buildingID)
	{
		if (buildingID.equals(theBuilding.getBuildingNumber()))
			return theBuilding;
		
		return null;
	}
	
	
	public String[] findAlarmIdsByPriority(Integer priority)
	{
		int p=priority.intValue();
		ArrayList result=null;
		Iterator i=alarmDefs.entrySet().iterator();
		while (i.hasNext()) {
			Map.Entry e=(Map.Entry) i.next();
			String id=e.getKey().toString();
			AlarmImpl ai=(AlarmImpl) e.getValue();
			if (ai.getPriority().intValue()==p) {
				if (result==null)
					result=new ArrayList();
				result.add(id);				
			}
		}
		
		if (result==null) {
			return new String[0];
		} else {
			int s=result.size();
			String[] res=new String[s];
			result.toArray(res);
			return res;
		}
	}

	public String findLaserSurveillanceAlarmId()
	{
		if (getAlarm(surveillanceAlarmId)==null)
			throw new LaserObjectNotFoundException("unable to find laser surveillance alarm");
		
		return surveillanceAlarmId;
	}

	public void deleteAlarm(Alarm alarm)
	{
		String member=alarm.getTriplet().getFaultMember();
		if (alarmDefs.remove(alarm.getTriplet().toIdentifier())!=null)
			saveMemberAlarms(member);
	}

	public void saveMemberAlarms(String member)
	{
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("No writeable configuration accessor available");
		
		StringBuffer sb=new StringBuffer();
		sb.append("<?xml version=\"1.0\"?>\n");
		sb.append("<alarm-definition-list>\n");
		Iterator i=alarmDefs.values().iterator();
		while (i.hasNext()) {
			Alarm a=(Alarm)i.next();
			if (a.getTriplet().getFaultMember().equals(member))
				encodeToXML(sb, (Alarm)i.next());
		}
		sb.append("</alarm-definition-list>\n");
		
		try {
			conf.setConfiguration(member+"/alarms", sb.toString());
		} catch (Exception e) {
			throw new RuntimeException("Failed while writing to "+member+"/alarms", e);
		}
	}
	
	public void saveAlarm(Alarm alarm)
	{
		alarmDefs.put(alarm.getTriplet().toIdentifier(), alarm);
		saveMemberAlarms(alarm.getTriplet().getFaultMember());
	}

	static String encodeToXML(Alarm alarm)
	{
		StringBuffer result=new StringBuffer();
		
		result.append("<?xml version=\"1.0\"?>\n");
		
		encodeToXML(result, alarm);
		
		return result.toString();
	}
	
	static String encodeToXML(StringBuffer result, Alarm alarm)
	{
		result.append("<alarm-definition");
		Triplet t=alarm.getTriplet();
		
		if (t==null || t.getFaultCode()==null || t.getFaultFamily()==null || t.getFaultMember()==null)
			throw new IllegalArgumentException("Incomplete alarm");
		
		DAOUtil.encodeAttr(result, "fault-family", t.getFaultFamily());
		DAOUtil.encodeAttr(result, "fault-member", t.getFaultMember());
		DAOUtil.encodeAttr(result, "fault-code", t.getFaultCode().toString());
		
		result.append(">\n");
		
		{
			String sn=alarm.getSystemName();
			String si=alarm.getIdentifier();
			String pd=alarm.getProblemDescription();
			
			if (sn!=null || si!=null || pd!=null) {
				result.append("\t<visual-fields>\n");
				
				// schema says all or nothing, but still, let's not be too picky
				
				if (sn!=null)
					DAOUtil.encodeElem(result, "system-name", sn, 2);
				if (si!=null)
					DAOUtil.encodeElem(result, "identifier", si, 2);
				if (pd!=null)
					DAOUtil.encodeElem(result, "problem-description", pd, 2);
				
				result.append("\t</visual-fields>\n");
			}
		}
		
		DAOUtil.encodeElemIf(result, "instant", alarm.getInstant(), 1);
		DAOUtil.encodeElemIf(result, "cause", alarm.getCause(), 1);
		DAOUtil.encodeElemIf(result, "action", alarm.getAction(), 1);
		DAOUtil.encodeElemIf(result, "consequence", alarm.getConsequence(), 1);
		DAOUtil.encodeElemIf(result, "priority", alarm.getPriority(), 1);
		
		ResponsiblePerson rp=alarm.getResponsiblePerson();
		if (rp!=null)
			DAOUtil.encodeElemIf(result, "responsible-id", rp.getResponsibleId(), 1);

		DAOUtil.encodeElemIf(result, "piquetGSM", alarm.getPiquetGSM(), 1);
		DAOUtil.encodeElemIf(result, "help-url", alarm.getHelpURL(), 1);
		
		Source s=alarm.getSource();
		if (s!=null)
			DAOUtil.encodeElemIf(result, "source-name", s.getName(), 1);
		
		Location l=alarm.getLocation();
		if (l!=null) {
			result.append("\t<location>\n");
			
			Building b=l.getBuilding();
			if (b!=null)
				DAOUtil.encodeElemIf(result, "building", b.getBuildingNumber(), 2);
			
			DAOUtil.encodeElemIf(result, "floor", l.getFloor(), 2);
			DAOUtil.encodeElemIf(result, "room", l.getRoom(), 2);
			DAOUtil.encodeElemIf(result, "mnemonic", l.getMnemonic(), 2);
			DAOUtil.encodeElemIf(result, "position", l.getPosition(), 2);
			
			result.append("\t</location>\n");
		}
		
		DAOUtil.encodeElemIf(result, "piquetEmail", alarm.getPiquetEmail(), 1);
		result.append("</alarm-definition>\n");
		
		return result.toString();
	}
	
	public void updateAlarm(Alarm alarm)
	{
		alarmDefs.put(alarm.getTriplet().toIdentifier(), alarm);
		saveMemberAlarms(alarm.getTriplet().getFaultMember());
	}

	public void updateStatus(Status status)
	{
		// TODO Auto-generated method stub
	}

	public Collection search(String select_sql)
	{
		throw new UnsupportedOperationException();
	}

	public Collection archiveSearch(String select_sql)
	{
		throw new UnsupportedOperationException();
	}

	public Building findBuilding(String building)
	{
		if (building.equals(theBuilding.getBuildingNumber()))
			return theBuilding;
		
		throw new LaserObjectNotFoundException("Couldn't find building "+building);
	}

	public void setConfAccessor(ConfigurationAccessor conf)
	{
		this.conf = conf;
	}

	public void setSurveillanceAlarmId(String surveillanceAlarmId)
	{
		this.surveillanceAlarmId = surveillanceAlarmId;
	}
	
	public void setResponsiblePersonDAO(ResponsiblePersonDAO responsiblePersonDAO)
	{
		this.responsiblePersonDAO=responsiblePersonDAO;
	}
	
	public void setSourceDAO(SourceDAO sourceDAO)
	{
		this.sourceDAO=sourceDAO;
	}
	
	public String[] getAllAlarmIDs()
	{
		Set keyset=alarmDefs.keySet();
		String[] result=new String[keyset.size()];
		keyset.toArray(result);
		return result;
	}
}
