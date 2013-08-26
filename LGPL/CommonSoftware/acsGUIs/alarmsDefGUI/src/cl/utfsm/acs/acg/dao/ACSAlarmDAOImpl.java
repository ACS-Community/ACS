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
package cl.utfsm.acs.acg.dao;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import alma.acs.alarmsystem.generated.Categories;
import alma.acs.alarmsystem.generated.Contact;
import alma.acs.alarmsystem.generated.FaultCode;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.acs.alarmsystem.generated.FaultMember;
import alma.acs.alarmsystem.generated.FaultMemberDefault;
import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.alarmmessage.generated.ReductionLinkDefinitionListType;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import cern.laser.business.LaserObjectNotFoundException;
import cern.laser.business.dao.ResponsiblePersonDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmImpl;
import cern.laser.business.data.Building;
import cern.laser.business.data.Category;
import cern.laser.business.data.Location;
import cern.laser.business.data.ResponsiblePerson;
import cern.laser.business.data.Source;
import cern.laser.business.data.Status;
import cern.laser.business.data.Triplet;
import cern.laser.business.definition.data.SourceDefinition;

import com.cosylab.acs.laser.dao.DAOUtil;
import com.cosylab.acs.laser.dao.xml.AlarmDefinition;
import com.cosylab.acs.laser.dao.xml.Child;
import com.cosylab.acs.laser.dao.xml.LinksToCreate;
import com.cosylab.acs.laser.dao.xml.Parent;
import com.cosylab.acs.laser.dao.xml.ReductionDefinitions;
import com.cosylab.acs.laser.dao.xml.ReductionLink;
import com.cosylab.acs.laser.dao.xml.Threshold;
import com.cosylab.acs.laser.dao.xml.Thresholds;

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

/**
 * Read alarms from the CDB.
 * 
 * CDB contains one file per each possible FF and one entry per each FC and FM.
 * It is possible to specify a default member to be used when the administrator
 * did not specify the member.
 * 
 * The alarms are stored in an HashMap having the triplet as key.
 * The default member has a triplet with a "*" replacing its name.
 * 
 *  The sources are defined together with an alarm definition so they are read here
 *  and requested by the ACSSourceDAOImpl at startup (instead of being read again from CDB).
 *  
 *  The initialization of the alarms is not completely done by loadAlarms because not all the info
 *  are available at this time. In fact the categories are assigned to alarms by ACSCategoryDAOImpl
 *  
 *  @see ACSSourceDAOImpl
 *  @see ACSCategoryDAOImpl
 * 
 * @author acaproni
 *
 */
public class ACSAlarmDAOImpl extends com.cosylab.acs.laser.dao.ACSAlarmDAOImpl
{
	static final HardcodedBuilding theBuilding=HardcodedBuilding.instance;
	static final HardcodedLocation theLocation=HardcodedLocation.instance;
	
	// The path where alarm definitions are
	private static final String ALARM_DEFINITION_PATH = "/Alarms/AlarmDefinitions";
	
	// The path in the CDB where reduction rules are defined
	private static final String REDUCTION_DEFINITION_PATH = "/Alarms/Administrative/ReductionDefinitions";
	
	// The type of the alarm definition in the XML
	private static final String XML_DOCUMENT_TYPE="AlarmDefinitions";
	
	// The FM for the default member
	private static final String DEFAULT_FM = "*";
	
	// The ACS logger
	Logger logger;
	
	ConfigurationAccessor conf;
	String surveillanceAlarmId;
	ResponsiblePersonDAO responsiblePersonDAO;
	
	/** The alarms read out the CDB
	 *  The CDB contains fault families.
	 *  The alarms are generated from the fault families.
	 */
	private HashMap<String,Alarm> alarmDefs=new HashMap<String,Alarm>();
	
	/**
	 * Source are defined together with alarms
	 * This HashMap contains all the sources read from CDB
	 */
	private ConcurrentHashMap<String, Source> srcDefs = new ConcurrentHashMap<String, Source>();
	
	/**
	 * Constructor 
	 * 
	 * @param log The log (not null)
	 * @param 
	 */
	public ACSAlarmDAOImpl(Logger log) {
		super(log);
		if (log==null) {
			throw new IllegalArgumentException("Invalid null logger");
		}
		logger =log;
	}
	
	/**
	 * Load alarms from CDB.
	 * 
	 * Read the alarms by the alarm definitions file of the CDB.
	 * The initialization of the alarms is not complete at this stage.
	 * In fact the categories are assigned to alarms when reading the
	 * categories from the CDB by ACSCategoryDAOImpl
	 * 
	 *  
	 * @throws Exception In case of error while parsing XML definition files
	 * 
	 * @see ACSCategoryDAOImpl
	 */
	public List<FaultFamily> loadAlarms() throws Exception{
		if (conf==null) {
			throw new IllegalStateException("Missing dal");
		}
		String xml;
		try {
			xml=conf.getConfiguration(ALARM_DEFINITION_PATH);
		} catch (Throwable t) {
			return new ArrayList<FaultFamily>();
			//throw new RuntimeException("Couldn't read alarm XML", t);
		}
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder;
		try {
			builder= factory.newDocumentBuilder();
		} catch (Exception e) {
			throw new Exception("Error building the DocumentBuilder from the DocumentBuilderFactory",e);
		}
		StringReader stringReader = new StringReader(xml);
		InputSource inputSource = new InputSource(stringReader);
		Document doc;
		try {
			doc= builder.parse(inputSource);
			if (doc==null) {
				throw new Exception("The builder returned a null Document after parsing");
			}
		} catch (Exception e) {
			throw new Exception("Error parsing XML: "+xml,e);
		}
		NodeList docChilds = doc.getChildNodes();
		if (docChilds==null || docChilds.getLength()!=1) {
			throw new Exception("Malformed xml: only one node (AlarmDefinitions) expected");
		}
		Node alarmDefNode = docChilds.item(0);
		if (alarmDefNode==null || alarmDefNode.getNodeName()!=XML_DOCUMENT_TYPE) {
			throw new Exception("Malformed xml: "+XML_DOCUMENT_TYPE+" not found");
		}
		NodeList alarmDefsNodes = alarmDefNode.getChildNodes();
		Unmarshaller FF_unmarshaller = new Unmarshaller(FaultFamily.class);
		FF_unmarshaller.setValidation(false);
		FF_unmarshaller.setWhitespacePreserve(true);
		Vector<FaultFamily> cdbFamilies= new Vector<FaultFamily>();
		for (int t=0; t<alarmDefsNodes.getLength(); t++) {
			Node alarmDef = alarmDefsNodes.item(t);
			FaultFamily family;
			try {
				family = (FaultFamily) FF_unmarshaller.unmarshal(alarmDef);
				if (family==null) {
					throw new Exception("Error unmarshalling a family node");
				}
				cdbFamilies.add(family);
			} catch (Exception e) {
				throw new Exception("Error parsing "+alarmDef.getNodeName(),e);
			}
			family.validate();
		}
		alarmDefs.clear();
		srcDefs.clear();
		generateAlarmsMap(cdbFamilies);
		loadReductionRules();
		return cdbFamilies;
	}
	
	/**
	 * Generate the alarms from the definition of the fault families.
	 * The alarms will be added into the HashMap with their triplet as key.
	 * The default item has FM="*".
	 * 
	 * The sources read from the families are also added to the HashMap of the sources
	 *  
	 * @param families The FF read from the CDB
	 */
	public void generateAlarmsMap(Vector<FaultFamily> families) {
		if (families==null) {
			throw new IllegalArgumentException("Invalid null vector of FFs");
		}
		for (FaultFamily family: families) {
			String FF= family.getName();
			String helpUrl = family.getHelpUrl();
			String source = family.getAlarmSource();
			Contact contactPerson = family.getContact();
			FaultMember[] FMs = family.getFaultMember();
			FaultMemberDefault defaultFM = family.getFaultMemberDefault();
			FaultCode[] FCs = family.getFaultCode();
			// Check the FCs
			//
			// There should be at least one FC in the CDB
			if (FCs==null || FCs.length==0) {
				logger.log(AcsLogLevel.WARNING,"No FC defined for family "+family.getName());
				continue;
			}
			// Check the FM
			//
			// There should be at least one FM or a default FM defined in the CDB
			if (defaultFM==null && (FMs==null || FMs.length==0)) {
				logger.log(AcsLogLevel.WARNING,"No FM defined for family "+family.getName());
				continue;
			}
			
			// Iterate over the FCs
			for (FaultCode code: FCs) {
				int FC=code.getValue();
				int priority = code.getPriority();
				String action = code.getAction();
				String cause = code.getCause();
				String consequence = code.getConsequence();
				String problemDesc = code.getProblemDescription();
				boolean instant = code.getInstant();
				// Iterate over all the FMs
				for (FaultMember member: FMs) {
					alma.acs.alarmsystem.generated.Location loc = member.getLocation();
					if (loc==null) {
						loc = new alma.acs.alarmsystem.generated.Location();
					}
					if (loc.getBuilding()==null) {
						loc.setBuilding("");
					}
					if (loc.getFloor()==null) {
						loc.setFloor("");
					}
					if (loc.getMnemonic()==null) {
						loc.setMnemonic("");
					}
					if (loc.getPosition()==null) {
						loc.setPosition("");
					}
					if (loc.getRoom()==null) {
						loc.setRoom("");
					}
				
					String FM = member.getName();
					if (FM.equals(DEFAULT_FM)) {
						logger.log(AcsLogLevel.ERROR,"In the CDB, FM="+DEFAULT_FM+" in family "+FF+" is not allowed");
					}
					AlarmImpl alarm = new AlarmImpl(); 
					
					alarm.setMultiplicityChildrenIds(new HashSet());
					alarm.setMultiplicityParentIds(new HashSet());
					alarm.setNodeChildrenIds(new HashSet());
					alarm.setNodeParentIds(new HashSet());
					
					alarm.setAction(action);
					alarm.setTriplet(new Triplet(FF, FM, FC));
					alarm.setCategories(new HashSet<Category>());
					alarm.setCause(cause);
					alarm.setConsequence(consequence);
					alarm.setProblemDescription(problemDesc);
					try {
						alarm.setHelpURL(new URL(helpUrl));
					} catch (MalformedURLException e) {
						alarm.setHelpURL(null);
					}
					
					alarm.setInstant(instant);
					Location location = new Location("0",loc.getFloor(),loc.getMnemonic(),loc.getPosition(),loc.getRoom());
					alarm.setLocation(location);
					if (contactPerson.getEmail()!=null) {
						alarm.setPiquetEmail(contactPerson.getEmail());
					} else {
						alarm.setPiquetEmail("");
					}
					if (contactPerson.getGsm()!=null) {
						alarm.setPiquetGSM(contactPerson.getGsm());
					} else {
						alarm.setPiquetGSM("");
					}
					alarm.setPriority(priority);
					ResponsiblePerson responsible = new ResponsiblePerson(0,contactPerson.getName(),"",contactPerson.getEmail(),contactPerson.getGsm(),"");
					alarm.setResponsiblePerson(responsible);
					SourceDefinition srcDef = new SourceDefinition(source,"SOURCE","",15,1);
					Source src = new Source(srcDef,responsible);
					alarm.setSource(src);
					alarm.setIdentifier(alarm.getTriplet().toIdentifier());
					alarmDefs.put(alarm.getAlarmId(), alarm);
					if (!srcDefs.containsKey(source)) {
						srcDefs.put(src.getSourceId(),src);
						logger.log(AcsLogLevel.DEBUG,"Source "+src.getName()+" (id="+src.getSourceId()+") added");
					}
					
					logger.log(AcsLogLevel.DEBUG,"Alarm added "+alarm.getAlarmId());
				}
				// Add the default
				if (defaultFM!=null) {
					alma.acs.alarmsystem.generated.Location loc = defaultFM.getLocation();
					if (loc==null) {
						loc = new alma.acs.alarmsystem.generated.Location();
					}
					if (loc.getBuilding()==null) {
						loc.setBuilding("");
					}
					if (loc.getFloor()==null) {
						loc.setFloor("");
					}
					if (loc.getMnemonic()==null) {
						loc.setMnemonic("");
					}
					if (loc.getPosition()==null) {
						loc.setPosition("");
					}
					if (loc.getRoom()==null) {
						loc.setRoom("");
					}
					AlarmImpl defaultAlarm = new AlarmImpl(); 
					
					defaultAlarm.setMultiplicityChildrenIds(new HashSet());
					defaultAlarm.setMultiplicityParentIds(new HashSet());
					defaultAlarm.setNodeChildrenIds(new HashSet());
					defaultAlarm.setNodeParentIds(new HashSet());
					
					defaultAlarm.setAction(action);
					defaultAlarm.setCategories(new HashSet<Category>());
					defaultAlarm.setCause(cause);
					defaultAlarm.setConsequence(consequence);
					defaultAlarm.setProblemDescription(problemDesc);
					try {
						defaultAlarm.setHelpURL(new URL(helpUrl));
					} catch (MalformedURLException e) {
						defaultAlarm.setHelpURL(null);
					}
					
					defaultAlarm.setInstant(instant);
					Location location = new Location("0",loc.getFloor(),loc.getMnemonic(),loc.getPosition(),loc.getRoom());
					defaultAlarm.setLocation(location);
					defaultAlarm.setPiquetEmail(contactPerson.getEmail());
					defaultAlarm.setPiquetGSM(contactPerson.getGsm());
					defaultAlarm.setPriority(priority);
					ResponsiblePerson responsible = new ResponsiblePerson(0,contactPerson.getName(),"",contactPerson.getEmail(),contactPerson.getGsm(),"");
					defaultAlarm.setResponsiblePerson(responsible);
					SourceDefinition srcDef = new SourceDefinition(source,"SOURCE","",15,1);
					Source src = new Source(srcDef,responsible);
					defaultAlarm.setSource(src);
					defaultAlarm.setIdentifier(defaultAlarm.getTriplet().toIdentifier());
					Triplet triplet = new Triplet(FF,DEFAULT_FM,FC);
					defaultAlarm.setTriplet(triplet);
					defaultAlarm.setIdentifier(triplet.toIdentifier());
					alarmDefs.put(defaultAlarm.getAlarmId(), defaultAlarm);
					if (!srcDefs.containsKey(source)) {
						srcDefs.put(src.getSourceId(),src);
						logger.log(AcsLogLevel.DEBUG,"Source "+src.getName()+" (id="+src.getSourceId()+") added");
					}
					logger.log(AcsLogLevel.DEBUG,"Default alarm added "+defaultAlarm.getAlarmId());
				}
			}
		}
	}
	
	/**
	 * Load the reduction rules from the CDB.
	 * 
	 * Read the reduction rules from the CDB and set up the alarms accordingly
	 */
	private void loadReductionRules()
	{		
		if (conf==null) {
			throw new IllegalStateException("Missing dal");
		}
		
		// The reduction rules (<parent, child>)
		ArrayList<LinkSpec> reductionRules=new ArrayList<LinkSpec>();
		
		String xml;
		try {
			xml=conf.getConfiguration(REDUCTION_DEFINITION_PATH);
		} catch (CDBRecordDoesNotExistEx cdbEx) {
			// No reduction rules defined in CDB
			logger.log(AcsLogLevel.WARNING,"No reduction rules defined in CDB");
			return;
			
		} catch (Exception e) {
			throw new RuntimeException("Couldn't read "+REDUCTION_DEFINITION_PATH, e);
		}
		ReductionDefinitions rds;
		try {
			rds=(ReductionDefinitions) ReductionDefinitions.unmarshal(new StringReader(xml));
		} catch (Exception e) {
			throw new RuntimeException("Couldn't parse "+REDUCTION_DEFINITION_PATH, e);
		}
		
		// Read the links to create from the CDB
		LinksToCreate ltc=rds.getLinksToCreate();
		//	Read the thresholds from the CDB
		Thresholds thresholds = rds.getThresholds();
		if (ltc!=null) { 
			ReductionLink[] rls=ltc.getReductionLink();
			for (ReductionLink link: rls) {
				Parent p=link.getParent();
				Child c=link.getChild();
				if (p==null || c==null) {
					throw new RuntimeException("Missing child or parent in a reduction link");
				}
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
				
				LinkSpec newRule =new LinkSpec(toMatcher(p.getAlarmDefinition()), toMatcher(c.getAlarmDefinition()), isMulti);
				reductionRules.add(newRule);
				StringBuffer buf = new StringBuffer();
				buf.replace(0, buf.length(), "");
				if (newRule.isMultiplicity) {
					buf.append("Found MULTIPLICITY RR: parent <");
				} else {
					buf.append("Found NODE RR: parent=<");
				}
				buf.append(newRule.parent.family+", "+newRule.parent.member+", "+newRule.parent.minCode+"-"+newRule.parent.minCode+">");
				buf.append(" child=<"+newRule.child.family+", "+newRule.child.member+", "+newRule.child.minCode+"-"+newRule.child.minCode+">");
				logger.log(AcsLogLevel.DEBUG,buf.toString());
			}
		}
		logger.log(AcsLogLevel.DEBUG,reductionRules.size()+" reduction rules read from CDB");
		
		Collection<Alarm> cc=alarmDefs.values();
		AlarmImpl[] allAlarms=new AlarmImpl[cc.size()];
		cc.toArray(allAlarms);
		
		LinkSpec[] ls=new LinkSpec[reductionRules.size()];
		reductionRules.toArray(ls);
		
		int num=allAlarms.length;
		int numls=ls.length;
		
		for (int a=0; a<num; a++) {
			AlarmImpl parent=allAlarms[a];
			for (LinkSpec lsb: ls) {
				if (lsb.matchParent(parent)) {
					AlarmRefMatcher childMatcher=lsb.child;
					boolean isMulti=lsb.isMultiplicity();
					for (int c=0; c<num; c++) {
						if (a==c) {
							continue;
						}
						AlarmImpl aic=allAlarms[c];
						if (childMatcher.isMatch(aic)) {
							if (isMulti) {
								parent.addMultiplicityChild(aic);
								logger.log(AcsLogLevel.DEBUG,"Added MULTI RR node child "+aic.getAlarmId()+" to "+parent.getAlarmId());
							} else {
								parent.addNodeChild(aic);
								logger.log(AcsLogLevel.DEBUG,"Added NODE RR node child "+aic.getAlarmId()+" to "+parent.getAlarmId());
							}
						} 
					}
				}  
			}
		}
		
		if (thresholds!=null && thresholds.getThresholdCount()>0) {
			Threshold[] ta = thresholds.getThreshold();
			for (AlarmImpl alarm: allAlarms) {
				String alarmFF = alarm.getTriplet().getFaultFamily();
				String alarmFM = alarm.getTriplet().getFaultMember();
				Integer alarmFC= alarm.getTriplet().getFaultCode();
				for (Threshold threshold: ta) {
					String thresholdFF=threshold.getAlarmDefinition().getFaultFamily();
					String thresholdFM=threshold.getAlarmDefinition().getFaultMember();
					int thresholdFC=threshold.getAlarmDefinition().getFaultCode();
					int thresholdVal = threshold.getValue();
					if (alarmFF.equals(thresholdFF) && alarmFM.equals(thresholdFM) && alarmFC==thresholdFC) {
						alarm.setMultiplicityThreshold(thresholdVal);
						logger.log(AcsLogLevel.DEBUG,"Threshold = "+thresholdVal+" set in alarm "+alarm.getAlarmId());
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
	
	/*
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
	}*/
	
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
		
		if (res==null) {
			throw new LaserObjectNotFoundException("Alarm ID "+alarmId+" expected, but not found");
		}

		return res;
	}

	/**
	 * Get an alarm from the cache.
	 * 
	 * Get an alarm from the cache. If the alarm with the given triplet is not in the cache then it
	 * looks for a default alarm before returning null.
	 * 
	 * If a default alarm is found, then a new alarm is created by cloning the default alarm.
	 * The triplet of this new alarm is set to be equal to the passed alarm ID.
	 * 
	 * @param alarmId The ID of the alarm
	 * @return An alarm with the given ID if it exists in the CDB
	 * 	       If an alarm with the give ID does not exist but a matching default alarm
	 *         is defined then it return a clone of the default alarm
	 *         null If the alarm is not defined in the CDB and a default alarm does not exist
	 *         
	 * 
	 */
	public Alarm getAlarm(String alarmId)
	{
		if (conf==null) {
			throw new IllegalStateException("Missing dal");
		}
		if (alarmId==null) {
			throw new IllegalArgumentException("Invalid null alarm ID");
		}
		
		AlarmImpl alarm =(AlarmImpl)alarmDefs.get(alarmId);
		if (alarm==null) {
			// The alarm is not in the HashMap
			// 
			// Does it exist a default alarm?
			String[] tripletItems = alarmId.split(":");
			if (tripletItems.length!=3) {
				logger.log(AcsLogLevel.ERROR,"Malformed alarm ID: "+alarmId);
				return null;
			}
			// Build the default alarm ID by replacing the FM with DEFAULT_FM
			String defaultTripletID=tripletItems[0]+":"+DEFAULT_FM+":"+tripletItems[2];
			logger.log(AcsLogLevel.DEBUG,alarmId+" not found: looking for default alarm "+defaultTripletID);
			AlarmImpl defaultalarm=(AlarmImpl)alarmDefs.get(defaultTripletID);
			if (defaultalarm==null) {
				// No available default alarm for this triplet
				logger.log(AcsLogLevel.WARNING,"No default alarm found for "+alarmId);
				return null;
			}
			logger.log(AcsLogLevel.DEBUG,"Default alarm found for "+alarmId);
			alarm=(AlarmImpl)defaultalarm.clone();
			Triplet alarmTriplet = new Triplet(tripletItems[0],tripletItems[1],Integer.parseInt(tripletItems[2]));
			alarm.setTriplet(alarmTriplet);
		}
		return alarm;
		
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
		return;
	}
	
	public void saveAlarm(Alarm alarm)
	{
		alarmDefs.put(alarm.getTriplet().toIdentifier(), alarm);
		saveMemberAlarms(alarm.getTriplet().getFaultMember());
	}

	/*
	static String encodeToXML(Alarm alarm)
	{
		StringBuffer result=new StringBuffer();
		
		result.append("<?xml version=\"1.0\"?>\n");
		
		encodeToXML(result, alarm);
		
		return result.toString();
	}*/
	
	/*
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
	}*/
	
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
	
	public String[] getAllAlarmIDs()
	{
		Set keyset=alarmDefs.keySet();
		String[] result=new String[keyset.size()];
		keyset.toArray(result);
		return result;
	}
	
	public void addFaultFamily(FaultFamily ff){
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("no writable configuration accessor");
		if(ff == null)
			throw new IllegalArgumentException("Null FaultFamily argument");
		StringWriter FFWriter = new StringWriter();
		Marshaller FF_marshaller;
		try {
			FF_marshaller = new Marshaller(FFWriter);
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		FF_marshaller.setValidation(false);
		try {
			FF_marshaller.marshal(ff);
		} catch (MarshalException e) {
			e.printStackTrace();
			return;
		} catch (ValidationException e) {
			e.printStackTrace();
			return;
		}
		String path = ALARM_DEFINITION_PATH + "/" + ff.getName();
		try {
			conf.addConfiguration(path, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
		} catch (Exception e) {
			throw new IllegalStateException("FaultFamily already exists");
		}
		Vector<FaultFamily> ffs = new Vector<FaultFamily>();
		ffs.add(ff);
		generateAlarmsMap(ffs);
	}
	
	public void updateFaultFamily(FaultFamily ff){
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("no writable configuration accessor");
		if(ff == null)
			throw new IllegalArgumentException("Null FaultFamily argument");
		StringWriter FFWriter = new StringWriter();
		Marshaller FF_marshaller;
		try {
			FF_marshaller = new Marshaller(FFWriter);
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		FF_marshaller.setValidation(false);
		try {
			FF_marshaller.marshal(ff);
		} catch (MarshalException e) {
			e.printStackTrace();
			return;
		} catch (ValidationException e) {
			e.printStackTrace();
			return;
		}
		String path = ALARM_DEFINITION_PATH + "/" + ff.getName();
		try {
			try{
				conf.getConfiguration(path);
				conf.deleteConfiguration(path);
			}catch(CDBRecordDoesNotExistEx e){
				//Record does not exist, so we skip removing it.
				throw new IllegalStateException("FaultFamily doesn't exist");
			}
			conf.addConfiguration(path, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
		} catch (IllegalStateException e){
			throw e;
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}
		Vector<FaultFamily> ffs = new Vector<FaultFamily>();
		ffs.add(ff);
		removeAlarmsMap(ffs);
		generateAlarmsMap(ffs);
	}
	
	public void removeFaultFamily(FaultFamily ff){
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("no writable configuration accessor");
		if(ff == null)
			throw new IllegalArgumentException("Null FaultFamily argument");
		try{
			conf.deleteConfiguration(ALARM_DEFINITION_PATH + "/" + ff.getName());
		}catch(CDBRecordDoesNotExistEx e){
			throw new IllegalStateException("FaultFamily doesn't exist");
		}catch(org.omg.CORBA.UNKNOWN e){ //This shouldn't be this way
			throw new IllegalStateException("FaultFamily doesn't exist");
		}catch(Exception e){
			e.printStackTrace();
		}
		Vector<FaultFamily> ffs = new Vector<FaultFamily>();
		ffs.add(ff);
		removeAlarmsMap(ffs);
	}
	
	public void removeAlarmsMap(Vector<FaultFamily> families) {
		if (families==null) {
			throw new IllegalArgumentException("Invalid null vector of FFs");
		}
		String[] als = getAllAlarmIDs();
		for(FaultFamily family: families){
			for(String al: als){
				if(al.startsWith(family.getName().concat(":"))){
					alarmDefs.remove(al);
				}
			}
		}
	}

	public alma.alarmsystem.alarmmessage.generated.ReductionDefinitions getReductionRules() {
		if (conf==null)
			throw new IllegalStateException("null configuration accessor");
		alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds;
		String xml;
		try {
			xml = conf.getConfiguration(REDUCTION_DEFINITION_PATH);
		} catch (CDBRecordDoesNotExistEx e) {
			rds = new alma.alarmsystem.alarmmessage.generated.ReductionDefinitions();
			ReductionLinkDefinitionListType rld = new ReductionLinkDefinitionListType();
			rld.setReductionLink(new alma.alarmsystem.alarmmessage.generated.ReductionLinkType[0]);
			rds.setLinksToCreate(rld);
			alma.alarmsystem.alarmmessage.generated.Thresholds ths = new alma.alarmsystem.alarmmessage.generated.Thresholds();
			ths.setThreshold(new alma.alarmsystem.alarmmessage.generated.Threshold[0]);
			rds.setThresholds(ths);
			return rds;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
		StringReader FFReader = new StringReader(xml);
		Unmarshaller FF_unmarshaller = new Unmarshaller(alma.alarmsystem.alarmmessage.generated.ReductionDefinitions.class);
		FF_unmarshaller.setValidation(false);
		try {
			rds = (alma.alarmsystem.alarmmessage.generated.ReductionDefinitions)FF_unmarshaller.unmarshal(FFReader);
		} catch (MarshalException e) {
			e.printStackTrace();
			return null;
		} catch (ValidationException e) {
			e.printStackTrace();
			return null;
		}
		try {
			rds.validate();
		} catch (ValidationException e) {
			e.printStackTrace();
		}
		return rds;
	}
	
	public void flushReductionRules(alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds) {
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("no writable configuration accessor");
		if(rds == null)
			throw new IllegalArgumentException("Null Reduction Definitions argument");
		StringWriter FFWriter = new StringWriter();
		Marshaller FF_marshaller;
		try {
			FF_marshaller = new Marshaller(FFWriter);
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		FF_marshaller.setValidation(false);
		try {
			FF_marshaller.marshal(rds);
		} catch (MarshalException e) {
			e.printStackTrace();
			return;
		} catch (ValidationException e) {
			e.printStackTrace();
			return;
		}
		try {
			conf.deleteConfiguration(REDUCTION_DEFINITION_PATH);
			conf.addConfiguration(REDUCTION_DEFINITION_PATH, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
			//System.err.println(FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
		} catch (org.omg.CORBA.UNKNOWN e) {
			try {
				conf.addConfiguration(REDUCTION_DEFINITION_PATH, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		} catch (Exception e) {
			e.printStackTrace();
			throw new IllegalStateException("Reduction Rules already exists");
		}
	}
	
	public void addReductionRule(alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds, alma.alarmsystem.alarmmessage.generated.ReductionLinkType rl){
		if(rl == null)
			throw new IllegalArgumentException("Null Reduction Link argument");
		//alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds = getReductionRules();
		//Store changes in the CDB.
		alma.alarmsystem.alarmmessage.generated.ReductionLinkType[] tmp = rds.getLinksToCreate().getReductionLink();
		for (int i = 0; i < tmp.length; i++) {
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p1 = tmp[i].getParent().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition c1 = tmp[i].getChild().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p2 = rl.getParent().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition c2 = rl.getChild().getAlarmDefinition();
			String n1 = new String(p1.getFaultFamily()+":"+p1.getFaultMember()+":"+p1.getFaultCode());
			String n2 = new String(c1.getFaultFamily()+":"+c1.getFaultMember()+":"+c1.getFaultCode());
			String n3 = new String(p2.getFaultFamily()+":"+p2.getFaultMember()+":"+p2.getFaultCode());
			String n4 = new String(c2.getFaultFamily()+":"+c2.getFaultMember()+":"+c2.getFaultCode());
			if(n1.compareTo(n3) == 0 && n2.compareTo(n4) == 0 && tmp[i].getType().compareTo(rl.getType()) == 0)
				throw new IllegalStateException("Reduction Rule already exist");
		}
		rds.getLinksToCreate().addReductionLink(rl);
		//Reflect the changes into the AlarmDAO
		alma.alarmsystem.alarmmessage.generated.AlarmDefinition in = rl.getParent().getAlarmDefinition(); 
		Alarm p = getAlarm(in.getFaultFamily()+":"+in.getFaultMember()+":"+in.getFaultCode());
		in = rl.getChild().getAlarmDefinition();
		Alarm c =  getAlarm(in.getFaultFamily()+":"+in.getFaultMember()+":"+in.getFaultCode());
		if(rl.getType().compareTo("NODE") == 0)
			p.addNodeChild(c);
		else if (rl.getType().compareTo("MULTIPLICITY") == 0)
			p.addMultiplicityChild(c);
		//Store new values in the CDB.
		//flushReductionRules(rds);
	}
	
	public void updateReductionRule(alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds, alma.alarmsystem.alarmmessage.generated.ReductionLinkType rl) {
		if(rl == null)
			throw new IllegalArgumentException("Null Reduction Link argument");
		//alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds = getReductionRules();
		//Store changes in the CDB.
		boolean removed = false;
		alma.alarmsystem.alarmmessage.generated.ReductionLinkType[] tmp = rds.getLinksToCreate().getReductionLink();
		for (int i = 0; i < tmp.length; i++) {
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p1 = tmp[i].getParent().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition c1 = tmp[i].getChild().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p2 = rl.getParent().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition c2 = rl.getChild().getAlarmDefinition();
			String n1 = new String(p1.getFaultFamily()+":"+p1.getFaultMember()+":"+p1.getFaultCode());
			String n2 = new String(c1.getFaultFamily()+":"+c1.getFaultMember()+":"+c1.getFaultCode());
			String n3 = new String(p2.getFaultFamily()+":"+p2.getFaultMember()+":"+p2.getFaultCode());
			String n4 = new String(c2.getFaultFamily()+":"+c2.getFaultMember()+":"+c2.getFaultCode());
			if(n1.compareTo(n3) == 0 && n2.compareTo(n4) == 0 && tmp[i].getType().compareTo(rl.getType()) == 0)
				removed = rds.getLinksToCreate().removeReductionLink(tmp[i]);
		}
		if(!removed)
			throw new IllegalStateException("Reduction Rule doesn't exist");
		rds.getLinksToCreate().addReductionLink(rl);
		//Store new values in the CDB.
		//flushReductionRules(rds);
	}
	
	public void deleteReductionRule(alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds, alma.alarmsystem.alarmmessage.generated.ReductionLinkType rl) {
		if(rl == null)
			throw new IllegalArgumentException("Null Reduction Link argument");
		//alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds = getReductionRules();
		boolean removed = false;
		alma.alarmsystem.alarmmessage.generated.ReductionLinkType[] tmp = rds.getLinksToCreate().getReductionLink();
		for (int i = 0; i < tmp.length; i++) {
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p1 = tmp[i].getParent().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition c1 = tmp[i].getChild().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p2 = rl.getParent().getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition c2 = rl.getChild().getAlarmDefinition();
			String n1 = new String(p1.getFaultFamily()+":"+p1.getFaultMember()+":"+p1.getFaultCode());
			String n2 = new String(c1.getFaultFamily()+":"+c1.getFaultMember()+":"+c1.getFaultCode());
			String n3 = new String(p2.getFaultFamily()+":"+p2.getFaultMember()+":"+p2.getFaultCode());
			String n4 = new String(c2.getFaultFamily()+":"+c2.getFaultMember()+":"+c2.getFaultCode());
			if(n1.compareTo(n3) == 0 && n2.compareTo(n4) == 0 && tmp[i].getType().compareTo(rl.getType()) == 0)
				removed = rds.getLinksToCreate().removeReductionLink(tmp[i]);
		}
		if(!removed)
			throw new IllegalStateException("Reduction Rule doesn't exist");
		//Reflect the changes into the AlarmDAO
		alma.alarmsystem.alarmmessage.generated.AlarmDefinition in = rl.getParent().getAlarmDefinition(); 
		Alarm p = getAlarm(in.getFaultFamily()+":"+in.getFaultMember()+":"+in.getFaultCode());
		in = rl.getChild().getAlarmDefinition();
		Alarm c =  getAlarm(in.getFaultFamily()+":"+in.getFaultMember()+":"+in.getFaultCode());
		if(rl.getType().compareTo("NODE") == 0)
			p.removeNodeChild(c);
		else if (rl.getType().compareTo("MULTIPLICITY") == 0)
			p.removeMultiplicityChild(c);
		//Store new values in the CDB.
		//flushReductionRules(rds);
	}
	
	public void addThreshold(alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds, alma.alarmsystem.alarmmessage.generated.Threshold th) {
		if(th == null)
			throw new IllegalArgumentException("Null Threshold argument");
		//alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds = getReductionRules();
		alma.alarmsystem.alarmmessage.generated.Threshold[] tmp = rds.getThresholds().getThreshold();
		for (int i = 0; i < tmp.length; i++) {
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p1 = th.getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p2 = tmp[i].getAlarmDefinition();
			String n1 = new String(p1.getFaultFamily()+":"+p1.getFaultMember()+":"+p1.getFaultCode());
			String n2 = new String(p2.getFaultFamily()+":"+p2.getFaultMember()+":"+p2.getFaultCode());
			if(n1.compareTo(n2) == 0)
				throw new IllegalStateException("Threshold entry already exists");
		}
		rds.getThresholds().addThreshold(th);
		//Reflect the changes into the AlarmDAO
		alma.alarmsystem.alarmmessage.generated.AlarmDefinition in = th.getAlarmDefinition(); 
		Alarm p = getAlarm(in.getFaultFamily()+":"+in.getFaultMember()+":"+in.getFaultCode());
		p.setMultiplicityThreshold(th.getValue());
		//flushReductionRules(rds);
	}
	
	public void updateThreshold(alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds, alma.alarmsystem.alarmmessage.generated.Threshold th) {
		if(th == null)
			throw new IllegalArgumentException("Null Threshold argument");
		boolean removed = false;
		//alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds = getReductionRules();
		alma.alarmsystem.alarmmessage.generated.Threshold[] tmp = rds.getThresholds().getThreshold();
		for (int i = 0; i < tmp.length; i++) {
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p1 = th.getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p2 = tmp[i].getAlarmDefinition();
			String n1 = new String(p1.getFaultFamily()+":"+p1.getFaultMember()+":"+p1.getFaultCode());
			String n2 = new String(p2.getFaultFamily()+":"+p2.getFaultMember()+":"+p2.getFaultCode());
			if(n1.compareTo(n2) == 0)
				removed = rds.getThresholds().removeThreshold(tmp[i]);
		}
		if(!removed)
			throw new IllegalStateException("Threshold doesn't exist");
		rds.getThresholds().addThreshold(th);
		//Reflect the changes into the AlarmDAO
		alma.alarmsystem.alarmmessage.generated.AlarmDefinition in = th.getAlarmDefinition(); 
		Alarm p = getAlarm(in.getFaultFamily()+":"+in.getFaultMember()+":"+in.getFaultCode());
		p.setMultiplicityThreshold(th.getValue());
		//flushReductionRules(rds);
	}
	
	public void deleteThreshold(alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds, alma.alarmsystem.alarmmessage.generated.Threshold th) {
		if(th == null)
			throw new IllegalArgumentException("Null Threshold argument");
		boolean removed = false;
		//alma.alarmsystem.alarmmessage.generated.ReductionDefinitions rds = getReductionRules();
		alma.alarmsystem.alarmmessage.generated.Threshold[] tmp = rds.getThresholds().getThreshold();
		for (int i = 0; i < tmp.length; i++) {
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p1 = th.getAlarmDefinition();
			alma.alarmsystem.alarmmessage.generated.AlarmDefinition p2 = tmp[i].getAlarmDefinition();
			String n1 = new String(p1.getFaultFamily()+":"+p1.getFaultMember()+":"+p1.getFaultCode());
			String n2 = new String(p2.getFaultFamily()+":"+p2.getFaultMember()+":"+p2.getFaultCode());
			if(n1.compareTo(n2) == 0)
				removed = rds.getThresholds().removeThreshold(tmp[i]);
		}
		if(!removed)
			throw new IllegalStateException("Threshold doesn't exist");
		//Reflect the changes into the AlarmDAO
		alma.alarmsystem.alarmmessage.generated.AlarmDefinition in = th.getAlarmDefinition(); 
		Alarm p = getAlarm(in.getFaultFamily()+":"+in.getFaultMember()+":"+in.getFaultCode());
		p.setMultiplicityThreshold(th.getValue());
		//flushReductionRules(rds);
	}
	
	/**
	 * Getter method
	 * 
	 * @return The sources
	 */
	public ConcurrentHashMap<String, Source> getSources() {
		return srcDefs;
	}
}
