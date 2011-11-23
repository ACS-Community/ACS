/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
package cl.utfsm.acs.acg.dao;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

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

import alma.acs.logging.AcsLogLevel;
import cern.laser.business.LaserObjectNotFoundException;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.Category;
import cern.laser.business.data.CategoryImpl;
import cern.laser.business.data.Triplet;

import com.cosylab.acs.laser.dao.xml.AlarmCategoryDefinitions;
import com.cosylab.acs.laser.dao.xml.AlarmCategoryLink;
import com.cosylab.acs.laser.dao.xml.AlarmDefinition;
import com.cosylab.acs.laser.dao.xml.CategoriesToCreate;
import com.cosylab.acs.laser.dao.xml.CategoryDefinition;
import com.cosylab.acs.laser.dao.xml.CategoryDefinitions;
import com.cosylab.acs.laser.dao.xml.CategoryLinksToCreate;

import alma.acs.alarmsystem.generated.Categories;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.alarmsystem.core.alarms.LaserCoreAlarms;
import alma.alarmsystem.core.alarms.LaserCoreFaultState;
import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;

public class ACSCategoryDAOImpl extends com.cosylab.acs.laser.dao.ACSCategoryDAOImpl {	
	int nextCatID=1;
	AlarmDAO alarmDao;
	ConfigurationAccessor conf;
	
	// The PATH in the CDB
	private static final String CATEGORY_DEFINITION_PATH = "/Alarms/Administrative/Categories";
	private static final String ALARM_CATEGORY_DEFINITION_PATH = "/Alarms/Administrative/AlarmCategoryDefinitions";

	// The HashMap with the path of all the categories
	HashMap<String, Category> catPathToCategory = new HashMap<String, Category>();
	
	// The HashMap of the categories (the key is the ID of the category)
	HashMap<Integer, Category> categories = new HashMap<Integer, Category>();
	
	//public ACSCategoryDAOImpl(Logger log, com.cosylab.acs.laser.dao.ACSAlarmDAOImpl alarmDAO) {
	//	super(log, alarmDAO);
	//}
	
	/**
	 * The default category used to publish alarms not assigned to any category.
	 * 
	 * This is read by the CDB from the is-default attribute of a category.
	 * In the configuration files there should be only one default category but 
	 * we can't consider an error if there is more then one (in this case a log message 
	 * is submitted). If the default category is defined more then once, the first 
	 * definition is used.
	 * 
	 * If no default category is defined another log message is submitted. 
	 * In this case alarms not assigned to any category will remain unassigned.
	 */
	private Category defaultCategory=null;
	public Category getDefaultCategory() { return defaultCategory; }
	
	String surveillanceCategoryPath;

	String categoryTreeRoot;
	
	// The logger
	Logger logger;
	
	/**
	 * Constructor 
	 * 
	 * @param log The log (not null)
	 */
	public ACSCategoryDAOImpl(Logger log,com.cosylab.acs.laser.dao.ACSAlarmDAOImpl alarmDAO) {
		super(log,alarmDAO);
		if (log==null) {
			throw new IllegalArgumentException("Invalid null logger");
		}
		if (alarmDAO==null) {
			throw new IllegalArgumentException("Invalid null alarmDAO");
		}
		this.alarmDao=alarmDAO;
		logger =log;
	}
	
	public Category findCategory(Integer identifier)
	{
		Category result = getCategory(identifier);

		if (result == null)
			throw new LaserObjectNotFoundException(
					"Couldn't find category with ID " + identifier);

		return result;
	}

	public Category getCategory(Integer identifier)
	{
		return categories.get(identifier);
	}

	public Category findByCategoryTreeRoot()
	{
		return findCategoryByPath(categoryTreeRoot);
	}

	public Category findBySurveillanceCategory()
	{
		return findCategoryByPath(surveillanceCategoryPath);
	}

	public Category[] findAllCategories()
	{
		Collection<Category> cats=categories.values();
		Category[] result=new Category[cats.size()];
		cats.toArray(result);
		return result;
	}

	public Category findCategoryByPath(String path)
	{
		Category result = getCategoryByPath(path);
		if (result == null)
			throw new LaserObjectNotFoundException(
					"Couldn't find category with path " + path);

		return result;
	}

	public Category getCategoryByPathInitialized(String path)
	{
		return getCategoryByPath(path);
	}

	public Category getCategoryByPath(String path)
	{
		return (Category) catPathToCategory.get(path);
	}

	public void saveCategory(Category category)
	{
		// actual save is done in flushCategory();
		
		CategoryImpl cimpl=(CategoryImpl)category;
		
		if (cimpl.getCategoryId()==null) {
			cimpl.setCategoryId(new Integer(nextCatID++));
		}
		
		categories.put(cimpl.getCategoryId(), cimpl);
		if (cimpl.getPath()!=null)
			catPathToCategory.put(cimpl.getPath(), cimpl);
	}

	public void updateCategory(Category category)
	{
		Integer id=category.getCategoryId();
		
		if (id==null)
			throw new IllegalStateException();
		
		Category previous=categories.get(id);
		
		catPathToCategory.values().remove(previous); // path may have changed
		
		categories.put(id, category);
		if (category.getPath()!=null)
			catPathToCategory.put(category.getPath(), category);
	}

	public void deleteCategory(Category category)
	{
		Integer id=category.getCategoryId();
		
		if (id==null)
			throw new IllegalStateException();
		
		Category previous=categories.get(id);
		
		categories.remove(id);
		
		// this one was in catPath, so remove that one instead
		// have to use values(), because the path may have changed,
		// but mapping didn't
		catPathToCategory.values().remove(previous);
	}

	public String[] getAlarms(Integer categoryId)
	{
		Category c=getCategory(categoryId);
		if (c==null)
			return new String[0];
		
		Set ids=((CategoryImpl)c).getAlarmIds();
		String[] result=new String[ids.size()];
		ids.toArray(result);
		
		return result;
	}

	public Integer[] getChildren(Integer parentId)
	{
		int pid=parentId.intValue();
		
		CategoryImpl cat=(CategoryImpl) getCategory(parentId);
		if (cat==null)
			return null;
		
		ArrayList<Integer> result=new ArrayList<Integer>();
		
		Iterator<Category> i=categories.values().iterator();
		while (i.hasNext()) {
			Category c=i.next();

			// root categories have null parentId
			if (c.getParentId() != null &&
				c.getParentId().intValue()==pid)
				result.add(c.getCategoryId());
		}
		
		Integer[] out=new Integer[result.size()];
		result.toArray(out);
		
		return out;
	}
	
	/**
	 * Load the categories from the CDB.
	 * <P>
	 * Loads all the category from the CDB and build an internal
	 * representation of category.
	 * The category is also added to all the alarms having the
	 * fault family specified in the XML.
	 * <P>
	 * All the categories derive from ROOT that is built here
	 * as default (in this way the user does ot need to add the ROOT
	 * entry in the CDB).
	 * 
	 * @return list of Category entries read from CDB 
	 * @throws Exception In case of error reading the values from the CDB
	 */
	public alma.acs.alarmsystem.generated.Category[] loadCategories() throws Exception {
		if (conf==null) {
			throw new IllegalStateException("Missing dal");
		}
		categories.clear();
		String xml;
		try {
			xml=conf.getConfiguration(CATEGORY_DEFINITION_PATH);
		} catch (Throwable t) {
			throw new RuntimeException("Couldn't read alarm list", t);
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
			throw new Exception("Malformed xml: only one node (categories) expected");
		}
		Node categoriesNode = docChilds.item(0);
		
		Unmarshaller FF_unmarshaller = new Unmarshaller(Categories.class);
		FF_unmarshaller.setValidation(false);
		FF_unmarshaller.setWhitespacePreserve(true);
		Categories daoCategories;
		try {
			daoCategories = (Categories) FF_unmarshaller.unmarshal(categoriesNode);
			logger.log(AcsLogLevel.DEBUG,"Categories definition read");
		} catch (Exception e) {
			throw new Exception("Error parsing "+CATEGORY_DEFINITION_PATH,e);
		}
		alma.acs.alarmsystem.generated.Category[] daoCategory = daoCategories.getCategory();
		if (daoCategory==null || daoCategory.length==0) {
			logger.log(AcsLogLevel.DEBUG,"No category defined");
		}
		
		// Add the root Category
		addRootCategory();
		
		// Goes through all the Categories read from the CDB
		for (alma.acs.alarmsystem.generated.Category category: daoCategory) {
			cern.laser.business.definition.data.CategoryDefinition definition = new cern.laser.business.definition.data.CategoryDefinition(category.getPath(),category.getDescription());
			
			CategoryImpl ci=new CategoryImpl();
			ci.setAlarmIds(new HashSet()); // will get filled in later
			ci.setCategoryId(new Integer(nextCatID++));
			ci.setChildrenIds(new HashSet<Integer>());
			ci.setDescription(definition.getDescription());
			ci.setName(definition.getPath());
			ci.setPath(definition.getPath());
			ci.setAlarmIds(new HashSet());
			setParentID(ci);
			
			// Stores the categories
			categories.put(ci.getCategoryId(), ci);
			catPathToCategory.put(ci.getPath(), ci);
			logger.log(AcsLogLevel.DEBUG,"Category "+ci.getName()+" added with ID="+ci.getCategoryId());

			// Check if the category is defined as default.
			if (category.hasIsDefault() && category.getIsDefault()==true) {
				if (defaultCategory!=null) {
					StringBuilder str = new StringBuilder("CDB misconfiguration: default category defined more then once (actual default: ");
					str.append(defaultCategory.getPath());
					str.append(", new default: ");
					str.append(category.getPath());
					logger.log(AcsLogLevel.WARNING,str.toString());
				} else {
					defaultCategory=ci;
				}
			}
			
			// A category contains a set of child ids.
			// This method adjusts the references of categories between the parent 
			// and the child
			adjustParentIDs(ci.getName(),ci.getCategoryId()); 
			
			
			// Connect alarms to this category
			for (alma.acs.alarmsystem.generated.Category cat : daoCategories.getCategory()) {
				if (cat.getPath().equals(ci.getPath())) {
					String[] families=cat.getAlarms().getFaultFamily();
					for (String faultFamily: families) {
						assignCategoryToAlarms(ci, faultFamily);
					}
				}
			}
		}
		// Assign core alarms to ROOT category
		assignCategoryOfCoreAlarms();
		// Log a message if no category has been defined in the CDB
		if (defaultCategory==null) {
			logger.log(AcsLogLevel.WARNING,"No default category defined in CDB");
		} else {
			// Check if there are alarms without category to assign to the default
			assignDefaultCategory(defaultCategory);
		}

        return daoCategory;
	}
	
	/**
	 * Assign core alarms to the root category.
	 * 
	 * @see LaserCoreAlarms
	 * @see LaserCoreFaultState
	 * @see LaserCoreFaultCodes
	 */
	private void assignCategoryOfCoreAlarms() {
		String[] coreIds = LaserCoreFaultState.getCoreAlarmIds();
		Category rootCategory=getCategoryByPath("ROOT");
		assignCategoryToAlarms(rootCategory, LaserCoreFaultState.FaultFamily);
	}
	
	/**
	 * Assign the default category to the alarms not assigned to any category
	 * 
	 * Scans all the alarms to check for alarms without any category and assign the default
	 * category to them.
	 * 
	 * @param defCategory The default category
	 */
	private void assignDefaultCategory(Category defCategory) {
		if (defCategory==null) {
			throw new IllegalArgumentException("Invalid null category");
		}
		String[] IDs = ((ACSAlarmDAOImpl)alarmDao).getAllAlarmIDs();
		for (String alarmID: IDs) {
			Alarm alarm = alarmDao.getAlarm(alarmID);
			if (alarm==null) {
				logger.log(AcsLogLevel.WARNING,"Got a null alarm for ID="+alarmID);
				continue;
			}
			Collection<Category> categories = alarm.getCategories();
			if (categories==null) {
				categories = new HashSet<Category>();
			}
			if (categories.size()==0) {
				categories.add(defCategory);
				alarm.setCategories(categories);
				StringBuilder str = new StringBuilder("Alarm ");
				str.append(alarm.getAlarmId());
				str.append(" assigned to default category ");
				str.append(defCategory.getPath());
				logger.log(AcsLogLevel.DEBUG,str.toString());
			}
		}
	}
	
	/**
	 * Set the ID of this category in the children list of its parents
	 * 
	 * A category contains a list of all its children.
	 * The first category is ROOT.
	 * If a category is child of another category is inferred by its name.
	 * If a category has no parents, it is set to be a ROOT child.
	 * 
	 * @param childrenName The name of this category
	 * @param ID The ID of this category 
	 */
	private void adjustParentIDs(String childrenName, int ID) {
		if (childrenName==null || childrenName.length()==0) {
			throw new IllegalArgumentException("Invalid children name "+childrenName);
		}
		// If the name does not contains ':' then it is a child of ROOT
		if (!childrenName.contains(":")) {
			CategoryImpl root = (CategoryImpl)getCategoryByPath("ROOT");
			Set<Integer> children = root.getChildrenIds();
			if (children==null) {
				children=new HashSet<Integer>();
			}
			children.add(new Integer(ID));
			root.setChildrenIds(children);
			return;
		}
		// The name contains ':' 
		int pos = childrenName.lastIndexOf(':');
		String parentID=childrenName.substring(0, pos);
		CategoryImpl parent = (CategoryImpl)getCategoryByPath(parentID);
		if (parent==null) {
			logger.log(AcsLogLevel.WARNING,"Parent category of "+parentID+" NOT found");
			return;
		}
		Set<Integer> children = parent.getChildrenIds();
		if (children==null) {
			children=new HashSet<Integer>();
		}
		children.add(new Integer(ID));
		parent.setChildrenIds(children);
	}
	
	/**
	 * Set the parent ID of the passed category
	 * 
	 * Each category has a parent ID that can be evaluated by reading
	 * the name of the category.
	 * If the name does not contain ':' then the parent ID is the ROOT.
	 * Otherwise its parent is the category whose name is represented
	 * by the substring before the ':'
	 * 
	 * @param cat
	 */
	private void setParentID(CategoryImpl cat) {
		if (cat.getPath().equals("ROOT")) {
			// ROOT has no parent
			cat.setParentId(null);
			return;
		}
		String name = cat.getPath();
		int pos =name.lastIndexOf(':');
		if (pos==-1) {
			// This category parent is ROOT
			Category root = getCategoryByPath("ROOT");
			cat.setParentId(root.getCategoryId());
			cat.setPath(cat.getPath());
			cat.setName(cat.getPath());
			return;
		}
		String parentID=name.substring(0, pos);
		CategoryImpl parent = (CategoryImpl)getCategoryByPath(parentID);
		if (parent==null) {
			logger.log(AcsLogLevel.WARNING,"Parent category of "+parentID+" NOT found");
			return;
		}
		cat.setParentId(parent.getCategoryId());
	}
	
	/**
	 * Add the ROOT category
	 * 
	 * This avoid the user to add this entry in the CDB
	 */
	private void addRootCategory() {
		CategoryImpl ci=new CategoryImpl();
		ci.setAlarmIds(new HashSet()); // will get filled in later
		ci.setCategoryId(new Integer(nextCatID++));
		ci.setChildrenIds(new HashSet());
		ci.setDescription("ROOT category");
		ci.setName("ROOT");
		ci.setParentId(null);
		ci.setPath("ROOT");
		ci.setAlarmIds(new HashSet());
		
		// Stores the categories
		categories.put(ci.getCategoryId(), ci);
		catPathToCategory.put(ci.getPath(), ci);
	}
	
	/**
	 * Dumps the category.
	 * 
	 */
	private void dumpCategories() {
		Set<Integer> keys=categories.keySet();
		if (keys==null ||keys.size()==0) {
			System.out.println("No categories");
		}
		for (Integer i : keys) {
			CategoryImpl cat = (CategoryImpl)getCategory(i);
			if (cat==null) {
				System.out.println("Null categor for ID "+i);
			}
			System.out.println("Category ID="+cat.getCategoryId());
			System.out.println("\tName: "+cat.getName());
			System.out.println("\tPath: "+cat.getPath());
			System.out.println("\tDesc: "+cat.getDescription());
			System.out.println("\tParentID: "+cat.getParentId());
			System.out.println("\tChildren: ");
			Set<Integer> childs = cat.getChildrenIds();
			if (childs==null || childs.size()==0) {
				System.out.println("\t\tNo childs");
			} else {
				for (Integer child: childs) {
					System.out.println("\t\t"+child);
				}
			}
		}
	}
	
	/**
	 * Assign the category to the all the alarms of a given FaultFamily.
	 * 
	 * In the CDB each category has a list (eventually empty) of FaultFamily.
	 * If a FaultFamily appear in the definition of a Category then add
	 * such category to all the alarms of such FF.
	 *   
	 *  @param category The category to assign to the alarms
	 *  @param FF The fault family of the alarms to assign the category to
	 */
	private void assignCategoryToAlarms(Category category, String FF) {
		if (category==null) {
			throw new IllegalArgumentException("Invalid null category");
		}
		if (FF==null) {
			throw new IllegalArgumentException("Invalid null fault family");
		}
		String[] alarmIDs=((ACSAlarmDAOImpl)alarmDao).getAllAlarmIDs();
		for (String id: alarmIDs) {
			Alarm alarm = alarmDao.getAlarm(id);
			if (alarm==null) {
				logger.log(AcsLogLevel.WARNING,"Got a null alarm for ID="+id);
				continue;
			}
			if (alarm.getTriplet().getFaultFamily().equals(FF)) {
				Collection<Category> alarmCategories = alarm.getCategories();
				if (!alarmCategories.contains(category)) {
					alarmCategories.add(category);
					logger.log(AcsLogLevel.DEBUG,"Category "+category.getName()+" assigned to alarm "+alarm.getAlarmId());
				}
			}
		}
	}
	
	public void linkWithAlarms()
	{
		if (conf==null)
			throw new IllegalStateException("null configuration accessor");
		if (alarmDao==null)
			throw new IllegalStateException("missing alarm DAO");
		
		String path=ALARM_CATEGORY_DEFINITION_PATH;
		
		String xml;
		try {
			xml=conf.getConfiguration(path);
		} catch (Exception e) {
			throw new RuntimeException("Failed to read "+path);
		}
		
		AlarmCategoryDefinitions acds;
		try {
			acds=(AlarmCategoryDefinitions) AlarmCategoryDefinitions.unmarshal(new StringReader(xml));
		} catch (Exception e) {
			throw new RuntimeException("Failed to parse "+path);
		}
		
		CategoryLinksToCreate cltc=acds.getCategoryLinksToCreate();
		if (cltc==null)
			throw new RuntimeException("Missing category-links-to-create in "+path);
		
		AlarmCategoryLink[] links=cltc.getAlarmCategoryLink();
		
		for (int a=0; a<links.length; a++) {
			AlarmCategoryLink l=links[a];
			com.cosylab.acs.laser.dao.xml.Alarm linkAlarm=l.getAlarm();
			com.cosylab.acs.laser.dao.xml.Category linkCat=l.getCategory();
			if (linkAlarm==null || linkCat==null)
				throw new RuntimeException("Missing alarm or category in a link in "+path);
			
			AlarmDefinition ad=linkAlarm.getAlarmDefinition();
			CategoryDefinition cd=linkCat.getCategoryDefinition();
			
			if (ad==null || cd==null)
				throw new RuntimeException("Missing alarm-definition or category-definition in "+path);
			
			String lff=ad.getFaultFamily();
			String lfm=ad.getFaultMember();
			if (lff==null || lfm==null)
				throw new RuntimeException("Missing fault-family or fault-member in "+path);
			
			String alarmId=Triplet.toIdentifier(lff, lfm, new Integer(ad.getFaultCode()));
			String catPath=cd.getPath();
			
			if (catPath==null)
				throw new RuntimeException("Missing category path in "+path);
			
			Alarm alarm=alarmDao.getAlarm(alarmId);
			Category cat=getCategoryByPath(catPath);
			
			if (alarm==null)
				throw new RuntimeException("Missing alarm with ID "+alarmId);
			if (cat==null)
				throw new RuntimeException("Missing category with path "+catPath);
			
			cat.addAlarm(alarm);
		}
		
		Iterator i=catPathToCategory.values().iterator();
		while (i.hasNext()) {
			CategoryImpl ci=(CategoryImpl)i.next();
			String cPath=ci.getPath();
			int lastcolon=cPath.lastIndexOf(':');
			if (lastcolon>=0) {
				String parentCPath=cPath.substring(0, lastcolon);
				
				CategoryImpl cp=(CategoryImpl)catPathToCategory.get(parentCPath);
				if (cp!=null) {
					cp.addChildCategory(ci);
				}
			}
		}
	}

	public void flushCategory()
	{
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("no writable configuration accessor");
		
		CategoryDefinitions acds=new CategoryDefinitions();
		CategoriesToCreate ctc=new CategoriesToCreate();
		acds.setCategoriesToCreate(ctc);
		
		AlarmCategoryDefinitions linksTop=new AlarmCategoryDefinitions();
		CategoryLinksToCreate cltc=new CategoryLinksToCreate();
		linksTop.setCategoryLinksToCreate(cltc);
		
		Iterator<Category> i=catPathToCategory.values().iterator();
		while (i.hasNext()) {
			CategoryDefinition cd=new CategoryDefinition();
			CategoryImpl ci=(CategoryImpl)i.next();
			
			cd.setDescription(ci.getDescription());
			cd.setPath(ci.getPath());
			
			ctc.addCategoryDefinition(cd);
			
			Iterator aidsi=ci.getAlarmIds().iterator();
			while (aidsi.hasNext()) {
				String aid=(String)aidsi.next();
				Alarm a=alarmDao.getAlarm(aid);
				if (a==null)
					throw new RuntimeException("Category has a link to a non-existent alarm");
				
				AlarmCategoryLink link=new AlarmCategoryLink();

				com.cosylab.acs.laser.dao.xml.Alarm linkAlarm=new com.cosylab.acs.laser.dao.xml.Alarm();
				com.cosylab.acs.laser.dao.xml.Category linkCat=new com.cosylab.acs.laser.dao.xml.Category();
				link.setAlarm(linkAlarm);
				link.setCategory(linkCat);
				
				AlarmDefinition linkAlarmDef=new AlarmDefinition();
				CategoryDefinition linkCatDef=new CategoryDefinition();
				linkAlarm.setAlarmDefinition(linkAlarmDef);
				linkCat.setCategoryDefinition(linkCatDef);
				
				linkAlarmDef.setFaultCode(a.getTriplet().getFaultCode().intValue());
				linkAlarmDef.setFaultFamily(a.getTriplet().getFaultFamily());
				linkAlarmDef.setFaultMember(a.getTriplet().getFaultMember());
				
				linkCatDef.setPath(ci.getPath());
				
				cltc.addAlarmCategoryLink(link);
			}
		}

		StringWriter catList=new StringWriter();
		try {
			acds.marshal(catList);
		} catch (Exception e) {
			throw new RuntimeException("Failed to encode categories", e);
		}
		
		StringWriter linkList=new StringWriter();
		try {
			acds.marshal(linkList);
		} catch (Exception e) {
			throw new RuntimeException("Failed to encode link", e);
		}
		
		try {
			conf.setConfiguration(CATEGORY_DEFINITION_PATH, catList.toString().replaceFirst("xsi:type=\".*\"", ""));
			conf.setConfiguration(ALARM_CATEGORY_DEFINITION_PATH, linkList.toString().replaceFirst("xsi:type=\".*\"", ""));
		} catch (Exception e) {
			throw new RuntimeException("Failed to store configuration", e);
		}
	}

	public void setConfAccessor(ConfigurationAccessor conf)
	{
		this.conf=conf;
	}
	
	public void setCategoryTreeRoot(String categoryTreeRoot)
	{
		this.categoryTreeRoot = categoryTreeRoot;
	}

	public void setSurveillanceCategoryPath(String surveillanceCategoryPath)
	{
		this.surveillanceCategoryPath = surveillanceCategoryPath;
	}

	public Integer[] getAllCategoryIDs()
	{
		Set<Integer> keyset=categories.keySet();
		Integer[] result=new Integer[keyset.size()];
		keyset.toArray(result);
		return result;
	}
	
	public Categories getCategories(){
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("no writable configuration accessor");
		Categories cats;
		String xml;
		try {
			xml = conf.getConfiguration(CATEGORY_DEFINITION_PATH);
		} catch (CDBRecordDoesNotExistEx e) {
			cats = new Categories();
			return cats;
		} catch (Exception e1) {
			e1.printStackTrace();
			return null;
		}
		StringReader FFReader = new StringReader(xml);
		Unmarshaller FF_unmarshaller = new Unmarshaller(Categories.class);
		FF_unmarshaller.setValidation(false);
		try {
			cats = (Categories)FF_unmarshaller.unmarshal(FFReader);
		} catch (MarshalException e1) {
			e1.printStackTrace();
			return null;
		} catch (ValidationException e1) {
			e1.printStackTrace();
			return null;
		}
		try {
			cats.validate();
		} catch (ValidationException e1) {
			e1.printStackTrace();
		}
		return cats;
	}
	
	public void flushCategories(Categories cats) {
		if (conf==null || !conf.isWriteable())
			throw new IllegalStateException("no writable configuration accessor");
		if(cats == null)
			throw new IllegalArgumentException("Null Categories argument");
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
			FF_marshaller.marshal(cats);
		} catch (MarshalException e) {
			e.printStackTrace();
			return;
		} catch (ValidationException e) {
			e.printStackTrace();
			return;
		}
		try {
			conf.deleteConfiguration(CATEGORY_DEFINITION_PATH);
			conf.addConfiguration(CATEGORY_DEFINITION_PATH, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
		} catch (org.omg.CORBA.UNKNOWN e) {
			try {
				conf.addConfiguration(CATEGORY_DEFINITION_PATH, FFWriter.toString().replaceFirst("xsi:type=\".*\"", ""));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		} catch (CDBXMLErrorEx e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
			throw new IllegalStateException("Category already exists");
		}
	}
	
	public void addCategory(Categories cats, alma.acs.alarmsystem.generated.Category cat){
		if(cat == null)
			throw new IllegalArgumentException("Null Category argument");
		alma.acs.alarmsystem.generated.Category[] tmp = cats.getCategory();
		for (int i = 0; i < tmp.length; i++)
			if(tmp[i].getPath().compareTo(cat.getPath()) == 0)
				throw new IllegalStateException("Category already exist");
		cats.addCategory(cat);
	}
	
	public void updateCategory(Categories cats, alma.acs.alarmsystem.generated.Category cat){
		if(cat == null)
			throw new IllegalArgumentException("Null Category argument");
		boolean removed = false;
		alma.acs.alarmsystem.generated.Category[] tmp = cats.getCategory();
		for (int i = 0; i < tmp.length; i++)
			if(tmp[i].getPath().compareTo(cat.getPath()) == 0)
				removed = cats.removeCategory(tmp[i]);
		if(!removed)
			throw new IllegalStateException("Category doesn't exist");
		cats.addCategory(cat);
	}
	
	public void deleteCategory(Categories cats, alma.acs.alarmsystem.generated.Category cat){
		if(cat == null)
			throw new IllegalArgumentException("Null Category argument");
		boolean removed = false;
		alma.acs.alarmsystem.generated.Category[] tmp = cats.getCategory();
		for (int i = 0; i < tmp.length; i++)
			if(tmp[i].getPath().compareTo(cat.getPath()) == 0)
				removed = cats.removeCategory(tmp[i]);
		if(!removed)
			throw new IllegalStateException("Category doesn't exist");
	}
}
