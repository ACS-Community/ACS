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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

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

public class ACSCategoryDAOImpl implements CategoryDAO
{
	int nextCatID=1;
	AlarmDAO alarmDao;
	ConfigurationAccessor conf;

	HashMap catIDtoCategory;
	HashMap catPathToCategory;
	
	static String getCategoriesPath()
	{
		return "/Alarms/CategoryDefinitions";
	}
	
	static String getCategoryAlarmLinksPath()
	{
		return "/Alarms/AlarmCategoryDefinitions";
	}

	String surveillanceCategoryPath;

	String categoryTreeRoot;
	
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
		return (Category)catIDtoCategory.get(identifier);
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
		Collection cats=catIDtoCategory.values();
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
		
		catIDtoCategory.put(cimpl.getCategoryId(), cimpl);
		if (cimpl.getPath()!=null)
			catPathToCategory.put(cimpl.getPath(), cimpl);
	}

	public void updateCategory(Category category)
	{
		Integer id=category.getCategoryId();
		
		if (id==null)
			throw new IllegalStateException();
		
		Category previous=(Category) catIDtoCategory.get(id);
		
		catPathToCategory.values().remove(previous); // path may have changed
		
		catIDtoCategory.put(id, category);
		if (category.getPath()!=null)
			catPathToCategory.put(category.getPath(), category);
	}

	public void deleteCategory(Category category)
	{
		Integer id=category.getCategoryId();
		
		if (id==null)
			throw new IllegalStateException();
		
		Category previous=(Category) catIDtoCategory.get(id);
		
		catIDtoCategory.remove(id);
		
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
		
		ArrayList result=new ArrayList();
		
		Iterator i=catIDtoCategory.values().iterator();
		while (i.hasNext()) {
			CategoryImpl c=(CategoryImpl)i.next();

			// root categories have null parentId
			if (c.getParentId() != null &&
				c.getParentId().intValue()==pid)
				result.add(c.getCategoryId());
		}
		
		Integer[] out=new Integer[result.size()];
		result.toArray(out);
		
		return out;
	}
	
	public void loadCategories()
	{
		if (conf==null)
			throw new IllegalStateException("null configuration accessor");
		
		String xml;
		
		try {
			xml=conf.getConfiguration(getCategoriesPath());
		} catch (Exception e) {
			throw new RuntimeException("Failed to read "+getCategoriesPath(), e);
		}
        System.out.println("\n\nCategoriesPath="+xml);
		
		CategoryDefinitions acds;
		try {
			acds=(CategoryDefinitions) CategoryDefinitions.unmarshal(new StringReader(xml));
		} catch (Exception e) {
			throw new RuntimeException("Failed to parse "+getCategoriesPath(), e);
		}
		
		CategoriesToCreate ctc=acds.getCategoriesToCreate();
		if (ctc==null)
			throw new RuntimeException(getCategoriesPath()+" must contain categories-to-create");
		
		CategoryDefinition[] cds=ctc.getCategoryDefinition();
		if (cds==null || cds.length<1) {
			throw new RuntimeException("No categories defined in "+getCategoriesPath());
		}
		
		catIDtoCategory=new HashMap();
		catPathToCategory=new HashMap();
		
		for (int a=0; a<cds.length; a++) {
			CategoryDefinition cd=cds[a];
			CategoryImpl ci=new CategoryImpl();
			ci.setAlarmIds(new HashSet()); // will get filled in later
			ci.setCategoryId(new Integer(nextCatID++));
			ci.setChildrenIds(new HashSet());
			ci.setDescription(cd.getDescription());
			ci.setName(cd.getPath());
			ci.setParentId(null);
			ci.setPath(cd.getPath());
			ci.setAlarmIds(new HashSet());
			
			catIDtoCategory.put(ci.getCategoryId(), ci);
			catPathToCategory.put(ci.getPath(), ci);
		}	
	}
	
	public void linkWithAlarms()
	{
		if (conf==null)
			throw new IllegalStateException("null configuration accessor");
		if (alarmDao==null)
			throw new IllegalStateException("missing alarm DAO");
		
		String path=getCategoryAlarmLinksPath();
		
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
		
		Iterator i=catPathToCategory.values().iterator();
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
			conf.setConfiguration(getCategoriesPath(), catList.toString());
			conf.setConfiguration(getCategoryAlarmLinksPath(), linkList.toString());
		} catch (Exception e) {
			throw new RuntimeException("Failed to store configuration", e);
		}
	}

	public void setConfAccessor(ConfigurationAccessor conf)
	{
		this.conf=conf;
	}

	public void setAlarmDAO(AlarmDAO alarmDAO)
	{
		this.alarmDao=alarmDAO;
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
		Set keyset=catIDtoCategory.keySet();
		Integer[] result=new Integer[keyset.size()];
		keyset.toArray(result);
		return result;
	}
}