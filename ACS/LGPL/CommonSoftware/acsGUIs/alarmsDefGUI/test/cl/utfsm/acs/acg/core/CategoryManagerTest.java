/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
 *    Copyright by ESO (in the framework of the ALMA collaboration),
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
package cl.utfsm.acs.acg.core;

import java.util.Iterator;
import java.util.List;

import alma.acs.alarmsystem.generated.Category;

import junit.framework.TestCase;

public class CategoryManagerTest extends TestCase { 

	AlarmSystemManager _alarmSystemManager;
	CategoryManager cm;

	public void setUp() throws Exception {

		_alarmSystemManager = AlarmSystemManager.getInstance(UserAuthenticator.Role.Administrator);
		_alarmSystemManager.connectToManager();
		_alarmSystemManager.connectToDAL();
		cm = _alarmSystemManager.getCategoryManager();
		cm.loadFromCDB();

	}

	public void testGetInstance() throws Exception {

		// Use the old way here...
		_alarmSystemManager.disconnectFromManager();
		AlarmSystemManager.destroy();

		DAOManager _daoManager;
		AcsInformation _acsInfo;
		_acsInfo = new AcsInformation("Test client");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
		_daoManager.connect();

		CategoryManager cm1;
		CategoryManager cm2;
		cm1 = CategoryManager.getInstance(_daoManager.getCategoryDAO());
		cm2 = CategoryManager.getInstance(_daoManager.getCategoryDAO());
		assertNotNull(cm1);
		assertNotNull(cm2);
		assertEquals(cm1,cm2);

		_acsInfo.disconnect();
		_alarmSystemManager = AlarmSystemManager.getInstance(UserAuthenticator.Role.Administrator);
		_alarmSystemManager.connectToManager();
		_alarmSystemManager.connectToDAL();
	}

	public void testGetAllCategories() {

		CategoryManager cm = _alarmSystemManager.getCategoryManager();
		assertNotNull(cm);
		cm.loadFromCDB();
		List<Category> categories = cm.getAllCategories();
		assertNotNull(categories);
		for (Category category : categories)
			assertNotNull(category);

	}

	public void testGetCategoryByPath() {

		CategoryManager cm = _alarmSystemManager.getCategoryManager();
		cm.loadFromCDB();
		List<Category> categories = cm.getAllCategories();
		Category category2 = null;
		for (Category category : categories) {
			category2 = cm.getCategoryByPath(category.getPath());
			assertNotNull(category2);
			assertEquals(category,category2);
		}

	}

	public void testLoadFromCDB() {

		CategoryManager cm = _alarmSystemManager.getCategoryManager();
		cm.loadFromCDB();
		List<Category> categories1 = cm.getAllCategories();
		assertNotNull(categories1);
		cm.loadFromCDB();
		List<Category> categories2 = cm.getAllCategories();
		assertNotNull(categories2);

		assertEquals(categories1.size(), categories2.size());
		Iterator<Category> iterator1 = categories1.iterator();
		Iterator<Category> iterator2 = categories2.iterator();
		for (; iterator1.hasNext();) {
			Category category1 = (Category)iterator1.next();
			Category category2 = (Category)iterator2.next();
			assertEquals(category1.getDescription(), category2.getDescription());
			assertEquals(category1.getIsDefault(), category2.getIsDefault());
			assertEquals(category1.getPath(), category2.getPath());
		}
	}
	
	public void testAddCategory() throws IllegalOperationException{
		
		List<Category> categories1 = cm.getAllCategories();
		assertNotNull(categories1);
		Category c = new Category();
		c.setPath("NEW CATEGORY");
		cm.addCategory(c);		
		List<Category> categories2 = cm.getAllCategories();
		assertNotNull(categories2);
		assertEquals(categories1.size() , categories2.size());
		
		boolean check = false;
		
		for (Iterator<Category> iterator = categories2.iterator(); iterator.hasNext();) {
			Category ctg= (Category) iterator.next();
			if ( ctg.getPath().compareTo("NEW CATEGORY") == 0 ){
				check = true; 
			}
		}
		assertTrue(check);
		
		check = false;
		try {
			cm.addCategory(c);
		} catch (IllegalOperationException e) {
			check = true;
		}
		assertTrue(check);
		
	}

	public void testDeleteCategory() throws Exception {
		CategoryManager cm = _alarmSystemManager.getCategoryManager();
		List<Category> categories1 = cm.getAllCategories();
		assertNotNull(categories1);   
		      
		boolean check = false;
		try {
			cm.deleteCategory(categories1.get(0));
		} catch (IllegalOperationException e) {
			check = true;
		}
		assertTrue(check);
		
//		List<Category> categories2 = cm.getAllCategories();
//		assertNotNull(categories2);
//		int size2 = categories2.size();
//		assertEquals(size1-1,size2);       

		boolean exception = false;
		try {
			cm.deleteCategory(null);
		} catch(NullPointerException e) {
			exception = true;        
		}
		assertTrue(exception);

		Category c = new Category();
		exception = false;
		try {
			cm.deleteCategory(c);
		} catch (NullPointerException e) {
			exception = true;
		}        
		assertTrue(exception);

		c.setPath("foobar");
		assertFalse(cm.deleteCategory(c));    
	}
	

	public void tearDown() {

		_alarmSystemManager.disconnectFromManager();
		AlarmSystemManager.destroy();
		CategoryManager.destroy();
	}
}
