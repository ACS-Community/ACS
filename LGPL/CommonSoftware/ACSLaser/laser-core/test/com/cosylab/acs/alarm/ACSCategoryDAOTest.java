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
package com.cosylab.acs.alarm;

import alma.acs.component.client.ComponentClientTestCase;

import cern.laser.business.data.Category;
import cern.laser.business.definition.data.CategoryDefinition;

import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ACSCategoryDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;


/**
 * @author acaproni
 */
public class ACSCategoryDAOTest extends ComponentClientTestCase
{
	private ACSCategoryDAOImpl categoryDAO;
	private ACSAlarmDAOImpl alarmDAO;
	
	public ACSCategoryDAOTest() throws Exception
	{
		super("ACSCategoryDAOTest");
	}
	
	public void setUp() throws Exception
	{
		super.setUp();
		ConfigurationAccessor conf;
		conf = ConfigurationAccessorFactory.getInstance(getContainerServices());
		assertNotNull("Got a null ConfigurationAccessor", conf);
		
		alarmDAO=new ACSAlarmDAOImpl(getContainerServices().getLogger());
		alarmDAO.setConfAccessor(conf);
		assertNotNull("AlarmDAO is null", alarmDAO);
		categoryDAO = new ACSCategoryDAOImpl(getContainerServices().getLogger(),alarmDAO);
		assertNotNull("category DAO is null", categoryDAO);
		categoryDAO.setConfAccessor(conf);
		
		// Load the categories
		categoryDAO.loadCategories();
	}
	
	// Checks the categories loaded from CDB
	public void testLoadCategory() throws Exception
	{
		Integer[] catIds = categoryDAO.getAllCategoryIDs();
		assertEquals(3, catIds.length);
		
		// Lokk for the ROOT to check the parent ID of the other categories
		Integer rootID=null;
		for (Integer val: catIds) {
			Category cat = categoryDAO.getCategory(val);
			if (cat.getName()=="ROOT") {
				rootID=cat.getCategoryId();
				break;
			}
		}
		assertNotNull("ROOT ID not found", rootID);
		
		// Found checks if both the categories are returned by getCategory
		int found=0;
		for (Integer val: catIds) {
			Category cat = categoryDAO.getCategory(val);
			assertNotNull("Got a null category", cat);
			
			assertEquals(cat.getPath(), cat.getName());

			if (cat.getPath().equals("CATEGORY1")) {
				assertEquals(cat.getDescription(), "Test category 1");
				// Check that parentID is the ROOT ID
				found++;
			} else if (cat.getPath().equals("CATEGORY2")) {
				assertEquals(cat.getDescription(), "Test category 2");
				found++;
			}
		}
		assertEquals(2, found);
	}
	
	/**
	 * Test the getting of categories by their IDs and PATHS
	 */
	public void testGetCategoryByID() throws Exception {
		Integer[] IDs = categoryDAO.getAllCategoryIDs();
		
		// get one category by ID and then get the category from the PATH
		// to check if they are the same category
		for (Integer ID: IDs) {
			Category catID = categoryDAO.getCategory(ID);
			Category catPath = categoryDAO.getCategoryByPath(catID.getPath());
			
			assertEquals(catID,catPath);
		}
	}
	
	/**
	 * Get the ROOT category
	 */
	public void testGetRoot() throws Exception {
		Category root = categoryDAO.getCategoryByPath("ROOT");
		assertNotNull("Found a null ROOT category", root);
		
		// The ROOT has a null parentID
		assertNull("The parentID of ROOT is not null", root.getParentId());
		
		// Check the name and the path
		assertEquals(root.getName(), "ROOT");
		assertEquals(root.getPath(), "ROOT");
		
		// ROOT is not a Leaf
		assertFalse("ROOT is not a leaf", root.isLeaf());
	}
	
	/**
	 * Check that all the categories are child of ROOT
	 */
	public void testRootChilds() throws Exception {
		Category root = categoryDAO.getCategoryByPath("ROOT");
		assertNotNull("Found a null ROOT category", root);
		
		Integer[] IDs = categoryDAO.getAllCategoryIDs();
		for (Integer ID: IDs) {
			Category cat = categoryDAO.getCategory(ID);
			// Skip the ROOT
			if (cat==root) {
				continue;
			}
			assertEquals("The category is not child of ROOT", root.getCategoryId(), cat.getParentId());
			
			assertTrue("The category should be a leaf", cat.isLeaf());
		}
	}
	
	/**
	 * Test the deletion of a category
	 * @throws Exception
	 */
	public void testDeleteCategory() throws Exception {
		Integer[] initialIDs = categoryDAO.getAllCategoryIDs();
		assertNotNull(initialIDs);
		int initialLen=initialIDs.length;
		initialIDs=null;
		
		Category root = categoryDAO.getCategoryByPath("ROOT");
		assertNotNull("Found a null ROOT category", root);
		
		Category catToDelete = categoryDAO.getCategory((root.getCategoryId()+1));
		assertNotNull("Category not found", catToDelete);
		
		// Delete the category
		categoryDAO.deleteCategory(catToDelete);
		
		assertEquals(initialLen-1, categoryDAO.getAllCategoryIDs().length);
		
		// Try to get the deleted CAT
		Category removedCat=categoryDAO.getCategory(catToDelete.getCategoryId());
		assertNull("The category should have been deleted",removedCat);
	}
	
	/**
	 * Test the find all categories
	 */
	public void testFindAllCategories() throws Exception {
		Integer[] IDs = categoryDAO.getAllCategoryIDs();
		assertNotNull(IDs);
		Category[] categories = categoryDAO.findAllCategories();
		assertNotNull(categories);
		
		assertEquals(IDs.length, categories.length);
		
		// Check if each ID is in the categories
		int found = 0;
		for (Integer ID: IDs) {
			for (Category cat: categories) {
				if (cat.getCategoryId()==ID) {
					found++;
				}
			}
		}
		assertEquals(IDs.length, found);
	}
	
	/**
	 * Test findCategory and getCategory
	 * @throws Exception
	 */
	public void testGettingCategories() throws Exception {
		Category root = categoryDAO.getCategoryByPath("ROOT");
		assertNotNull("Found a null ROOT category", root);
		
		assertEquals(categoryDAO.getCategory(root.getCategoryId()), categoryDAO.findCategory(root.getCategoryId()));
		
		// Try to get a non existent Category 
		Category cat = categoryDAO.getCategory(Integer.valueOf(-1));
		assertNull("A category with ID=-1 should not be found by getCategory", cat);
		
		cat=root;
		try {
			cat = categoryDAO.findCategory(Integer.valueOf(-1));
		} catch (Exception e) {
			// We expect to be here as the category does not exist
			cat = null;
		}
		assertNull("A category with ID=-1 should not be found", cat);
	}
	
	/**
	 * Test if getting children works
	 * @throws Exception
	 */
	public void testGetChilds() throws Exception {
		// The root is the only one having childs
		Category root = categoryDAO.getCategoryByPath("ROOT");
		assertNotNull("Found a null ROOT category", root);
		
		Integer[] childs = categoryDAO.getChildren(root.getCategoryId());
		assertNotNull(childs);
		assertEquals(categoryDAO.getAllCategoryIDs().length-1,childs.length);
		
		// Other categories have no children
		Category cat = categoryDAO.getCategory(root.getCategoryId()+1);
		assertNotNull(cat);
		childs = categoryDAO.getChildren(cat.getCategoryId());
		assertNotNull(childs);
		assertEquals(0, childs.length);
		
		// The childs of a non existent ID are null
		assertNull(categoryDAO.getChildren(Integer.valueOf(-1)));
	}
	
	/**
	 * Test if updating a category works
	 */
	public void testUpdateCategory() {
		// The root is the only one having childs
		Category root = categoryDAO.getCategoryByPath("ROOT");
		assertNotNull("Found a null ROOT category", root);
		
		// Get the number of defined categories
		Integer[] IDs = categoryDAO.getAllCategoryIDs();
		int len = IDs.length;
		
		// get a category to update
		Category cat = categoryDAO.getCategory(root.getCategoryId()+1);
		assertNotNull(cat);
		
		CategoryDefinition def = cat.getDefinition();
		assertNotNull(def);
		String newDesc = "New Description";
		def.setDescription(newDesc);
		cat.setDefinition(def);
		
		// Update the category
		categoryDAO.updateCategory(cat);
		
		// Check if the size of the categories is right
		assertEquals(len, categoryDAO.getAllCategoryIDs().length);
		
		// Get the category
		cat = categoryDAO.getCategory(cat.getCategoryId());
		assertNotNull("Category not found", cat);
		
		// Check if the description has been updated
		assertEquals(newDesc, cat.getDescription());
	}
}
