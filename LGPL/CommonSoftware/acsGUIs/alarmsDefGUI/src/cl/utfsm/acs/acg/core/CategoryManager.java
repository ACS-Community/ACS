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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import alma.acs.alarmsystem.generated.Category;
import alma.acs.alarmsystem.generated.FaultFamily;

import com.cosylab.acs.laser.dao.ACSCategoryDAOImpl;

import cern.laser.business.dao.CategoryDAO;

/**
 * Class used to manage all the information about categories that the ACG
 * uses.
 * @author rtobar
 *
 */
public class CategoryManager implements EntityManager {

	/**
	 * The singleton instance shared across the project
	 */
	private static CategoryManager _instance;

	private CategoryDAO _categoryDAO;
	private List<Category> _categoryList;

	private CategoryManager(CategoryDAO categoryDAO) {
		_categoryDAO = categoryDAO;
		_categoryList = new ArrayList<Category>();
	}

	public static CategoryManager getInstance(CategoryDAO categoryDAO) {
		if( _instance == null ) {
			_instance = new CategoryManager(categoryDAO);
		}
		return _instance;
	}

	public List<Category> getAllCategories() {
		return _categoryList;
	}

	public Category getCategoryByPath(String path) {
		for (Category category : _categoryList) {
			if(category.getPath().compareTo(path) == 0) {
				return category;
			}
		}
		return null;
	}

	public void loadFromCDB() {

		try {
			_categoryList = new ArrayList<Category>(Arrays.asList(((ACSCategoryDAOImpl)_categoryDAO).loadCategories()));
		} catch(Exception e) {
			// The category list is empty
			_categoryList = new ArrayList<Category>();
		}
	}

	/**
	 * Destroys the singleton instance of this class. This is needed to renew the internal reference to
	 * the CategoryDAO if a new connection to the DAL and the ACS Manager has been performed
	 */
	public static void destroy() {
		_instance = null;
	}
	
	
	/**
	 * Deletes a category. The category to be deleted is checked against the existing
	 * Fault Families in order to preserve the consistency of the application
	 * (i.e., a category cannot be deleted if it currently being used
	 *  by a Fault Family). 
	 * @param c The category to be deleted
	 * @return True if it deletes the given category, false otherwise
	 * @throws NullPointerException If the given category is null
	 * @throws IllegalOperationException If the category is part of a existing Fault Family
	 */
	public boolean deleteCategory(Category c) throws NullPointerException, IllegalOperationException {

		if( c == null || c.getPath() == null )
			throw new NullPointerException("The category to be deleted (or its name) is null");

		// Check the category FFs with the existing FFs
		AlarmManager alarmManager = AlarmSystemManager.getInstance().getAlarmManager();
		List<FaultFamily> ffList = alarmManager.getAllAlarms();

		if( c.getAlarms() != null ) {
			String [] categoryFFs = c.getAlarms().getFaultFamily();

			for (Iterator<FaultFamily> iterator = ffList.iterator(); iterator.hasNext();) {
				FaultFamily faultFamily = (FaultFamily) iterator.next();
				for(int i=0; i!=categoryFFs.length; i++) {
					if( categoryFFs[i].compareTo(faultFamily.getName()) == 0 )
						throw new IllegalOperationException();
				}
			}
		}

		for (Iterator<Category> iterator = _categoryList.iterator(); iterator.hasNext();) {
			Category ct = (Category) iterator.next();
			if( ct.getPath().compareTo(c.getPath() ) == 0 ) {
				iterator.remove();
				return true;
			}
		}   

		return false;		
	}
	    
	/**
	 * Add a new Category. If there is a Category with the same name that the given Category, the new Category cannot be added 
	 * @param c The Category to be added
	 * @return True If the Category is added , False otherwise 
	 * @throws IllegalOperationException If the Category to add already exists
	 * @throws NullPointerException If the Category is null 
	 */
	public boolean addCategory(Category c) throws IllegalOperationException, NullPointerException {
		
		if( c == null || c.getPath() == null )
			throw new NullPointerException("The category to be added (or its name) is null");
		
		for (Iterator<Category> iterator = _categoryList.iterator(); iterator.hasNext();) {
			Category ctg= (Category) iterator.next();
			if ( ctg.getPath().compareTo(c.getPath()) == 0 ){
				throw new IllegalOperationException();
			}
		} 
		
		_categoryList.add(c);
		return true;
	}
}
	
