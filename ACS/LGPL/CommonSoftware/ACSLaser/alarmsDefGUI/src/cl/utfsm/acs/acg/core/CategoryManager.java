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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.exolab.castor.xml.ValidationException;

import alma.acs.alarmsystem.generated.Alarms;
import alma.acs.alarmsystem.generated.Categories;
import alma.acs.alarmsystem.generated.Category;
import alma.acs.alarmsystem.generated.FaultFamily;

//import com.cosylab.acs.laser.dao.ACSCategoryDAOImpl;
import cl.utfsm.acs.acg.dao.ACSCategoryDAOImpl;

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
	private HashMap<String, ObjectState> _objState;
	private AlarmManager _alarmManager;

	private CategoryManager(CategoryDAO categoryDAO) {
		_categoryDAO = categoryDAO;
		_categoryList = new ArrayList<Category>();
		_objState = new HashMap<String, ObjectState>();
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
		_alarmManager = AlarmSystemManager.getInstance().getAlarmManager();
		try {
			_categoryList = new ArrayList<Category>(Arrays.asList(((ACSCategoryDAOImpl)_categoryDAO).loadCategories()));
			_objState.clear();
			for (Category ctg : _categoryList)
				_objState.put(ctg.getPath(), new ObjectState(false));
		} catch(Exception e) {
			// The category list is empty
			_categoryList = new ArrayList<Category>();
		}
	}
	
	public String checkCDB() {
		String error = "";
		List<Category> cats = _categoryList;
		for(Category c: cats) {
			if(c.getPath() == null || c.getPath().length() == 0)
				error += "Category "+c.getPath()+" doesn't have a path.\n";
			if(!c.hasIsDefault())
				error += "Category "+c.getPath()+" doesn't define isDefault value.\n";
			Alarms als = c.getAlarms();
			if(als != null) {
				String[] ffs = als.getFaultFamily();
				for(String ff: ffs) {
					if(_alarmManager.getFaultFamily(ff) == null)
						error += "FaultFamily "+ff+" defined in category "+c.getPath()+" doesn't exist.\n";
				}
			}
		}
		return error;
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
			for (FaultFamily ff : ffList)
				for(int i=0; i != categoryFFs.length; i++)
					if(categoryFFs[i].compareTo(ff.getName()) == 0)
						throw new IllegalOperationException("The Category can't be removed since it has a FaultFamily assigned");
		}

		for (Iterator<Category> iterator = _categoryList.iterator(); iterator.hasNext();) {
			Category ct = (Category) iterator.next();
			if(ct.getPath().compareTo(c.getPath() ) == 0) {
				iterator.remove();
				ObjectState os = _objState.get(c.getPath());
				if(os == null)
					throw new IllegalOperationException("There is no ObjectState associated with the given Category");
				os.delete();
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

		for (Category ctg : _categoryList)
			if (ctg.getPath().compareTo(c.getPath()) == 0)
				throw new IllegalOperationException("The Category already exists");
		c.setIsDefault(false);
		_categoryList.add(c);
		ObjectState os = _objState.get(c.getPath());
		if(os == null) {
			os = new ObjectState(true);
			os.create();
			_objState.put(c.getPath(), os);
		}
		else
			os.update();
		return true;
	}
	
	public void updateCategory(Category c, Category ci) throws NullPointerException, IllegalOperationException{
		if( c == null || c.getPath() == null )
			throw new NullPointerException("The category to be updated (or its name) is null");
		if( ci == null || ci.getPath() == null )
			throw new NullPointerException("The category with the new values (or its name) is null");
		
		for (Category ctg : _categoryList)
			if(ctg.getPath().compareTo(ci.getPath()) == 0) {
				if(c.getPath().compareTo(ci.getPath()) == 0)
					continue;
				throw new IllegalOperationException("The Category " + ci.getPath() + " already exists");
			}
		
		for (Category ctg : _categoryList)
			if (ctg.getPath().compareTo(c.getPath()) == 0){
				ObjectState os = _objState.get(c.getPath());
				if(os == null)
					throw new IllegalOperationException("There is no ObjectState associated with the given Category");
				if(c.getPath().compareTo(ci.getPath()) == 0)
					os.update();
				else {
					os.delete();
					os = _objState.get(ci.getPath());
					if(os == null){
						os = new ObjectState(true);
						os.create();
						_objState.put(ci.getPath(), os);
					}
					else
						os.update();
				}
				ctg.setPath(ci.getPath());
				ctg.setAlarms(ci.getAlarms());
				ctg.setDescription(ci.getDescription());
				//ctg.setIsDefault(ci.getIsDefault());
				//if(!deleteCategory(ctg))
				//	throw new IllegalOperationException("The category doesn't exist");
				//_categoryList.add(ctg);
				return;
			}
		throw new IllegalOperationException("The Category " + c.getPath() + " doesn't exists");
	}

	public void updateDefaultCategory(Category c){
		if(c.getIsDefault() == false){
			for (Category ctg : _categoryList)
				if (ctg.getIsDefault() == true)
					ctg.setIsDefault(false);
			for (Category ctg : _categoryList)
				if (ctg.getPath().compareTo(c.getPath()) == 0)
					ctg.setIsDefault(true);                    
		}
	}
	
	public void saveToCDB(){
		Set<String> keyset = _objState.keySet();
		String[] objs = new String[keyset.size()];
		keyset.toArray(objs);
		Categories cats = ((ACSCategoryDAOImpl)_categoryDAO).getCategories();
		boolean flush = false;
		try {
			for (int i = 0; i < objs.length; i++) {
				ObjectState os = _objState.get(objs[i]);
				Category c = getCategoryByPath(objs[i]);
				if(c != null)
					c.validate();
				switch(os.getAction()){
				case -1: //Error, no state assigned.
					break;
				case 0:
					break;
				case 1:
					((ACSCategoryDAOImpl)_categoryDAO).addCategory(cats, c);
					flush = true;
					break;
				case 2:
					((ACSCategoryDAOImpl)_categoryDAO).updateCategory(cats, c);
					flush = true;
					break;
				case 3:
					c = new Category();
					c.setPath(objs[i]);
					((ACSCategoryDAOImpl)_categoryDAO).deleteCategory(cats, c);
					flush = true;
					break;
				default: //Shouldn't happen.
					break;
				}
			}
			_objState.clear();
			if(flush)
				((ACSCategoryDAOImpl)_categoryDAO).flushCategories(cats);
			for (Category c : _categoryList)
				_objState.put(c.getPath(), new ObjectState(false));
		} catch (ValidationException e) {
			e.printStackTrace();
		}
	}
}