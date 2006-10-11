/*
 * Created on Jul 18, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.acs.alarm;

import alma.acs.component.ComponentLifecycleException;

import com.cosylab.acs.laser.dao.ACSCategoryDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;


/**
 * @author Simon Belak
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class ACSCategoryDAOTest extends AlarmTestBase
{
	ACSCategoryDAOImpl categoryDAO;
	
	public ACSCategoryDAOTest()
	{
		super();
		try	{
			init();
		} catch (Exception e) {
			System.out.println("Failed to init: " + e.toString());
			System.exit(-1);
		}
	}
	
	public void init() throws Exception
	{
		ConfigurationAccessor conf;
		conf = ConfigurationAccessorFactory.getInstance(client.getContainerServices());
		
		categoryDAO = new ACSCategoryDAOImpl();
		categoryDAO.setConfAccessor(conf);
	}
		
	public void testFindByCategoryTreeRoot()
	{
		System.out.print("Loading categories ... ");
		try	{
			categoryDAO.loadCategories();
		} catch (Exception e) {
			System.out.println("Failed");
			System.out.println(e.toString());
			return;
		}
		System.out.println("Done");
		
		categoryDAO.setCategoryTreeRoot("ROOT:SOURCE");
		System.out.println("categoryTreeRoot set to ROOT:SOURCE");
		
		System.out.print("Evoking findByCategoryTreeRoot() ... ");
		try {
			categoryDAO.findByCategoryTreeRoot();
		} catch (Exception e) {
			System.out.println("Failed");
			System.out.println(e.toString());
			return;
		}
		System.out.println("Done");
	}
}
