package cl.utfsm.acs.acg.dao;

import alma.acs.alarmsystem.generated.Alarms;
import alma.acs.alarmsystem.generated.Categories;
import alma.acs.alarmsystem.generated.Category;
import cl.utfsm.acs.acg.core.AcsInformation;
import cl.utfsm.acs.acg.core.AlarmManager;
import cl.utfsm.acs.acg.core.DAOManager;
import junit.framework.TestCase;

public class ACSCategoryDAOImplTest extends TestCase{
	AcsInformation _acsInfo;
	DAOManager _daoManager;
	AlarmManager _am;
	ACSCategoryDAOImpl _categoryDAO;

	public void setUp() throws Exception {
		_acsInfo = new AcsInformation("Test client");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
		_daoManager.connect();
		_categoryDAO = (ACSCategoryDAOImpl)_daoManager.getCategoryDAO();
	}
	
	public void testSaveCategory(){
		//boolean exception;
		Category c1;
		
		c1 = new Category();
		c1.setPath("Foobar");
		c1.setDescription("Foobar Category");
		c1.setIsDefault(false);
		Alarms alarms = new Alarms();
		alarms.addFaultFamily("ffTest");
		c1.setAlarms(alarms);

		Categories cats = _categoryDAO.getCategories();
		_categoryDAO.addCategory(cats, c1);
	}
	
	public void testUpdateCategory(){
		//boolean exception;
		Category c1;
		
		c1 = new Category();
		c1.setPath("Foobar");
		c1.setDescription("Foobar Category2");
		c1.setIsDefault(false);
		Alarms alarms = new Alarms();
		alarms.addFaultFamily("ffTest");
		c1.setAlarms(alarms);

		Categories cats = _categoryDAO.getCategories();
		_categoryDAO.updateCategory(cats, c1);
	}
	
	public void testDeleteCategory(){
		//boolean exception;
		Category c1;
		
		c1 = new Category();
		c1.setPath("Foobar");
		c1.setDescription("Foobar Category2");
		c1.setIsDefault(false);
		Alarms alarms = new Alarms();
		alarms.addFaultFamily("ffTest");
		c1.setAlarms(alarms);

		Categories cats = _categoryDAO.getCategories();
		_categoryDAO.deleteCategory(cats, c1);
	}

	public void tearDown() throws Exception {
		_acsInfo.disconnect();
	}
}
