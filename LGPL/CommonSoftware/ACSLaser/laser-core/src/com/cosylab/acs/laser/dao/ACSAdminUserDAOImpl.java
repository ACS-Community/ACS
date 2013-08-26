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

import cern.laser.business.LaserObjectNotFoundException;
import cern.laser.business.dao.AdminUserDAO;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.AdminUser;
import cern.laser.business.data.Category;
import cern.laser.business.data.Source;

class HardcodedAdminUser extends AdminUser
{
	public HardcodedAdminUser()
	{
		super("adminUser", "adminPass12");
	}
	
	public void addAdministeredCategory(Category category)
	{
		// ignore - everything id administered
	}
	public void addAdministeredSource(Source source)
	{
		// ignore - everything is administered
	}
	public boolean administersCategory(Integer categoryId)
	{
		return true;
	}
	public boolean administersSource(Source source)
	{
		return true;
	}
	public void removeAdministeredCategory(Category category)
	{
		throw new UnsupportedOperationException();
	}
	public void removeAdministeredSource(Source source)
	{
		throw new UnsupportedOperationException();
	}
	public boolean equals(Object obj)
	{
		return obj instanceof HardcodedAdminUser;
	}
	public Integer getDefaultConfiguration()
	{
		return new Integer(1);
	}
	public String getDefaultPrinter()
	{
		return "default printer";
	}
	public Integer getIdent()
	{
		return new Integer(1);
	}
	public String getName()
	{
		return "adminUser";
	}
	public String getPassword()
	{
		return "adminPass12";
	}
	public String getUserId()
	{
		return "adminUser";
	}
	public int hashCode()
	{
		return getUserId().hashCode();
	}
	public void setDefaultConfiguration(Integer defaultConfiguration)
	{
		if (defaultConfiguration.intValue()!=1)
			throw new UnsupportedOperationException();
	}
	public void setDefaultPrinter(String defaultPrinter)
	{
		if (!"default printer".equals(defaultPrinter))
			throw new UnsupportedOperationException();
	}
	protected void setIdent(Integer ident)
	{
		if (ident.intValue()!=1)
			throw new UnsupportedOperationException();
	}
	protected void setPassword(String password)
	{
		if (!"adminPass12".equals(password))
			throw new UnsupportedOperationException();
	}
	protected void setUserId(String userId)
	{
		if (!"adminUser".equals(userId))
			throw new UnsupportedOperationException();
	}
}

public class ACSAdminUserDAOImpl implements AdminUserDAO 
{
	public static final AdminUser theAdminUser=new HardcodedAdminUser();

	String laserAdminUser;
	
	AlarmDAO alarmDAO;
	SourceDAO sourceDAO;
	CategoryDAO categoryDAO;
	
	public void setAlarmDAO(AlarmDAO alarmDAO)
	{
		this.alarmDAO=alarmDAO;
	}
	
	public void setSourceDAO(SourceDAO sourceDAO)
	{
		this.sourceDAO=sourceDAO;
	}
	
	public void setCategoryDAO(CategoryDAO categoryDAO)
	{
		this.categoryDAO=categoryDAO;
	}
	
	public AdminUser findAdminUser(String identifier) 
	{
		if (identifier.equals(theAdminUser.getUserId()))
			return theAdminUser;
		
		throw new LaserObjectNotFoundException("User "+identifier+" not found");
	}

	public AdminUser findByLaserAdminUser()
	{
		return findAdminUser(laserAdminUser);
	}

	public AdminUser findAdminUserByNamePassword(String name, String password) 
	{
		if (name.equals(theAdminUser.getUserId()) && password.equals(theAdminUser.getPassword()))
			return theAdminUser;
		
		throw new LaserObjectNotFoundException("No such user");
	}

	public AdminUser getAdminUserByName(String name) 
	{
		if (name.equals(theAdminUser.getUserId()))
			return theAdminUser;
		
		return null;
	}

	public String[] getAdministeredSources(String userId) 
	{
		if (sourceDAO==null)
			throw new IllegalStateException("Missing source DAO");
		
		if (userId.equals("adminUser")) {
			if (sourceDAO instanceof ACSSourceDAOImpl) {
				return ((ACSSourceDAOImpl)sourceDAO).getAllSourceIDs();
			} else {
				throw new UnsupportedOperationException();
			}
		} else {
			return null;
		}
	}

	public Integer[] getAdministeredCategories(String userId)
	{
		if (categoryDAO==null)
			throw new IllegalStateException("Missing categoryDAO");
		
		if (userId.equals(theAdminUser.getUserId())) {
			if (categoryDAO instanceof ACSCategoryDAOImpl) {
				return ((ACSCategoryDAOImpl)categoryDAO).getAllCategoryIDs();
			} else {
				throw new UnsupportedOperationException();
			}
		} else {
			return null;
		}
	}

	public AdminUser[] findAllAdminUsers() 
	{
		return new AdminUser[] { theAdminUser };
	}

	public void saveAdminUser(AdminUser adminUser) 
	{
		// ignore
	}

	public void deleteAdminUser(AdminUser adminUser) 
	{
		throw new UnsupportedOperationException();
	}

	public void updateAdminUser(AdminUser admin_user) 
	{
		throw new UnsupportedOperationException();
	}
}
