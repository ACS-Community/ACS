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

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.WDAL;

public class ConfigurationAccessorFactory
{
	private static class DALConfAccessor implements ConfigurationAccessor
	{
		final DAL dal;
		
		public DALConfAccessor(DAL dal)
		{
			if (dal==null)
				throw new IllegalArgumentException("null DAL");
			
			this.dal=dal;
		}

		public String getConfiguration(String path) throws Exception 
		{
			return dal.get_DAO(path);
		}

		public void setConfiguration(String path, String data) throws Exception 
		{
			if (isWriteable()) {
				((WDAL)dal).set_DAO(path, data);
			} else {
				throw new Exception("This ConfigurationAccessor is not writeable");
			}
		}
		
		public boolean isWriteable()
		{
			return dal instanceof WDAL;
		}

		public void deleteConfiguration(String path) throws Exception
		{
			if (isWriteable())
				((WDAL)dal).remove_node(path);
			else
				throw new Exception("This ConfigurationAccessor is not writeable");
		}
	}
	
	private ConfigurationAccessorFactory()
	{
		
	}
	
	public static ConfigurationAccessor getInstance(ContainerServicesBase contBase) throws AcsJContainerServicesEx
	{
		return new DALConfAccessor(contBase.getCDB());
	}
}
