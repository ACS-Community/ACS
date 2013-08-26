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
package cl.utfsm.acs.acg.dao;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;
import com.cosylab.CDB.WDAL;

public class ConfigurationAccessorFactory
{
	private static class DALConfAccessor implements ConfigurationAccessor
	{
		final DAL dal;
		final JDAL jdal;
		
		public DALConfAccessor(DAL dal)
		{
			if (dal==null)
				throw new IllegalArgumentException("null DAL");
			
			this.dal=dal;
			jdal = JDALHelper.narrow(dal);
		}

		public String getConfiguration(String path) throws Exception 
		{
			return dal.get_DAO(path);
		}

		public void setConfiguration(String path, String data) throws Exception 
		{
			if (isWriteable()) {
				((WDAL)dal).set_DAO(path, data);
				jdal.clear_cache_all();
			} else {
				throw new Exception("This ConfigurationAccessor is not writeable");
			}
		}
		
		public void addConfiguration(String path, String data) throws Exception
		{
			if (isWriteable()) {
				System.out.println(path);
				System.out.println(data);
				((WDAL)dal).add_node(path, data);
				jdal.clear_cache_all();
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
			if (isWriteable()){
				((WDAL)dal).remove_node(path);
				jdal.clear_cache_all();
			}
			else
				throw new Exception("This ConfigurationAccessor is not writeable");
		}
		
		public String listConfigurations(String path)
		{
			return dal.list_nodes(path);
		}
	}

	private ConfigurationAccessorFactory()
	{
		
	}
	
	public static ConfigurationAccessor getInstance(DAL dal) throws AcsJContainerServicesEx
	{
		return new DALConfAccessor(dal);
	}
}

