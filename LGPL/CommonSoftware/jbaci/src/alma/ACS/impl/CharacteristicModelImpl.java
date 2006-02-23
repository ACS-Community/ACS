/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.ACS.impl;

import org.omg.CORBA.Any;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CosPropertyService.PropertySet;

import alma.ACS.CharacteristicModelOperations;
import alma.ACS.NoSuchCharacteristic;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DAO;

/**
 * Implementation of <code>alma.ACS.CharacteristicModel</code>.
 * TODO temporary implementation - real caching (DAL wide, not per CharacteristicModelImpl instance) has to be implemented
 * TODO what about reconnection then... is persistent DAL server enough?
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
// TODO to be fully implemented, cached, etc.
public class CharacteristicModelImpl implements CharacteristicModelOperations {

	/**
	 * Model name (used to determine CDB lookup).
	 */
	protected String modelName;

	/**
	 * CDB DAO.
	 */
	protected DAO dao;


	// TODO remove this crap
	private String prefix = "";
	public void setFieldPrefix(String prefix)
	{
		this.prefix = prefix;
	}

	/**
	 * @param modelName	name of the model (used to determine CDB lookup), non-<code>null</code>.
	 * @param dal		CDB DAL object, non-<code>null</code>.
	 */
	public CharacteristicModelImpl(String modelName, DAL dal) {

		if (modelName == null)
			throw new NullPointerException("modelName == null");	

		if (dal == null)
			throw new NullPointerException("dal == null");	

		this.modelName = modelName;
		
		try
		{
			this.dao = dal.get_DAO_Servant(modelName);
		}
		catch (Throwable th)
		{
			// TODO throw better exception
			throw new NullPointerException("Failed to get DAO for '" + modelName + "'.");
		}
	}

	/*********************** [ CharacteristicModel ] ***********************/

	/**
	 * @see alma.ACS.CharacteristicModelOperations#get_characteristic_by_name(java.lang.String)
	 */
	public Any get_characteristic_by_name(String name)
		throws NoSuchCharacteristic {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}

	/**
	 * @see alma.ACS.CharacteristicModelOperations#find_characteristic(java.lang.String)
	 */
	public String[] find_characteristic(String wildcard) {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}

	/**
	 * @see alma.ACS.CharacteristicModelOperations#get_all_characteristics()
	 */
	public PropertySet get_all_characteristics() {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}

	/*********************** [ Helpers ] ***********************/

	/**
	 * Read string characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public String getString(String name)
		throws NoSuchCharacteristic
	{
		// TODO temporary implementation
		try
		{
			return dao.get_string(prefix+name);
		}
		catch (Throwable th)
		{
			throw new NoSuchCharacteristic(name, modelName);
		}
	}

	/**
	 * Read long characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public long getLong(String name)
		throws NoSuchCharacteristic
	{
		// TODO temporary implementation
		try
		{
			return (long)dao.get_long(prefix+name);
		}
		catch (Throwable th)
		{
			throw new NoSuchCharacteristic(name, modelName);
		}
	}

	/**
	 * Read int characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public int getInteger(String name)
		throws NoSuchCharacteristic
	{
		// TODO temporary implementation
		try
		{
			return dao.get_long(prefix+name);
		}
		catch (Throwable th)
		{
			throw new NoSuchCharacteristic(name, modelName);
		}
	}

	/**
	 * Read double characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public double getDouble(String name)
		throws NoSuchCharacteristic
	{
		// TODO temporary implementation
		try
		{
			return dao.get_double(prefix+name);
		}
		catch (Throwable th)
		{
			throw new NoSuchCharacteristic(name, modelName);
		}
	}

	/**
	 * Read sequence long characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public int[] getIntegerSeq(String name)
		throws NoSuchCharacteristic
	{
		// TODO temporary implementation
		try
		{
			return dao.get_long_seq(prefix+name);
		}
		catch (Throwable th)
		{
			throw new NoSuchCharacteristic(name, modelName);
		}
	}

	/**
	 * Read sequence double characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public double[] getDoubleSeq(String name)
		throws NoSuchCharacteristic
	{
		// TODO temporary implementation
		try
		{
			return dao.get_double_seq(prefix+name);
		}
		catch (Throwable th)
		{
			throw new NoSuchCharacteristic(name, modelName);
		}
	}

	/**
	 * Read sequence string characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public String[] getStringSeq(String name)
		throws NoSuchCharacteristic
	{
		// TODO temporary implementation
		try
		{
			return dao.get_string_seq(prefix+name);
		}
		catch (Throwable th)
		{
			throw new NoSuchCharacteristic(name, modelName);
		}
	}
}
