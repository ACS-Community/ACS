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

import java.util.ArrayList;
import java.util.regex.Pattern;

import org.omg.CORBA.Any;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CosPropertyService.MultipleExceptions;
import org.omg.CosPropertyService.Property;
import org.omg.CosPropertyService.PropertySet;
import org.omg.CosPropertyService.PropertySetPOATie;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManagerPackage.AdapterInactive;

import alma.ACS.CharacteristicModelOperations;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.jbaci.PropertySetImpl;
import alma.acs.container.ContainerServices;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.WrongCDBDataTypeEx;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DAO;

import com.cosylab.util.WildcharMatcher;;


/**
 * Implementation of <code>alma.ACS.CharacteristicModel</code>.
 * TODO temporary implementation - real caching (DAL wide, not per CharacteristicModelImpl instance) has to be implemented
 * TODO what about reconnection then... is persistent DAL server enough?
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @author <a href="mailto:cmaureirATinf.utfsm.cl">Cristian Maureira</a>
 * @version $id$
 */
// TODO to be fully implemented, cached, etc.
public class CharacteristicModelImpl implements CharacteristicModelOperations {

	//for create an Any (i don't know any other method!)
	private ContainerServices m_container;
	public void lendContainerServices(ContainerServices c){
		m_container = c;
	}
	
	
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
		

	//cmenay

		 try{
			String strVal = new String();
			if (prefix=="")	
				strVal = dao.get_string(name);
			else
				strVal = dao.get_string(prefix+name);

			 //I needed the getAny() to create a new Any, since a constructor for
			 // Any (i.e: new Any() ), doesn't exist
			 Any value_p = m_container.getAdvancedContainerServices().getAny();
			 value_p.insert_string(strVal);
			 return value_p;

         }
		 catch (CDBFieldDoesNotExistEx fde){
                NoSuchCharacteristic nsc = new NoSuchCharacteristic();
                nsc.characteristic_name = name;
                nsc.component_name = modelName;
                throw nsc;
         }

		 catch (SystemException se){
			 throw se;
		 } 
		 
		 
		 catch (WrongCDBDataTypeEx wct) {
				
		}
		 
		throw new NoSuchCharacteristic();
	}

	/**
	 * @see alma.ACS.CharacteristicModelOperations#find_characteristic(java.lang.String)
	 */
	public String[] find_characteristic(String wildcard) {
		//cmenay
		try {
			String[] allSeq; 
				if (prefix=="")	
					allSeq = dao.get_string_seq("");
				else
					allSeq = dao.get_string_seq(prefix);
			
			int max;
			max = allSeq.length;
			ArrayList<String> arrSeq = new ArrayList<String>();
			
			String regExpStr = WildcharMatcher.simpleWildcardToRegex(wildcard);
			Pattern pattern = Pattern.compile(regExpStr);

			for(int i=0;i<max;i++)
				if( pattern.matcher(allSeq[i]).matches())
					arrSeq.add(allSeq[i]);
			if (arrSeq.isEmpty())
				throw new CDBFieldDoesNotExistEx();
			
			String[] ret = new String[arrSeq.size()];
			
			for(int i=0;i<arrSeq.size();i++)
				ret[i]=arrSeq.get(i);
			
			return ret;
			
		} catch (CDBFieldDoesNotExistEx e) {
			return new String[0];
		} catch (WrongCDBDataTypeEx e) {
			return new String[0];
		}
	
	}

	/**
	 * @see alma.ACS.CharacteristicModelOperations#get_all_characteristics()
	 */
	public PropertySet get_all_characteristics() {
		

			String[] allSeq; 
			
			try {
				
				if (prefix=="")	
					allSeq = dao.get_string_seq("");
				else
					allSeq = dao.get_string_seq(prefix);
				Property[] p = new Property[allSeq.length];
				for (int i=0;i<allSeq.length;i++){
					Any a = get_characteristic_by_name(allSeq[i]);
					p[i] = new Property(allSeq[i],a);
				}

				//dangerous methods!! 
				 ORB orb = m_container.getAdvancedContainerServices().getORB();
				 POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			     rootpoa.the_POAManager().activate();

				PropertySetImpl psetImpl = new PropertySetImpl(p);
				PropertySetPOATie psetTie = new PropertySetPOATie(psetImpl,rootpoa);

				return psetTie._this(orb);

			} catch (CDBFieldDoesNotExistEx e) {

			} catch (WrongCDBDataTypeEx e) {

			} catch (NoSuchCharacteristic e) {
				
			} catch (MultipleExceptions e) {
				
			} catch (InvalidName e) {

			} catch (AdapterInactive e) {

			} catch (NullPointerException e){
				System.out.println( e);
			}

				

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
	 * Read double characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	
	//cmenay
	public float getFloat(String name)
		throws NoSuchCharacteristic
	{
		// TODO temporary implementation
		try
		{
			return (float)dao.get_double(prefix+name);
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
	 * Read sequence double characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public float[] getFloatSeq(String name)
		throws NoSuchCharacteristic
	{

		try
		{
			double[] temp = dao.get_double_seq(prefix+name);
			float[] ret = new float[temp.length];
			
			for(int i=0;i<temp.length;i++){				
				ret[i] = (float)temp[i];	
			}
			return ret;
			
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
	public int[] getLongSeq(String name)
		throws NoSuchCharacteristic
	{

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
	 * Read sequence string characteristic.
	 * @param name	characteristic name.
	 * @throws NoSuchCharacteristic is thrown if characterstic does not exist.
	 */
	public String[] getStringSeq(String name)
		throws NoSuchCharacteristic
	{

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
