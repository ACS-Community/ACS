/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
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

/**
 * Implementation of comparable common property, i.e. type of <code>java.lang.Object</code>.
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @author <a href="mailto:cmaureirATinf.utfsm.cl">Cristian Maureira</a>
 * @version $id$
 */
package alma.ACS.jbaci;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.omg.CORBA.Any;
import org.omg.CORBA.TypeCode;
import org.omg.CosPropertyService.ConflictingProperty;
import org.omg.CosPropertyService.ExceptionReason;
import org.omg.CosPropertyService.FixedProperty;
import org.omg.CosPropertyService.InvalidPropertyName;
import org.omg.CosPropertyService.MultipleExceptions;
import org.omg.CosPropertyService.PropertiesHolder;
import org.omg.CosPropertyService.PropertiesIteratorHolder;
import org.omg.CosPropertyService.Property;
import org.omg.CosPropertyService.PropertyException;
import org.omg.CosPropertyService.PropertyNamesHolder;
import org.omg.CosPropertyService.PropertyNamesIteratorHolder;
import org.omg.CosPropertyService.PropertyNotFound;
import org.omg.CosPropertyService.PropertySetPOA;
import org.omg.CosPropertyService.ReadOnlyProperty;
import org.omg.CosPropertyService.UnsupportedProperty;
import org.omg.CosPropertyService.UnsupportedTypeCode;


/**
 * Implementation of <code>org.omg.CosPropertyService.PropertySet</code>.
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @author <a href="mailto:cmmaureirATinf.utfsm.cl">Cristian Maureira</a>
 * @version $id$
 */
public class PropertySetImpl extends PropertySetPOA
{

	private Map<String, Any> propMap; 

	/**
	 * TODO (HSO): Do we ever put any data into this list? 
	 */
	private ArrayList<TypeCode> allowed_property_types;
	
	/**
	 * TODO (HSO): Do we ever put any data into this list? 
	 */
	private ArrayList<String> allowed_properties;
	
	
	//others constructors might be created on demand
	
	public PropertySetImpl(){

		allowed_property_types = new ArrayList<TypeCode>();
		allowed_properties = new ArrayList<String>();
		propMap = new HashMap<String, Any>();
	}
	
	public PropertySetImpl(Property[] propSet) throws MultipleExceptions{

		allowed_property_types = new ArrayList<TypeCode>();
		allowed_properties = new ArrayList<String>();
		propMap = new HashMap<String, Any>();
		define_properties(propSet);
	}
	
	/***checkTypeAndProperty According to documentation, we jave to check if the new property will be valid
	 * according to the constraints that might exists. If both allowed lists are empty, then there aren't 
	 * any constraints.
	 * @param name
	 * @param value
	 * @return boolean True if the type and name of property are valid and can be added
	 * @throws InvalidPropertyName
	 * @throws UnsupportedTypeCode
	 * @throws UnsupportedProperty
	 */
	private boolean checkTypeAndProperty(String name, Any value) throws InvalidPropertyName, UnsupportedTypeCode, UnsupportedProperty{
		
		if(name==null || name.length()==0)
			throw new InvalidPropertyName();
		
		if(value==null)
			return false;
		if(allowed_property_types.size()==0 && allowed_properties.size()==0)
			return true;
		if(!isValidType(value.type()))
			throw new UnsupportedTypeCode();
		if(!isValidProperty(name))
			throw new UnsupportedProperty();
				
		return true;
	}
	
	private boolean isValidProperty(String name) {
		
		if (allowed_properties.size()==0) {
			return true;
		}
		for (int i=0;i<allowed_properties.size();i++) {
			if(allowed_properties.get(i).equals(name)) {
				return true;
			}
		}
		return false;
	}

	private boolean isValidType(TypeCode code) {
		
		if (allowed_property_types.size()==0)
			return true;
		for (int i=0;i<allowed_property_types.size();i++) {
			if(allowed_property_types.get(i).kind() == code.kind()) {
				return true;
			}
		}
		return false;
	}

	public void define_properties(Property[] propSet) throws MultipleExceptions {
		
		ArrayList<PropertyException> errorList = new ArrayList<PropertyException>();
		for (int i=0;i<propSet.length;i++)
			try {
				define_property(propSet[i].property_name,propSet[i].property_value);
			} catch (ConflictingProperty e) {
				errorList.add(new PropertyException(ExceptionReason.conflicting_property,propSet[i].property_name) );
			} catch (UnsupportedProperty e) {
				errorList.add(new PropertyException(ExceptionReason.unsupported_property,propSet[i].property_name) );
			} catch (UnsupportedTypeCode e) {
				errorList.add(new PropertyException(ExceptionReason.unsupported_type_code,propSet[i].property_name) );
			} catch (ReadOnlyProperty e) {
				errorList.add(new PropertyException(ExceptionReason.read_only_property,propSet[i].property_name) );
			} catch (InvalidPropertyName e) {
				errorList.add(new PropertyException(ExceptionReason.invalid_property_name,propSet[i].property_name) );
			}	
			
			if(!errorList.isEmpty()) {
				throw new MultipleExceptions(errorList.toArray(new PropertyException[errorList.size()]));
			}
	}



	public void define_property(String name, Any value)
			throws ConflictingProperty, UnsupportedProperty,
			UnsupportedTypeCode, ReadOnlyProperty, InvalidPropertyName {

		checkTypeAndProperty(name, value);
		if(  propMap.containsKey(name) )
		{
			Property p =  new Property(name, propMap.get(name)) ;
			if (value.type().kind() != p.property_value.type().kind())
				throw new ConflictingProperty();
			else {
				propMap.put(name, value);
			}
		} else {
			propMap.put(name, value);
		}

	}

	public boolean delete_all_properties() {
		//A client could invoke get_number_of_properties to determine how many
		//properties remain. Then invoke get_all_property_names to extract the property
		//names remaining. A separate invocation of delete_property for each such property
		//name is necessary to determine the specific exception.
		/*int i = get_number_of_properties();
		String[] names = get_all_property_names(0,null,null);
		
		for(int a = 0;a <=i;a++ ){
			
				delete_property(names[a]);
			
		}
		*/
		return false;
		
	}

	public void delete_properties(String[] arg0) throws MultipleExceptions {
		//Deletes the properties defined in the property_names parameter. This is a batch
		//operation that returns the MultipleExceptions exception if any delete failed.
		
	}

	public void delete_property(String arg0) throws FixedProperty, PropertyNotFound, InvalidPropertyName {
		//Deletes the specified property if it exists from a PropertySet.
		
	}

	public void get_all_properties(int arg0, PropertiesHolder arg1, PropertiesIteratorHolder arg2) {
		//Returns all of the properties defined in the PropertySet. If more than how_many
		//properties are found, then the remaining properties are returned in Iterator
		
	}

	public void get_all_property_names(int arg0, PropertyNamesHolder arg1, PropertyNamesIteratorHolder arg2) {
		//Returns all of the property names currently defined in the PropertySet. If the
		//PropertySet contains more than how_many property names, then the remaining
		//property names are put into the PropertyNamesIterator.
		
	}

	public int get_number_of_properties() {
		return propMap.size();
	}

	public boolean get_properties(String[] arg0, PropertiesHolder arg1) {
		//Returns the values of the properties listed in property_names.
		//When the boolean flag is true, the Properties parameter contains valid values for all
		//requested property names. If false, then all properties with a value of type tk_void
		//may have failed due to PropertyNotFound or InvalidPropertyName.
		//A separate invocation of get_property for each such property name is necessary to
		//determine the specific exception or to verify that tk_void is the correct any
		//TypeCode for that property name.
		//This approach was taken to avoid a complex, hard to program structure to carry mixed
		//results.
		return false;
	}

	public Any get_property_value(String name) throws PropertyNotFound, InvalidPropertyName {
		//Returns the value of a property in the PropertySet.
		Any p = null;
		if(propMap.containsKey(name)){
			p = propMap.get(name);
		} 
		else{
			throw new PropertyNotFound();
		}
		return p;
	}

	public boolean is_property_defined(String arg0) throws InvalidPropertyName {
		return false;
	}


}
