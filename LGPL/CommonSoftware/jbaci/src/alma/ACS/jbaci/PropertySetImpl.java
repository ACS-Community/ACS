package alma.ACS.jbaci;

import java.util.ArrayList;

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
import org.omg.CosPropertyService.PropertySetOperations;

public class PropertySetImpl implements PropertySetOperations
								 {

	
	
	private ArrayList propList;
	private ArrayList valueList;
	private ArrayList allowed_property_types;
	private ArrayList allowed_properties;
	
	public PropertySetImpl(){
		
		propList = new ArrayList();
		valueList = new ArrayList();
		allowed_property_types = new ArrayList();
		allowed_properties = new ArrayList();
	}
	
	public PropertySetImpl(Property[] propSet) throws MultipleExceptions{
		
		propList = new ArrayList();
		valueList = new ArrayList();
		allowed_property_types = new ArrayList();
		allowed_properties = new ArrayList();
		define_properties(propSet);
	}
	
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
		
		if (allowed_properties.size()==0)
			return true;
		for (int i=0;i<allowed_properties.size();i++)
			if(allowed_properties.get(i).toString()==name)
			  return true;
		
		return false;
	}

	private boolean isValidType(TypeCode code) {
		
		if (allowed_property_types.size()==0)
			return true;
		for (int i=0;i<allowed_property_types.size();i++)
			if(((TypeCode)allowed_property_types.get(i)).kind()==code.kind())
			  return true;
		
		return false;
	}

	public void define_properties(Property[] propSet) throws MultipleExceptions {
		
		ArrayList errorList = new ArrayList();
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
			
			if(errorList.size()>0)
			throw new MultipleExceptions((PropertyException[])errorList.toArray());
	}


	
	public void define_property(String name, Any value)
			throws ConflictingProperty, UnsupportedProperty,
			UnsupportedTypeCode, ReadOnlyProperty, InvalidPropertyName {

		checkTypeAndProperty(name, value);
		
		int tmp = propList.indexOf(name);
		if (propList.indexOf(name) != -1) {
			Property p =  new Property((String)propList.toArray()[tmp],(Any)valueList.toArray()[tmp]) ;
			if (value.type().kind() != p.property_value.type().kind())
				throw new ConflictingProperty();
			else 
				propList.add(name);
				valueList.add(value);
		} else {

			propList.add(name);
			valueList.add(value);
		}

	}

	public boolean delete_all_properties() {
		// TODO Auto-generated method stub
		return false;
	}

	public void delete_properties(String[] arg0) throws MultipleExceptions {
		// TODO Auto-generated method stub
		
	}

	public void delete_property(String arg0) throws FixedProperty, PropertyNotFound, InvalidPropertyName {
		// TODO Auto-generated method stub
		
	}

	public void get_all_properties(int arg0, PropertiesHolder arg1, PropertiesIteratorHolder arg2) {
		// TODO Auto-generated method stub
		
	}

	public void get_all_property_names(int arg0, PropertyNamesHolder arg1, PropertyNamesIteratorHolder arg2) {
		// TODO Auto-generated method stub
		
	}

	public int get_number_of_properties() {
		// TODO Auto-generated method stub
		return 0;
	}

	public boolean get_properties(String[] arg0, PropertiesHolder arg1) {
		// TODO Auto-generated method stub
		return false;
	}

	public Any get_property_value(String arg0) throws PropertyNotFound, InvalidPropertyName {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean is_property_defined(String arg0) throws InvalidPropertyName {
		// TODO Auto-generated method stub
		return false;
	}


}
