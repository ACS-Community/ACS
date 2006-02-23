/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

import com.cosylab.introspection.DataFormatter;

import abeans.datatypes.DynamicValueDescriptor;
import abeans.models.meta.LinkableRealization;

/**
 * Descriptor of ACS properties.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class TypelessPropertyDescriptor
	extends DynamicValueDescriptor
	implements LinkableRealization {

	/**
	 * Property type.
	 */
	Class propertyType;

	/**
	 * @param dataAccessTypes
	 * @param dataTypes
	 * @param settable
	 */
	public TypelessPropertyDescriptor(String name, Class propertyType,
									  Class[] dataAccessTypes, Class[] dataTypes, boolean settable) {
		super(dataAccessTypes, dataTypes, settable);
		this.propertyType = propertyType;
	}

	/**
	 * @see abeans.models.meta.LinkableRealization#isTransient()
	 */
	public boolean isTransient() {
		return false;
	}

	/**
	 * @see abeans.models.meta.ClassRepresentable#getType()
	 */
	public Class getType() {
		return propertyType;
	}

	/**
	 * Returns textual description of this object.
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer sb = new StringBuffer(200);
		sb.append(DataFormatter.toString(getClass()));
		sb.append(" = { name = '");
		sb.append(getName());
		sb.append("' propertyType = '");
		sb.append(propertyType);
		sb.append("' }");
		return sb.toString();
	}

}
