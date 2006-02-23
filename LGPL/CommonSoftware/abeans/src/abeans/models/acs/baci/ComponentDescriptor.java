/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

import com.cosylab.introspection.DataFormatter;

import abeans.models.meta.ConnectableRealization;
import abeans.models.meta.ModelingElementDescriptor;
import abeans.models.meta.NamespaceRealization;
import abeans.models.meta.TargetPart;

/**
 * Descriptor of ACS components.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class ComponentDescriptor
	extends ModelingElementDescriptor
	implements ConnectableRealization, NamespaceRealization {

	/**
	 * Component type.
	 */
	Class componentType;

	/**
	 * Constrcuts component desciptor.
	 */
	public ComponentDescriptor(String name, Class componentType)
	{
		super(TargetPart.PATHCOMPONENT);
		setName(name);
		this.componentType = componentType;
	}

	/**
	 * @see abeans.models.meta.ConnectableRealization#getConnectionString()
	 */
	public String getConnectionString() {
		return getName();
	}

	/**
	 * @see abeans.models.meta.ClassRepresentable#getType()
	 */
	public Class getType() {
		return componentType;
	}

	/**
	 * @see abeans.models.meta.NamespaceRealization#isLeaf()
	 */
	public boolean isLeaf() {
		return false;
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
		sb.append("' connectionString = '");
		sb.append(getConnectionString());
		sb.append("' type = '");
		sb.append(getType());
		sb.append("' }");
		return sb.toString();
	}

}
