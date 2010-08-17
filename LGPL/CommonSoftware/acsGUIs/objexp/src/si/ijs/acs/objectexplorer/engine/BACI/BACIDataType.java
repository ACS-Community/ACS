package si.ijs.acs.objectexplorer.engine.BACI;

import si.ijs.acs.objectexplorer.engine.DataType;
import si.ijs.acs.objectexplorer.engine.DataElement;

public class BACIDataType implements DataType {
	private Class type;
	private DataElement el = null;
	public BACIDataType(Class type) {
		this.type = type;
	}
	public boolean isPrimitive() {
		return type.isPrimitive();
	}
	public boolean isArray() {
		return type.isArray();
	}
	public DataType getComponentType() {
		return new BACIDataType(type.getComponentType());
	}
	public String getName() {
		return type.getName();
	}
	public String toString() {
		return type.toString();
	}
	public boolean isInterface() {
		return type.isInterface();
	}
	public Class getType() {
		return type;
	}
	public DataElement getElement() {
		return el;
	}
	public void setElement(DataElement el) {
		this.el = el;
	}
}
