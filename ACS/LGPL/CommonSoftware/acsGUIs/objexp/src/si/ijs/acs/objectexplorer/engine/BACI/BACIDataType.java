package si.ijs.acs.objectexplorer.engine.BACI;

import si.ijs.acs.objectexplorer.engine.DataType;
import si.ijs.acs.objectexplorer.engine.DataElement;

public class BACIDataType implements DataType {
	private Class type;
	private DataElement el = null;
	private ArrayTypes arrayType = ArrayTypes.NOT;
	private int arrayLength = -1;
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
	public void setArrayType(String type) {
		if(type == null) {
			arrayType = ArrayTypes.NOT;
			arrayLength = -1; //Not an array.
		} else if(type.equals("U")) {
			arrayType = ArrayTypes.USEQ;
			arrayLength = 0; //Unbounded sequence.
		} else if(type.matches("B\\d+")) {
			arrayType = ArrayTypes.BSEQ;
			arrayLength = Integer.parseInt(type.substring(1)); //Bounded sequence
		} else if(type.matches("A\\d+")) {
			arrayType = ArrayTypes.ARRAY;
			arrayLength = Integer.parseInt(type.substring(1)); //Array
		} else
			System.out.println("Incorrect arrayType: " + arrayType);
	}
	public ArrayTypes getArrayType() {
		return arrayType;
	}
	public int getArrayLength() {
		return arrayLength;
	}
}
