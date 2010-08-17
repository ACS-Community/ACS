package si.ijs.acs.objectexplorer.engine;

public interface DataType {
	public boolean isPrimitive();
	public boolean isArray();
	public DataType getComponentType();
	public String getName();
	public boolean isInterface();
	public Class getType();
	public DataElement getElement();
	public void setElement(DataElement el);
}
