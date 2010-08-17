package si.ijs.acs.objectexplorer.engine;

public interface DataElement {
	public String toString(String start, int level, boolean expand);
	public String id();
	public String name();
}
