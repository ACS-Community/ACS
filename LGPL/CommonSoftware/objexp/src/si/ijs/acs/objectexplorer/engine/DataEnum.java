package si.ijs.acs.objectexplorer.engine;

import java.util.HashMap;
import java.util.Set;
import java.util.LinkedHashSet;

public class DataEnum implements DataElement {
	private HashMap<Integer, String> types;
	private int value = 0;
	private String id;
	private String name;
	private LinkedHashSet<String> keys;
	public DataEnum(String id) {
		this.id = id;
		name = id.substring(id.lastIndexOf("/")+1,id.lastIndexOf(":"));
		types = new HashMap<Integer, String>();
		keys = new LinkedHashSet<String>();
	}
	public void add(int value, String type) {
		if(types.get(value) != null)
			types.remove(value);
		if(keys.contains(type))
			keys.remove(type);
		types.put(value,type);
		keys.add(type);
	}
	public void remove(int value) {
		String type = types.get(value);
		if(type != null)
			types.remove(value);
		if(keys.contains(type))
			keys.remove(type);
	}
	public String get(int value) {
		return types.get(value);
	}
	public int size() {
		return types.size();
	}
	public String toString() {
		return get(value);
	}
	public void set(int value) {
		this.value = value;
	}
	public int get() {
		return value;
	}
	public String name() {
		return name;
	}
	public String id() {
		return id;
	}
	public Set<String> keySet() {
		return keys;
	}
	public String toString(String start, int level, boolean expand) {
		StringBuffer result = new StringBuffer(500);
		result.append(get(value));
		if (expand) {
			result.append('\n');
			result.append(start);
			result.append("  (" + name + ")");
			result.append('\n');
			result.append(start);
			result.append("  |value");
			result.append(": ");
			result.append(value);
		}
		return result.toString();
	}
}
