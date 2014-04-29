package si.ijs.acs.objectexplorer.engine;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Set;

import alma.acs.gui.util.DataFormatter;
import alma.acs.util.UTCUtility;

public class DataStruct implements DataElement {
	private String name;
	private String id;
	private HashMap<String, Object> members;
	private LinkedHashSet<String> keys;
	private static final SimpleDateFormat df = new SimpleDateFormat("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS");
	public DataStruct(String id) {
		this.id = id;
		name = id.substring(id.lastIndexOf("/")+1,id.lastIndexOf(":"));
		members = new HashMap<String, Object>();
		keys = new LinkedHashSet<String>();
	}
	public void add(String name, Object value) {
		if(members.get(name) != null)
			members.remove(name);
		if(keys.contains(name))
			keys.remove(name);
		members.put(name,value);
		keys.add(name);
	}
	public void remove(String name) {
		if(members.get(name) != null)
			members.remove(name);
		if(keys.contains(name))
			keys.remove(name);
	}
	public Object get(String name) {
		return members.get(name);
	}
	public int size() {
		return members.size();
	}
	public String name() {
		return name;
	}
	public Set<String> keySet() {
		return keys;
	}
	public String id() {
		return id;
	}
	public String toString() {
		return name+": "+id;
	}
	public String toString(String start, int level, boolean expand) {
		StringBuffer result = new StringBuffer(500);
		result.append(id());
		if (expand) {
			result.append('\n');
			result.append(start);
			result.append("  (" + name + ")");
			for(String key: keys) {
				result.append('\n');
				result.append(start);
				result.append(key);
				result.append(": ");
				Object value = get(key);
				if (value != null && value.getClass().isArray()) {
					result.append(DataElementFormatter.unpackArray(value,start,level, expand));
				} else {
					if(value instanceof DataElement)
						result.append(((DataElement)value).toString(start + " ", level + 1, expand));
					else {
						// msekoran - ACS timestamp support (this is really not clean solution)
						if (key.equals("timeStamp") && id.equals("IDL:alma/ACSErr/Completion:1.0")) {
							long javaTime = UTCUtility.utcOmgToJava(((Long)value).longValue()); 
							result.append(df.format(new Date(javaTime)));
						}
						else
							result.append(DataFormatter.unpackReturnValue(value, start + " ", level + 1, expand));
					}
				}
			}
		}
		return result.toString();
	}
}
