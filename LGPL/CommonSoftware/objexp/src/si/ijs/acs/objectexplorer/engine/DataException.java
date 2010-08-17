package si.ijs.acs.objectexplorer.engine;

import com.cosylab.gui.components.r2.DataFormatter;

import java.util.HashMap;
import java.util.Set;
import java.util.LinkedHashSet;

public class DataException extends org.omg.CORBA.UserException implements DataElement {
	private String name;
	private String id;
	private HashMap<String, Object> members;
	private LinkedHashSet<String> keys;
	public DataException(String id) {
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
	public String toString(String start, int level, boolean expand) {
		StringBuffer result = new StringBuffer(500);
		//result.append(toString());
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
				// msekoran - ACS timestamp support (this is really not clean solution)
				//if (curField.getName().equals("timeStamp") && curField.getDeclaringClass().getName().equals("alma.ACSErr.Completion"))
				//{
				//	long javaTime = UTCUtility.utcOmgToJava(curField.getLong(value)); 
				//	result.append(df.format(new Date(javaTime)));
				//}
				//else
				Object value = get(key);
				if(value instanceof DataElement)
					result.append(((DataElement)value).toString(start + " ", level + 1, expand));
				else
					result.append(DataFormatter.unpackReturnValue(value, start + " ", level + 1, expand));
			}
		}
		return result.toString();
	}
}
