package alma.acs.tmcdb.translator;

import java.util.List;
import java.util.Map;

public abstract class AbstractTable2Class {

	protected static final String PACKAGE = "alma.acs.tmcdb.";

	protected Map<String, String> map;
	protected List<String> tablesWithGeneratedKeys;

	public Map<String,String> getMap() { return map; };

	public boolean hasGeneratedKey(String table) {
		return tablesWithGeneratedKeys.contains(table.toLowerCase());
	}

}
