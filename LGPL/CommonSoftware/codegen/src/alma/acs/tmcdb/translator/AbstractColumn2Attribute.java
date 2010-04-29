package alma.acs.tmcdb.translator;

import java.util.Map;

public abstract class AbstractColumn2Attribute {

	protected Map<String, Map<String,String>> map;

	public Map<String, Map<String,String>> getMap() { return map; };

}