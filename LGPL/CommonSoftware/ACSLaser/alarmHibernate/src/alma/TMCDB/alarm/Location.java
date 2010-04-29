/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;

import com.cosylab.cdb.jdal.hibernate.ElementValue;
import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;

/**
 * @author msekoranja
 */
public class Location {

	// hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
 	public Map<String, Object> _ = new InternalElementsMap<String, Object>();
	
	public Location(String building, String floor, String room, String mnemonic, String position) {
		if (building != null)
			_.put("building", new ElementValue(building));
		if (floor != null)
			_.put("floor", new ElementValue(floor));
		if (room != null)
			_.put("room", new ElementValue(room));
		if (mnemonic != null)
			_.put("mnemonic", new ElementValue(mnemonic));
		if (position != null)
			_.put("position", new ElementValue(position));
	}
	
}
