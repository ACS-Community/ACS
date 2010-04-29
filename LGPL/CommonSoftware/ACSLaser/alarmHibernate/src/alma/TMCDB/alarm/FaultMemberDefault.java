package alma.TMCDB.alarm;

import java.util.Map;

import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;

public class FaultMemberDefault {
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
	public Map<String, Object> _ = new InternalElementsMap<String, Object>();

    public FaultMemberDefault(Location location)
    {
    	if (location != null)
    		_.put("location", location);
    }
}
