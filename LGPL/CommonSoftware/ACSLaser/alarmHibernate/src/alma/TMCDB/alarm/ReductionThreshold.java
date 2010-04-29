/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;

import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 *
 */
public class ReductionThreshold implements NameOverrideFeature {
	
	private int value;
	
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
	public Map<String, Object> _ = new InternalElementsMap<String, Object>();

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "threshold";
	}
	
	public ReductionThreshold(int value, AlarmDefinition alarm)
	{
		this.value = value;
		_.put("alarm-definition", alarm);
	}

	/**
	 * @return the value
	 */
	public int getValue() {
		return value;
	}


}
