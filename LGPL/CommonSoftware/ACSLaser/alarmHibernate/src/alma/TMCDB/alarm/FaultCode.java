/**
 * 
 */
package alma.TMCDB.alarm;

import java.util.Map;

import com.cosylab.cdb.jdal.hibernate.ElementValue;
import com.cosylab.cdb.jdal.hibernate.InternalElementsMap;
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 */
public class FaultCode implements NameOverrideFeature {

	private int value;
	private boolean instant;
	
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    // since childred can be properties and subcomponent Object is used
	public Map<String, Object> _ = new InternalElementsMap<String, Object>();

    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "fault-code";
	}

	public FaultCode(int value, boolean instant,
			int priority, String cause, String action, String consequence,
			String problemDescription) {
		this.value = value;
		this.instant = instant;
		
		_.put("priority", new ElementValue(String.valueOf(priority)));
		if (cause != null)
			_.put("cause", new ElementValue(cause));
		if (action != null)
			_.put("action", new ElementValue(action));
		if (consequence != null)
			_.put("consequence", new ElementValue(consequence));
		_.put("problem-description", new ElementValue(problemDescription));
	}

	/**
	 * @return the instant
	 */
	public boolean isInstant() {
		return instant;
	}

	/**
	 * @return the value
	 */
	public int getValue() {
		return value;
	}

}
