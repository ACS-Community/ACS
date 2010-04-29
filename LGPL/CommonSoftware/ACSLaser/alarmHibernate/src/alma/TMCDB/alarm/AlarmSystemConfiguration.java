/**
 * 
 */
package alma.TMCDB.alarm;

import com.cosylab.cdb.jdal.hibernate.ElementValueFeature;
import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 *
 */
public class AlarmSystemConfiguration implements ElementValueFeature,
		NameOverrideFeature {

	public String getElementValue() {
		return "<configuration-property name=\"Implementation\">CERN</configuration-property>";
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "alarm-system-configuration";
	}

}
