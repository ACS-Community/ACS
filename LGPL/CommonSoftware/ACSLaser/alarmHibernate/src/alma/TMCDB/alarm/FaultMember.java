/**
 * 
 */
package alma.TMCDB.alarm;

import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 *
 */
public class FaultMember extends FaultMemberDefault implements
		NameOverrideFeature {

	private String name;
	
	/**
	 * @param location
	 */
	public FaultMember(String name, Location location) {
		super(location);
		this.name = name;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "fault-member";
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

}
