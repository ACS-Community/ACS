/**
 * 
 */
package alma.TMCDB.alarm;

import com.cosylab.cdb.jdal.hibernate.NameOverrideFeature;

/**
 * @author msekoranja
 *
 */
public class AlarmDefinition implements NameOverrideFeature {

	private String faultFamilyName;
	private String faultMemberName;
	private int faultCode;
	
	
	public AlarmDefinition(String faultFamilyName, String faultMemberName, int faultCode) {
		this.faultFamilyName = faultFamilyName;
		this.faultMemberName = faultMemberName;
		this.faultCode = faultCode;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.NameOverrideFeature#getNameOverride()
	 */
	public String getNameOverride() {
		return "alarm-definition";
	}

	/**
	 * @return the faultFamilyName
	 */
	public String getFaultFamilyName() {
		return faultFamilyName;
	}
	/**
	 * @return the faultMemberName
	 */
	public String getFaultMemberName() {
		return faultMemberName;
	}
	/**
	 * @return the faultCode
	 */
	public int getFaultCode() {
		return faultCode;
	}


}
