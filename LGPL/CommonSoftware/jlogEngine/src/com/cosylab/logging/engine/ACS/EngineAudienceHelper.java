package com.cosylab.logging.engine.ACS;

/**
 * The definition of the audience used by the engine to filter out
 * logs.
 * This helper class encapsulates the audience as defined in
 * the <code>log_audience</code> IDL module.
 * 
 * @author acaproni
 *
 */
public enum EngineAudienceHelper {
	
	OPERATOR(alma.log_audience.OPERATOR.value),
    DEVELOPER(alma.log_audience.DEVELOPER.value),
    SYSADMIN(alma.log_audience.SYSADMIN.value),
    DBA(alma.log_audience.DBA.value),
    NO_AUDIENCE(alma.log_audience.NO_AUDIENCE.value);
	
    // The value of the string representing the mode
	public final String val;
	
	/**
	 * Constructor.
	 * 
	 * @param str The string describing the audience,
	 *            read from the IDL definition.
	 */
	private EngineAudienceHelper(String str) {
		this.val=str;
	}
	
	/**
	 * Return the audience corresponding to the passed description.
	 * The description passed as parameter must be equal to the 
	 * definition in the IDL.
	 * 
	 * @param audience The description of the audience
	 * @return the audience corresponding to the passed description
	 *         <code>null</code> if <code>audience</code>  does not correspond to
	 *                           any valid EngineAudienceHelper
	 */
	public static EngineAudienceHelper fromString(String audience) {
		for (EngineAudienceHelper engineAudience: EngineAudienceHelper.values()) {
			if (audience.equals(engineAudience.val)) {
				return engineAudience;
			}
		}
		return null;
	}
}
