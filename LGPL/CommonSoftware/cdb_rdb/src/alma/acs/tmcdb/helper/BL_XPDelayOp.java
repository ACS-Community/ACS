package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'BL_XPDelayOp' CHECK constraint defined for the 'Operation' column of the 'BL_XPDelay' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum BL_XPDelayOp {

	I("I"),
	U("U"),
	D("D");

	private String _stringValue;

	BL_XPDelayOp(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static BL_XPDelayOp valueOfForEnum(String value) {
		if( value.equals("I") )
			return I;
		if( value.equals("U") )
			return U;
		if( value.equals("D") )
			return D;
		else
			throw new RuntimeException("Invalid value for BL_XPDelayOp enumeration: " + value);
	}

}
