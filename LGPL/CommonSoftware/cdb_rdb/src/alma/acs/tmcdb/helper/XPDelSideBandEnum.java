package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'XPDelSideBandEnum' CHECK constraint defined for the 'SideBand' column of the 'XPDelay' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum XPDelSideBandEnum {

	LSB("LSB"),
	USB("USB");

	private String _stringValue;

	XPDelSideBandEnum(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static XPDelSideBandEnum valueOfForEnum(String value) {
		if( value.equals("LSB") )
			return LSB;
		if( value.equals("USB") )
			return USB;
		else
			throw new RuntimeException("Invalid value for XPDelSideBandEnum enumeration: " + value);
	}

}
