package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'AntennaType' CHECK constraint defined for the 'AntennaType' column of the 'Antenna' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum AntennaType {

	VA("VA"),
	AEC("AEC"),
	ACA("ACA");

	private String _stringValue;

	AntennaType(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static AntennaType valueOfForEnum(String value) {
		if( value.equals("VA") )
			return VA;
		if( value.equals("AEC") )
			return AEC;
		if( value.equals("ACA") )
			return ACA;
		else
			throw new RuntimeException("Invalid value for AntennaType enumeration: " + value);
	}

}
