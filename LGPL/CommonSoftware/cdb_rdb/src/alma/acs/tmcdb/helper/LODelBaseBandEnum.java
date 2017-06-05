package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'LODelBaseBandEnum' CHECK constraint defined for the 'BaseBand' column of the 'LODelay' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum LODelBaseBandEnum {

	BB_1("BB_1"),
	BB_2("BB_2"),
	BB_3("BB_3"),
	BB_4("BB_4");

	private String _stringValue;

	LODelBaseBandEnum(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static LODelBaseBandEnum valueOfForEnum(String value) {
		if( value.equals("BB_1") )
			return BB_1;
		if( value.equals("BB_2") )
			return BB_2;
		if( value.equals("BB_3") )
			return BB_3;
		if( value.equals("BB_4") )
			return BB_4;
		else
			throw new RuntimeException("Invalid value for LODelBaseBandEnum enumeration: " + value);
	}

}
