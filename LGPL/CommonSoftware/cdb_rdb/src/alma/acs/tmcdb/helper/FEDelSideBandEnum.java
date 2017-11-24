package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'FEDelSideBandEnum' CHECK constraint defined for the 'SideBand' column of the 'FEDelay' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum FEDelSideBandEnum {

	LSB("LSB"),
	USB("USB");

	private String _stringValue;

	FEDelSideBandEnum(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static FEDelSideBandEnum valueOfForEnum(String value) {
		if( value.equals("LSB") )
			return LSB;
		if( value.equals("USB") )
			return USB;
		else
			throw new RuntimeException("Invalid value for FEDelSideBandEnum enumeration: " + value);
	}

}
