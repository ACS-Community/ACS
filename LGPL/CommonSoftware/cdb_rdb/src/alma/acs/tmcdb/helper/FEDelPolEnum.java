package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'FEDelPolEnum' CHECK constraint defined for the 'Polarization' column of the 'FEDelay' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum FEDelPolEnum {

	X("X"),
	Y("Y");

	private String _stringValue;

	FEDelPolEnum(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static FEDelPolEnum valueOfForEnum(String value) {
		if( value.equals("X") )
			return X;
		if( value.equals("Y") )
			return Y;
		else
			throw new RuntimeException("Invalid value for FEDelPolEnum enumeration: " + value);
	}

}
