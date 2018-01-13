package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'BL_AntennaPMCoeffOffOp' CHECK constraint defined for the 'Operation' column of the 'BL_PointingModelCoeffOffset' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum BL_AntennaPMCoeffOffOp {

	I("I"),
	U("U"),
	D("D");

	private String _stringValue;

	BL_AntennaPMCoeffOffOp(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static BL_AntennaPMCoeffOffOp valueOfForEnum(String value) {
		if( value.equals("I") )
			return I;
		if( value.equals("U") )
			return U;
		if( value.equals("D") )
			return D;
		else
			throw new RuntimeException("Invalid value for BL_AntennaPMCoeffOffOp enumeration: " + value);
	}

}
