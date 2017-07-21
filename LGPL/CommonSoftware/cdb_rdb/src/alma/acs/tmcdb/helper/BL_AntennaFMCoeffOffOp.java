package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'BL_AntennaFMCoeffOffOp' CHECK constraint defined for the 'Operation' column of the 'BL_FocusModelCoeffOffset' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum BL_AntennaFMCoeffOffOp {

	I("I"),
	U("U"),
	D("D");

	private String _stringValue;

	BL_AntennaFMCoeffOffOp(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static BL_AntennaFMCoeffOffOp valueOfForEnum(String value) {
		if( value.equals("I") )
			return I;
		if( value.equals("U") )
			return U;
		if( value.equals("D") )
			return D;
		else
			throw new RuntimeException("Invalid value for BL_AntennaFMCoeffOffOp enumeration: " + value);
	}

}
