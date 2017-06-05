package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'BL_AntennaFMCoeffOffBand' CHECK constraint defined for the 'ReceiverBand' column of the 'BL_FocusModelCoeffOffset' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum BL_AntennaFMCoeffOffBand {

	ALMA_RB_01("ALMA_RB_01"),
	ALMA_RB_02("ALMA_RB_02"),
	ALMA_RB_03("ALMA_RB_03"),
	ALMA_RB_04("ALMA_RB_04"),
	ALMA_RB_05("ALMA_RB_05"),
	ALMA_RB_06("ALMA_RB_06"),
	ALMA_RB_07("ALMA_RB_07"),
	ALMA_RB_08("ALMA_RB_08"),
	ALMA_RB_09("ALMA_RB_09"),
	ALMA_RB_10("ALMA_RB_10");

	private String _stringValue;

	BL_AntennaFMCoeffOffBand(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static BL_AntennaFMCoeffOffBand valueOfForEnum(String value) {
		if( value.equals("ALMA_RB_01") )
			return ALMA_RB_01;
		if( value.equals("ALMA_RB_02") )
			return ALMA_RB_02;
		if( value.equals("ALMA_RB_03") )
			return ALMA_RB_03;
		if( value.equals("ALMA_RB_04") )
			return ALMA_RB_04;
		if( value.equals("ALMA_RB_05") )
			return ALMA_RB_05;
		if( value.equals("ALMA_RB_06") )
			return ALMA_RB_06;
		if( value.equals("ALMA_RB_07") )
			return ALMA_RB_07;
		if( value.equals("ALMA_RB_08") )
			return ALMA_RB_08;
		if( value.equals("ALMA_RB_09") )
			return ALMA_RB_09;
		if( value.equals("ALMA_RB_10") )
			return ALMA_RB_10;
		else
			throw new RuntimeException("Invalid value for BL_AntennaFMCoeffOffBand enumeration: " + value);
	}

}
