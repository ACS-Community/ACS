package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'AssemblyTypeBEType' CHECK constraint defined for the 'BaseElementType' column of the 'AssemblyType' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum AssemblyTypeBEType {

	ANTENNA("Antenna"),
	PAD("Pad"),
	FRONTEND("FrontEnd"),
	WEATHERSTATIONCONTROLLER("WeatherStationController"),
	CORRQUADRANT("CorrQuadrant"),
	ACACORRSET("AcaCorrSet"),
	CENTRALLO("CentralLO"),
	AOSTIMING("AOSTiming"),
	PHOTONICREFERENCE("PhotonicReference"),
	HOLOGRAPHYTOWER("HolographyTower"),
	ARRAY("Array");

	private String _stringValue;

	AssemblyTypeBEType(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static AssemblyTypeBEType valueOfForEnum(String value) {
		if( value.equals("Antenna") )
			return ANTENNA;
		if( value.equals("Pad") )
			return PAD;
		if( value.equals("FrontEnd") )
			return FRONTEND;
		if( value.equals("WeatherStationController") )
			return WEATHERSTATIONCONTROLLER;
		if( value.equals("CorrQuadrant") )
			return CORRQUADRANT;
		if( value.equals("AcaCorrSet") )
			return ACACORRSET;
		if( value.equals("CentralLO") )
			return CENTRALLO;
		if( value.equals("AOSTiming") )
			return AOSTIMING;
		if( value.equals("PhotonicReference") )
			return PHOTONICREFERENCE;
		if( value.equals("HolographyTower") )
			return HOLOGRAPHYTOWER;
		if( value.equals("Array") )
			return ARRAY;
		else
			throw new RuntimeException("Invalid value for AssemblyTypeBEType enumeration: " + value);
	}

}
