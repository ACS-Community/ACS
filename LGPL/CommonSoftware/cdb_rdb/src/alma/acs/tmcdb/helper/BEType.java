package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'BEType' CHECK constraint defined for the 'BaseType' column of the 'BaseElement' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum BEType {

	ANTENNA("Antenna"),
	PAD("Pad"),
	FRONTEND("FrontEnd"),
	WEATHERSTATIONCONTROLLER("WeatherStationController"),
	CENTRALLO("CentralLO"),
	AOSTIMING("AOSTiming"),
	HOLOGRAPHYTOWER("HolographyTower"),
	PHOTONICREFERENCE("PhotonicReference"),
	CORRQUADRANT("CorrQuadrant"),
	ACACORRSET("AcaCorrSet"),
	CORRQUADRANTRACK("CorrQuadrantRack"),
	CORRSTATIONBIN("CorrStationBin"),
	CORRBIN("CorrBin");

	private String _stringValue;

	BEType(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static BEType valueOfForEnum(String value) {
		if( value.equals("Antenna") )
			return ANTENNA;
		if( value.equals("Pad") )
			return PAD;
		if( value.equals("FrontEnd") )
			return FRONTEND;
		if( value.equals("WeatherStationController") )
			return WEATHERSTATIONCONTROLLER;
		if( value.equals("CentralLO") )
			return CENTRALLO;
		if( value.equals("AOSTiming") )
			return AOSTIMING;
		if( value.equals("HolographyTower") )
			return HOLOGRAPHYTOWER;
		if( value.equals("PhotonicReference") )
			return PHOTONICREFERENCE;
		if( value.equals("CorrQuadrant") )
			return CORRQUADRANT;
		if( value.equals("AcaCorrSet") )
			return ACACORRSET;
		if( value.equals("CorrQuadrantRack") )
			return CORRQUADRANTRACK;
		if( value.equals("CorrStationBin") )
			return CORRSTATIONBIN;
		if( value.equals("CorrBin") )
			return CORRBIN;
		else
			throw new RuntimeException("Invalid value for BEType enumeration: " + value);
	}

}
