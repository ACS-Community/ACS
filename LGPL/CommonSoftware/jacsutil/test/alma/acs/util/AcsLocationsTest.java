/*
 * Created on Jan 3, 2005 by mschilli
 */
package alma.acs.util;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;



/**
 *
 * @author mschilli
 */
public class AcsLocationsTest extends TestCase {

	public void testSuccessfulConversions() {
		
		String loc;
		String[] hostport;
		String host;
		String port;

		// --- mgr
		loc = "corbaloc::localhost:3000/Manager";
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(AcsLocations.convertToManagerLocation(host, port), loc);

		loc = "corbaloc:seciop:localhost:3000/Manager";
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(AcsLocations.convertToManagerLocation(host, port, "seciop"), loc);
		
		
		// --- cdb
		loc = "corbaloc::localhost:3012/CDB";
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(AcsLocations.convertToCdbLocation(host, port), loc);

		loc = "corbaloc:seciop:localhost:3012/CDB";
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(AcsLocations.convertToCdbLocation(host, port, "seciop"), loc);

		// --- ir
		loc = "corbaloc::localhost:3004/InterfaceRepository";
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(AcsLocations.convertToInterfaceRepositoryLocation(host, port), loc);

		loc = "corbaloc:seciop:localhost:3004/InterfaceRepository";
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(AcsLocations.convertToInterfaceRepositoryLocation(host, port, "seciop"), loc);

		// --- ns
		loc = "corbaloc::localhost:3001/NameService";
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(AcsLocations.convertToNameServiceLocation(host, port), loc);

		loc = "corbaloc:seciop:localhost:3001/NameService";
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(AcsLocations.convertToNameServiceLocation(host, port, "seciop"), loc);
		
		// --- IOR parsing
		loc = IOR_1;
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(host, "192.168.1.3");
		assertEquals(port, "1120");
		
		loc = IOR_2;
		hostport = AcsLocations.convert(loc);
		host = hostport[0];
		port = hostport[1];
		assertEquals(host, "134.171.12.179");
		assertEquals(port, "3500");
	}

	
	public void testErroneousConversions() {
		
		String bad1 = ":localhost:3000/Manager";
		String bad2 = "corbaloc:localhost:3000/Manager";
		String bad3 = "corbaloc::localhost/3004/Manager";
		String bad4 = "corbaloc://localhost:3001/Manager";
		String bad5 = "X"+IOR_1;
		String bad6 = IOR_1.substring(0, 10) + IOR_1.substring(11);
		
		String[] hostport;
		String host;
		String port;

		// --- bad1
		try {
			hostport = AcsLocations.convert(bad1);
			throw new AssertionFailedError("convert() didn't detect erroneous corbaloc: "+bad1);
		} catch (IllegalArgumentException exc) {}
		
		// --- bad2
		try {
			hostport = AcsLocations.convert(bad2);
			throw new AssertionFailedError("convert() didn't detect erroneous corbaloc: "+bad2);
		} catch (IllegalArgumentException exc) {}

		// --- bad3
		try {
			hostport = AcsLocations.convert(bad3);
			throw new AssertionFailedError("convert() didn't detect erroneous corbaloc: "+bad3);
		} catch (IllegalArgumentException exc) {}

		// --- bad4
		try {
			hostport = AcsLocations.convert(bad4);
			throw new AssertionFailedError("convert() didn't detect erroneous corbaloc: "+bad4);
		} catch (IllegalArgumentException exc) {}

		// --- bad5
		try {
			hostport = AcsLocations.convert(bad5);
			throw new AssertionFailedError("convert() didn't detect erroneous corbaloc: "+bad5);
		} catch (IllegalArgumentException exc) {}

		// --- bad6
		try {
			hostport = AcsLocations.convert(bad6);
			throw new AssertionFailedError("convert() didn't detect erroneous corbaloc: "+bad6);
		} catch (IllegalArgumentException exc) {}
		
	}
	

	public void testFigureOutManagerLocation() {
		
		String guess;

		System.getProperties().remove("ACS.baseport");
		System.getProperties().remove("ACS.managerhost");
		System.getProperties().remove("ACS.manager");
		
		// baseport    : unset
		// manager     : unset
		// managerhost : unset
		guess = AcsLocations.figureOutManagerLocation();
		assertEquals(guess, "corbaloc::"+AcsLocations.getLocalIP()+":3000/Manager");
		
		
		// baseport    : set
		// manager     : unset
		// managerhost : unset
		System.setProperty("ACS.baseport", "3");
		guess = AcsLocations.figureOutManagerLocation();
		assertEquals(guess, "corbaloc::"+AcsLocations.getLocalIP()+":3300/Manager");

		
		// baseport    : set
		// manager     : set
		// managerhost : unset
		System.setProperty("ACS.manager", "corbaloc::456.456.456.456:3500/Manager");
		guess = AcsLocations.figureOutManagerLocation();
		assertEquals(guess, "corbaloc::456.456.456.456:3500/Manager");

		
		// baseport   : set
		// manager    : set
		// managerhost: set
		System.setProperty("ACS.managerhost", "123.123.123.123");
		guess = AcsLocations.figureOutManagerLocation();
		assertEquals(guess, "corbaloc::123.123.123.123:3300/Manager");

	}
	
	
	private final String IOR_1 = "IOR:012020202100000049444c3a434f5242416e65742f526f6f6d496e666f726d6174696f6e3a312e3000202020020000000153495670000000010101200500000073756e6700202020d7000000010000004e00000001504d43000000002100000049444c3a434f5242416e65742f526f6f6d496e666f726d6174696f6e3a312e30002020201a000000564953494f5242202d20494f4e414f52422054455354494e4700202000000000000000006a000000010100200c0000003139322e3136382e312e3300600420204e00000001504d43000000002100000049444c3a434f5242416e65742f526f6f6d496e666f726d6174696f6e3a312e30002020201a000000564953494f5242202d20494f4e414f52422054455354494e4700";
	private final String IOR_2 = "IOR:000000000000001C49444C3A696A732E73692F6D6163692F4D616E616765723A312E3000000000020000000000000080000102000000000F3133342E3137312E31322E31373900000DAC0000000000164F52422F4D616E61676572504F412F4D616E616765720000000000030000000000000008000000004A414300000000010000001C000000000001000100000001050100010001010900000001050100010000001400000008000000E000609E3A000000010000002C0000000000000001000000010000001C00000000000100010000000105010001000101090000000105010001";
	
}

//
//
//
//
//
//
//
//
//
//
//
//