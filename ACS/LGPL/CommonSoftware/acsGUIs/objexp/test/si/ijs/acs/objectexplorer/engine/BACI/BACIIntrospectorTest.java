package si.ijs.acs.objectexplorer.engine.BACI;

import si.ijs.acs.objectexplorer.engine.IntrospectionInconsistentException;

import junit.framework.TestCase;

/**
 * Ad-hoc test for a particular problem just found, when objexp had no junit tests at all.
 * Many more tests should be added, also for different classes. 
 */
public class BACIIntrospectorTest extends TestCase
{

	public void testFullTypeToType() {
		// the good case
		assertEquals("SourceCatalogue", BACIIntrospector.fullTypeToType("IDL:alma/archive/SourceCatalogue:1.0"));

		try {
			BACIIntrospector.fullTypeToType("");
			fail("IntrospectionInconsistentException expected");
		}
		catch (IntrospectionInconsistentException ex) {
			// expected
		}
		
		try {
			// test missing ":1.0" in IDL type
			BACIIntrospector.fullTypeToType("IDL:alma/archive/SourceCatalogue");
			fail("IntrospectionInconsistentException expected");
		}
		catch (IntrospectionInconsistentException ex) {
			// expected
		}
	}
}
