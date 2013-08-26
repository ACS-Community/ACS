package alma.acs.container.corba;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.LogRecord;

import junit.framework.TestCase;

import org.apache.avalon.framework.configuration.DefaultConfiguration;
import org.jacorb.orb.BufferManager;
import org.jacorb.orb.giop.ServiceContextTransportingOutputStream;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.portable.OutputStream;

import alma.acs.container.ContainerSealant;
import alma.acs.testsupport.LogRecordCollectingLogger;
import alma.jconttest.ComponentWithBadNullsOperations;
import alma.jconttest.ComponentWithBadNullsImpl.ComponentWithBadNullsImpl;
import alma.jconttest.ComponentWithBadNullsPackage.Enum1;
import alma.jconttest.ComponentWithBadNullsPackage.Struct1;
import alma.jconttest.ComponentWithBadNullsPackage.Struct1Holder;
import alma.jconttest.ComponentWithBadNullsPackage.Struct2;
import alma.jconttest.ComponentWithBadNullsPackage.Struct2Helper;


/**
 * Tests CorbaNullFinder. See COMP-4592 and COMP-6091.
 * @author hsommer
 */
public class CorbaNullFinderTest extends TestCase
{
	/**
	 * Tests jacorb's reaction to null data inside structs.
	 * If this test fails after a jacorb update, the logic of the Null Finder must be revisited.
	 */
	public void testJacorbNullBehavior() throws Exception {
		// Jacorb uses ReplyOutputStream, but its base class ServiceContextTransportingOutputStream is easier to construct
		// and should be similar enough for this test.
		BufferManager.configure(new DefaultConfiguration("dummy")); // otherwise jacorb exception later if unconfigured
		OutputStream out = new ServiceContextTransportingOutputStream(); 
		
		Struct2 myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		// the good data should marshal without exception
		Struct2Helper.write(out, myStruct2);
		
		// null string
		try {
			myStruct2.mystruct1.mystring = null;
			Struct2Helper.write(out, myStruct2);
			fail("null strings in structs should marshal with exception.");
		}
		catch (MARSHAL ex) {
			// expected
			assertEquals("org.omg.CORBA.MARSHAL: Null References  vmcid: 0x0  minor code: 0  completed: No", ex.toString());
		}
			
		// null enum
		myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		try {
			myStruct2.mystruct1.myenum1 = null;
			Struct2Helper.write(out, myStruct2);
			fail("null strings in structs should marshal with NPE.");
		}
		catch (NullPointerException ex) {
			// expected... this is a really mean case, because we get NPE instead of MARSHAL. Maybe a jacorb bug?
		}
		
		// null struct
		myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		try {
			myStruct2.mystruct1 = null;
			Struct2Helper.write(out, myStruct2);
			fail("null structs inside structs should marshal with NPE.");
		}
		catch (NullPointerException ex) {
			// expected... this is a really mean case, because we get NPE instead of MARSHAL. Maybe a jacorb bug?
		}
		// top-level struct itself is null
		try {
			Struct2Helper.write(out, null);
			fail("top-level null structs should marshal with NPE.");
		}
		catch (NullPointerException ex) {
			// expected... 
		}
		
		// null sequence of structs
		myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		try {
			myStruct2.seqOfStruct1 = null;
			Struct2Helper.write(out, myStruct2);
			fail("null sequence of structs inside structs should marshal with NPE.");
		}
		catch (NullPointerException ex) {
			// expected... this is a really mean case, because we get NPE instead of MARSHAL. Maybe a jacorb bug?
		}

		// sequence with null struct
		myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		try {
			myStruct2.seqOfStruct1 = new Struct1[1]; // with null inside
			Struct2Helper.write(out, myStruct2);
			fail("sequence of structs with nulls should marshal with NPE.");
		}
		catch (NullPointerException ex) {
			// expected... this is a really mean case, because we get NPE instead of MARSHAL. Maybe a jacorb bug?
		}
	}
	
	/**
	 * Tests how java classes for IDL interfaces, structs, and enums are distinguished.
	 */
	public void testIdlTypeInfering() {
		assertTrue(CorbaNullFinder.isIDLEnumClass(Enum1.class));
		assertFalse(CorbaNullFinder.isIDLEnumClass(Struct1.class));
		assertTrue(CorbaNullFinder.isIDLStructClass(Struct1.class));
		assertFalse(CorbaNullFinder.isIDLStructClass(Enum1.class));
		
		assertTrue(CorbaNullFinder.isIDLInterfaceClass(alma.ACS.Property.class));
		assertFalse(CorbaNullFinder.isIDLStructClass(alma.ACS.Property.class));
		assertFalse(CorbaNullFinder.isIDLEnumClass(alma.ACS.Property.class));
	}
	
	
	/**
	 * Tests the errors found and reported by CorbaNullFinder
	 */
	public void testNullFinder() {
		List<String> expected = new ArrayList<String>();
		
		Struct2 myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		
		// all fine
		CorbaNullFinder finder = new CorbaNullFinder(myStruct2);
		assertFalse(finder.hasErrors());
		assertNullFinderErrors(expected, finder.getErrors());
		
		// struct missing
		myStruct2.mystruct1 = null;
		finder = new CorbaNullFinder(myStruct2);
		assertTrue(finder.hasErrors());
		expected.add("Null struct in field Struct2/mystruct1");
		assertNullFinderErrors(expected, finder.getErrors());

		// string and enum in struct missing
		myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		myStruct2.mystruct1.mystring = null;
		myStruct2.mystruct1.myenum1 = null;
		finder = new CorbaNullFinder(myStruct2);
		assertTrue(finder.hasErrors());
		expected.clear();
		expected.add("Null string in field Struct2/mystruct1/mystring");
		expected.add("Null enum in field Struct2/mystruct1/myenum1");
		expected.add("Null string in field Struct2/seqOfStruct1[0]/mystring"); // mystruct1 is also part of the sequence
		expected.add("Null enum in field Struct2/seqOfStruct1[0]/myenum1");
		assertNullFinderErrors(expected, finder.getErrors());
		
		// null sequence of structs
		myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		myStruct2.seqOfStruct1 = null;
		finder = new CorbaNullFinder(myStruct2);
		assertTrue(finder.hasErrors());
		expected.clear();
		expected.add("Null array in field Struct2/seqOfStruct1");
		assertNullFinderErrors(expected, finder.getErrors());
		
		// sequence with null struct
		myStruct2 = ComponentWithBadNullsImpl.createGoodStruct2();
		myStruct2.seqOfStruct1[0] = null;
		finder = new CorbaNullFinder(myStruct2);
		assertTrue(finder.hasErrors());
		expected.clear();
		expected.add("Null object in field Struct2/seqOfStruct1[0]");
		assertNullFinderErrors(expected, finder.getErrors());
		
		// Arrays outside of structs, with good values or with null values (see COMP-6091)
		finder = new CorbaNullFinder(new String[] {"goodString"});
		assertFalse(finder.hasErrors());
		finder = new CorbaNullFinder(new String[] {"goodStringAtFirst", null, "anotherGoodString"});
		assertTrue(finder.hasErrors());
		expected.clear();
		expected.add("Null object in field String[1]");
		assertNullFinderErrors(expected, finder.getErrors());
		finder = new CorbaNullFinder(new int[] {1,2}); // CorbaNullFinder will see them as Integer objects.
		assertFalse(finder.hasErrors());

		// main object null
		finder = new CorbaNullFinder(null);
		assertTrue(finder.hasErrors());
		expected.clear();
		expected.add("Top-level object is null; cannot distinguish between a legal null object reference and an illegal null data item.");
		assertNullFinderErrors(expected, finder.getErrors());
	}
	
	private void assertNullFinderErrors(List<String> expected, List<String> actual) {
		assertEquals("Number of errors found by CorbaNullFinder", expected.size(), actual.size());
		for (int i = 0; i < expected.size(); i++) {
			String error1 = expected.get(i);
			String error2 = actual.get(i);
			assertEquals("Error message", error1, error2);
		}
	}
	
	/**
	 * Strictly speaking this is a test for {@link ContainerSealant}, but practically fits well in here.
	 */
	public void testNullFinderInContainerSealant() throws Exception {
		// activate the null checker
		System.setProperty(ContainerSealant.CHECK_NULLS_CORBA_OUT_PROPERTYNAME, "true");
		
		// The test component, with the real interceptor around it
		LogRecordCollectingLogger collectingLogger = LogRecordCollectingLogger.getCollectingLogger("Collecting_" + getName());
		ComponentWithBadNullsImpl compImpl = new ComponentWithBadNullsImpl();
		ComponentWithBadNullsOperations sealedComponent = 
			ContainerSealant.createContainerSealant(
					ComponentWithBadNullsOperations.class, 
					compImpl, 
					"InstanceForUnitTest", false, collectingLogger, null, null
			);
		
		// invocation of methodWithReturnData
		String instring = null;
		Struct1 instruct1 = ComponentWithBadNullsImpl.createGoodStruct1();
		instruct1.myenum1 = null;
		instruct1.mystring = null;
		StringHolder inoutstringHolder = new StringHolder();
		Struct1Holder inoutstruct1Holder = new Struct1Holder();
		StringHolder outstringHolder = new StringHolder();
		Struct1Holder outstruct1Holder = new Struct1Holder();
		sealedComponent.methodWithReturnData(instring, instruct1, 
				inoutstringHolder, inoutstruct1Holder, 
				outstringHolder, outstruct1Holder);

		// check the logs produced by the ContainerSealant who uses CorbaNullFinder
		LogRecord[] logRecords = collectingLogger.getCollectedLogRecords();
		assertEquals(3, logRecords.length);
		assertEquals("intercepted a call to 'InstanceForUnitTest#methodWithReturnData'.", logRecords[0].getMessage());
		assertTrue(logRecords[1].getMessage().startsWith("returning from InstanceForUnitTest#methodWithReturnData after"));
		System.out.println(logRecords[2].getMessage());
		assertEquals(
				"Illegal null value in out parameter(s) of method methodWithReturnData:\n" + 
				"  Parameter StringHolder: \n" + 
				"    Null string in field StringHolder/value\n" + 
				"  Parameter Struct1Holder: \n" +  
				"    Null string in field Struct1Holder/value/mystring\n" + 
				"    Null enum in field Struct1Holder/value/myenum1\n" + 
				"  Parameter StringHolder: \n" + 
				"    Null string in field StringHolder/value\n" + 
				"  Parameter Struct1Holder: \n" + 
				"    Null string in field Struct1Holder/value/mystring\n" + 
				"    Null enum in field Struct1Holder/value/myenum1\n",
				logRecords[2].getMessage() );
	}
}
