package alma.acs.nc;


import org.omg.CORBA.Any;

import alma.ACS.booleanSeqHelper;
import alma.ACS.doubleSeqHelper;
import alma.ACS.floatSeqHelper;
import alma.ACS.longSeqHelper;
import alma.ACS.stringSeqHelper;
import alma.ACS.uLongLongSeqHelper;
import alma.ACS.uLongSeqHelper;

import alma.ADMINTEST1.NotNestedEvent;
import alma.ADMINTEST1.NotNestedEventHelper;
import alma.ADMINTEST1.InterfaceForNestedEventDefinitionPackage.NestedEvent;
import alma.ADMINTEST1.InterfaceForNestedEventDefinitionPackage.NestedEventHelper;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.exceptions.AcsJException;
import java.util.Vector;

public class AnyAideTest extends ComponentClientTestCase {

	private AnyAide anyAide;
	private AdvancedContainerServices advancedCS;
	
	public AnyAideTest() throws Exception {
		super("AnyAideTest");
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		anyAide = new AnyAide(getContainerServices());
		advancedCS = getContainerServices().getAdvancedContainerServices();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testCorbaStructToJavaClass() {
//		String nestedStructId = "IDL:alma/acssamp/NotNestedStruct:1.0";
		NotNestedEvent notNestedStruct = new NotNestedEvent();
		notNestedStruct.val = advancedCS.getAny();
		notNestedStruct.val.insert_long(12);
		Any any1 = advancedCS.getAny();
		NotNestedEventHelper.insert(any1, notNestedStruct);
		String qualClassName = anyAide.corbaStructToJavaClass(any1.type(), false);
		assertEquals("alma.ADMINTEST1.NotNestedEvent", qualClassName);
		
//		nestedStructId = "IDL:alma/acssamp/SampObj/SampDataBlock:1.0";
		NestedEvent nestedStruct = new NestedEvent();
		nestedStruct.sampVal = advancedCS.getAny();
		nestedStruct.sampVal.insert_long(12);
		Any any2 = advancedCS.getAny();
		NestedEventHelper.insert(any2, nestedStruct);
		qualClassName = anyAide.corbaStructToJavaClass(any2.type(), true);
		assertEquals("alma.ADMINTEST1.InterfaceForNestedEventDefinitionPackage.NestedEvent", qualClassName);
	}

	public void testDoubleArrayToCorbaAny() {
		final double [] doubles = new double[] {1.3,3.5,5.7};
		Any any1 = anyAide.doubleArrayToCorbaAny(doubles);
		double [] doubles2;
		doubles2 = doubleSeqHelper.extract(any1);
		assertEquals(doubles.length, doubles2.length);
		for(int i = 0;i < doubles2.length;++i) {
			assertEquals(doubles[i], doubles2[i]);
		}
	}

	public void testFloatArrayToCorbaAny() {
		final float [] floats = new float[] {1.3f,3.5f,5.7f};
		Any any1 = anyAide.floatArrayToCorbaAny(floats);
		float [] floats2;
		floats2 = floatSeqHelper.extract(any1);
		assertEquals(floats.length, floats2.length);
		for(int i = 0;i < floats2.length;++i) {
			assertEquals(floats[i], floats2[i]);
		}
	}

	public void testIntArrayToCorbaAny() {
		final int [] ints = new int[] {3,5,7};
		Any any1 = anyAide.intArrayToCorbaAny(ints);
		int [] ints2;
		ints2 = longSeqHelper.extract(any1);
		assertEquals(ints.length, ints2.length);
		for(int i = 0;i < ints2.length;++i) {
			assertEquals(ints[i], ints2[i]);
		}
	}


	public void testStringArrayToCorbaAny() {
		final String [] strs = new String[] {"3","5","7"};
		Any any1 = anyAide.stringArrayToCorbaAny(strs);
		String [] strs2;
		strs2 = stringSeqHelper.extract(any1);
		assertEquals(strs.length, strs2.length);
		for(int i = 0;i < strs2.length;++i) {
			assertEquals(strs[i], strs2[i]);
		}
	}


	public void testObjectToCorbaAny() {
		final String strVal = "str";
		final Long lVal = 13L;
		final Double dVal = 13.7;
		final Float fVal = 13.3f;
		final Integer iVal = 11;
		final int [] intArr = new int[] {1,2,3,4};
		final NestedEvent neVal = new NestedEvent();
		neVal.sampVal = advancedCS.getAny();
		neVal.sampVal.insert_long(15);

		Any anyNull = null, anyString = null, anyLong = null, anyDouble = null, anyFloat = null, anyInt = null;
		Any anyNe = null, anyIntArr = null;
		try {
			anyNull = anyAide.objectToCorbaAny(null);
			anyString = anyAide.objectToCorbaAny(strVal);
			anyLong = anyAide.objectToCorbaAny(lVal);
			anyDouble = anyAide.objectToCorbaAny(dVal);
			anyFloat = anyAide.objectToCorbaAny(fVal);
			anyInt = anyAide.objectToCorbaAny(iVal);
			anyNe = anyAide.objectToCorbaAny(neVal);
			anyIntArr = anyAide.objectToCorbaAny(intArr);
		} catch(AcsJException ex) {}
			
		assertNotNull(anyNull);
		assertNotNull(anyString);
		assertNotNull(anyLong);
		assertNotNull(anyDouble);
		assertNotNull(anyFloat);
		assertNotNull(anyInt);
		assertNotNull(anyNe);
		assertNotNull(anyIntArr);
			
		assertNull(anyNull.extract_Object());
		assertEquals(lVal.longValue(), anyLong.extract_longlong());
		assertEquals(dVal.doubleValue(), anyDouble.extract_double());
		assertEquals(fVal.floatValue(), anyFloat.extract_float());
		assertEquals(iVal.intValue(), anyInt.extract_long());
		assertEquals(strVal, anyString.extract_string());
		
		NestedEvent neVal2 = NestedEventHelper.extract(anyNe);
		assertEquals(neVal.sampVal.extract_long(), neVal2.sampVal.extract_long());

		int [] intArr2 = longSeqHelper.extract(anyIntArr);
		assertEquals(intArr.length, intArr2.length);
		for(int i = 0;i < intArr2.length;++i)
		{
			assertEquals(intArr[i], intArr2[i]);
		}

		try {
			Any anyVector = anyAide.objectToCorbaAny(new Vector());
			fail();	
		} catch(AcsJException ex) {
		}
	}

	public void testComplexObjectToCorbaAny() {
		NestedEvent neVal = new NestedEvent();
		neVal.sampVal = advancedCS.getAny();
		neVal.sampVal.insert_long(15);

		Any anyNe = null;
		try {
			anyNe = anyAide.complexObjectToCorbaAny(neVal);
		} catch(AcsJException ex) {}

		assertNotNull(anyNe);
		NestedEvent neVal2 = NestedEventHelper.extract(anyNe);
		assertEquals(neVal.sampVal.extract_long(), neVal2.sampVal.extract_long());

		try {
			anyAide.complexObjectToCorbaAny(null);
			fail();
		} catch(AcsJException ex) {}

	}

	public void testCorbaAnyToObject() {
		Any aNull = advancedCS.getAny();
		Any aInt = advancedCS.getAny();
		Any aFloat = advancedCS.getAny();
		Any aDouble = advancedCS.getAny();
		Any aStr = advancedCS.getAny();
		Any aLong = advancedCS.getAny();
		Any auLong = advancedCS.getAny();
		Any aLongLong = advancedCS.getAny();
		Any auLongLong = advancedCS.getAny();
		Any aIntSeq = advancedCS.getAny();

		aInt.insert_long(15);
		aFloat.insert_float(3.7f);
		aDouble.insert_double(3.5);
		aStr.insert_string("str value");
		aLong.insert_long(17);
		auLong.insert_ulong(17);
		aLongLong.insert_longlong(37);
		auLongLong.insert_ulonglong(37);
		int [] intSeq = new int[] {1,2,3,4};
		longSeqHelper.insert(aIntSeq, intSeq);

		Object nullVal = anyAide.corbaAnyToObject(aNull);		
		Integer iVal = (Integer)anyAide.corbaAnyToObject(aInt);
		Float fVal = (Float)anyAide.corbaAnyToObject(aFloat);
		Double dVal = (Double)anyAide.corbaAnyToObject(aDouble);
		String strVal = (String)anyAide.corbaAnyToObject(aStr);
		Integer lVal = (Integer)anyAide.corbaAnyToObject(aLong);
		Integer ulVal = (Integer)anyAide.corbaAnyToObject(auLong);
		Long llVal = (Long)anyAide.corbaAnyToObject(aLongLong);
		Long ullVal = (Long)anyAide.corbaAnyToObject(auLongLong);
		int [] intSeq2 = (int[])anyAide.corbaAnyToObject(aIntSeq);

		assertNull(nullVal);
		assertEquals(aInt.extract_long(), iVal.intValue());
		assertEquals(aFloat.extract_float(), fVal.floatValue());
		assertEquals(aDouble.extract_double(), dVal.doubleValue());
		assertEquals(aStr.extract_string(), strVal);
		assertEquals(aLong.extract_long(), lVal.longValue());
		assertEquals(auLong.extract_ulong(), ulVal.longValue());
		assertEquals(aLongLong.extract_longlong(), llVal.longValue());
		assertEquals(aLongLong.extract_longlong(), ullVal.longValue());
		assertEquals(intSeq.length, intSeq2.length);
		for(int i = 0;i < intSeq2.length;++i)
		{
			assertEquals(intSeq[i], intSeq2[i]);
		}
	}

	public void testComplexAnyToObject() {
		NestedEvent nestedStruct = new NestedEvent();
		nestedStruct.sampVal = advancedCS.getAny();
		nestedStruct.sampVal.insert_long(12);
		Any any1 = advancedCS.getAny();
		NestedEventHelper.insert(any1, nestedStruct);
		NestedEvent ns2 = (NestedEvent)anyAide.complexAnyToObject(any1);
		assertNotNull(ns2);
		assertTrue(ns2.sampVal.toString().equals("12"));
	}






}
