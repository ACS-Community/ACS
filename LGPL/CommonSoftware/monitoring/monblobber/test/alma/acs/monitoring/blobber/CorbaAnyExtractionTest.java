/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.monitoring.blobber;


import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.omg.CORBA.Any;

import alma.TMCDB.MonitorBlob;
import alma.TMCDB.MonitorDataBlock;
import alma.TMCDB.booleanSeqBlobData;
import alma.TMCDB.booleanSeqBlobDataSeqHelper;
import alma.TMCDB.doubleBlobData;
import alma.TMCDB.doubleBlobDataSeqHelper;
import alma.TMCDB.doubleSeqBlobData;
import alma.TMCDB.doubleSeqBlobDataSeqHelper;
import alma.TMCDB.floatBlobData;
import alma.TMCDB.floatBlobDataSeqHelper;
import alma.TMCDB.floatSeqBlobData;
import alma.TMCDB.floatSeqBlobDataSeqHelper;
import alma.TMCDB.longLongBlobData;
import alma.TMCDB.longLongBlobDataSeqHelper;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.testsupport.JUnit4StandaloneTestBase;
import alma.acs.monitoring.DAO.ComponentData;
import alma.acs.monitoring.blobber.TestBlobber.TestMonitorPointExpert;

/**
 * Moved here the tests from {@link BlobberWorkerUnitTest}
 * that deal with extraction of Corba Any data and its processing.
 * <p>
 * Fixed the existing tests to pass (it was early work in progress),
 * weeded out unnecessary code and made some cases more challenging
 * using different numbers or data types. Overall the old structure was maintained though.
 * <p>
 * These tests do not need a blobber component and its blobber worker object,
 * so that we can write them as fairly stand-alone tests.
 * 
 * @author hsommer
 */
public class CorbaAnyExtractionTest extends JUnit4StandaloneTestBase
{
	private AcsCorba acsCorba;
	
	private AnyExtractor anyExtractor;
	
	private Clobber clobber;
	
	private TestMonitorPointExpert monitorPointExpert;
	
	/**
	 * Base time is using Array time which has an epoch of 1858, Nov 17, hence the addition.
	 */
	private static final long BASE_TIME = (System.currentTimeMillis() + 3506716800000L) * 1000000;


	@Before
	public void setUp() throws Exception {
		super.setUp();
		
		acsCorba = new AcsCorba(logger);
		acsCorba.initCorbaForClient(false);
		
		monitorPointExpert = new TestMonitorPointExpert();
		
		anyExtractor = new AnyExtractor(logger, monitorPointExpert);
		clobber = new Clobber(logger);
	}

	@After
	public void tearDown() throws Exception {
		super.tearDown();
	}
	
	
	/**
	 * Test of extractData method for 'doubleBlobDataSeq' data.
	 */
	@Test
	public void testExtractData_doubleBlobDataSeq() throws Exception {
		
		String propertyName = "MODULE_MODE_STATUS";

		Any any = create_any();
		double[] doubleDataArray = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
		doubleBlobData[] doubleBlobDataArray = createDoubleBlobData(doubleDataArray);
		doubleBlobDataSeqHelper.insert(any, doubleBlobDataArray);

		List<MonitorPointTimeSeries> extractedData = anyExtractor.extractData(any, propertyName);
		assertThat(extractedData, hasSize(1));
		
		// Check the raw data
		MonitorPointTimeSeries mpTs = extractedData.get(0);
		assertThat(mpTs.getCorbaTypeId(), equalTo("IDL:alma/TMCDB/doubleBlobDataSeq:1.0"));
		assertThat(mpTs.getMonitorPointIndex(), equalTo(0));
		List<MonitorPointValue> dataList = mpTs.getDataList();
		assertThat(dataList, hasSize(doubleDataArray.length));
		for (int i = 0; i < doubleBlobDataArray.length; i++) {
			MonitorPointValue mpVal = dataList.get(i);
			assertThat(mpVal.getTime(), equalTo(BASE_TIME + i));
			assertThat(mpVal.getData(), contains((Object) new Double(doubleDataArray[i])));
		}

		// Generate and check the CLOB data
		String clobActual = clobber.generateClob(mpTs);
		String clobExpected = 
				 BASE_TIME      + "|1.0|" + 
				(BASE_TIME + 1) + "|2.0|" + 
				(BASE_TIME + 2) + "|3.0|" + 
				(BASE_TIME + 3) + "|4.0|" + 
				(BASE_TIME + 4) + "|5.0|" + 
				(BASE_TIME + 5) + "|6.0|" + 
				(BASE_TIME + 6) + "|7.0|" + 
				(BASE_TIME + 7) + "|8.0|" + 
				(BASE_TIME + 8) + "|9.0|" + 
				(BASE_TIME + 9) + "|10.0\n";
		assertThat(clobActual, equalTo(clobExpected));

		logger.info("Validated doubleBlobDataSeq.");
	}

	/**
	 * Test of extractData method for 'doubleSeqBlobDataSeq' data for a multi-valued MP.
	 */
	@Test
	public void testExtractData_doubleSeqBlobDataSeq_multivalued() throws Exception {
		String propertyName = "MODULE_MODE_STATUS";

		Any any = create_any();
		double[] doubleData_time1 = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
		double[] doubleData_time2 = { 11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8, 19.9, 20.0 };
		double[] doubleData_time3 = { 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0 };
		double[][] doubleDataMatrix = { 
				doubleData_time1, 
				doubleData_time2, 
				doubleData_time3 };
		doubleSeqBlobData[] doubleSeqBlobDataArray = createDoubleSeqBlobData(doubleDataMatrix);
		doubleSeqBlobDataSeqHelper.insert(any, doubleSeqBlobDataArray);
		
		monitorPointExpert.setMultivalued(propertyName, true);
		
		// Test the AnyExtractor stand-alone
		
		List<MonitorPointTimeSeries> extractedData = anyExtractor.extractData(any, propertyName);
		assertThat("No demultiplexing into several MonitorPointTimeSeries instances expected", extractedData, hasSize(1));
		
		// Check the raw data
		MonitorPointTimeSeries mpTs = extractedData.get(0);
		assertThat(mpTs.getCorbaTypeId(), equalTo("IDL:alma/TMCDB/doubleSeqBlobDataSeq:1.0"));
		assertThat(mpTs.getMonitorPointIndex(), equalTo(0));
		List<MonitorPointValue> dataList = mpTs.getDataList();
		assertThat(dataList, hasSize(doubleDataMatrix.length));
		for (int i = 0; i < doubleDataMatrix.length; i++) {
			MonitorPointValue mpVal = dataList.get(i);
			assertThat(mpVal.getTime(), equalTo(BASE_TIME + i));
			List<Object> data = mpVal.getData();
			assertThat(data, hasSize(doubleData_time1.length));
			for (int j = 0; j < doubleData_time1.length; j++) {
				Object dataPart = data.get(j);
				assertThat(dataPart, instanceOf(Double.class));
				double dataPartExpected = doubleDataMatrix[i][j];
				assertThat(((Double)dataPart).doubleValue(), equalTo(dataPartExpected));
			}
		}
		logger.info("Validated doubleSeqBlobDataSeq interpreted as coming from a multivalued MP.");

		// As a variation we test also "BlobberWorker.createBlobData" which in real life surrounds the AnyExtractor call 
		
		String componentName = "CONTROL/DV01/PSA";
		String serialNumber = "3456328928847";
		MonitorBlob blob = new MonitorBlob(false, (short) 0, new String[]{}, "wrong:" + propertyName, any);
		MonitorBlob[] blobs = new MonitorBlob[] {blob};
		long startTime = BASE_TIME + 100;
		long stopTime = BASE_TIME + 101;
		MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
		BlobData blobData = BlobberWorker.createBlobData(block, blob, extractedData.get(0), propertyName, serialNumber, logger);
		String clobExpected = 
				 BASE_TIME      + "|1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0|" +
				(BASE_TIME + 1) + "|11.1 12.2 13.3 14.4 15.5 16.6 17.7 18.8 19.9 20.0|" +
				(BASE_TIME + 2) + "|21.0 22.0 23.0 24.0 25.0 26.0 27.0 28.0 29.0 30.0\n";
		checkComponentData(blobData, clobExpected, 3, componentName, propertyName, serialNumber, startTime, stopTime, 0, null);
	}

	
	/**
	 * Test of extractData method for 'doubleSeqBlobDataSeq' data for a sequence property 
	 * that expands into multiple single-valued MPs.
	 */
	@Test
	public void testExtractData_doubleSeqBlobDataSeq_singlevalued() throws Exception {
		String propertyName = "SYSTEM_STATUS";

		Any any = create_any();
		double[] doubleData_time1 = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
		double[] doubleData_time2 = { 11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8, 19.9, 20.0 };
		double[] doubleData_time3 = { 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0 };
		double[][] doubleDataMatrix = { 
				doubleData_time1, 
				doubleData_time2, 
				doubleData_time3 };
		doubleSeqBlobData[] doubleSeqBlobDataArray = createDoubleSeqBlobData(doubleDataMatrix);
		doubleSeqBlobDataSeqHelper.insert(any, doubleSeqBlobDataArray);
		
		monitorPointExpert.setMultivalued(propertyName, false);
		
		// Test the AnyExtractor stand-alone
		
		List<MonitorPointTimeSeries> extractedData = anyExtractor.extractData(any, propertyName);
		assertThat("Demultiplexing into several MonitorPointTimeSeries instances expected", extractedData, hasSize(doubleData_time1.length));
		
		for (int index = 0; index < extractedData.size(); index++) {
			// check one of the expanded logical properties at a time
			MonitorPointTimeSeries mpTs = extractedData.get(index);
			assertThat(mpTs.getCorbaTypeId(), equalTo("IDL:alma/TMCDB/doubleBlobDataSeq:1.0"));
			assertThat(mpTs.getMonitorPointIndex(), equalTo(index));
			List<MonitorPointValue> dataList = mpTs.getDataList();
			assertThat(dataList, hasSize(doubleDataMatrix.length));

			for (int i = 0; i < doubleDataMatrix.length; i++) {
				MonitorPointValue mpVal = dataList.get(i);
				assertThat(mpVal.getTime(), equalTo(BASE_TIME + i));
				// This should be the transpose of matrix doubleDataMatrix
				assertThat(mpVal.getData(), contains((Object) new Double(doubleDataMatrix[i][index])));
			}
		}
		
		logger.info("Validated doubleSeqBlobDataSeq interpreted as coming from a multiple single-valued MPs.");

		// As a variation we test also "BlobberWorker.createBlobData" which in real life surrounds the AnyExtractor call.
		// It includes generation of statistics and CLOB data.
		
		String componentName = "CONTROL/DV01/PSA";
		String serialNumber = "3456328928847";
		MonitorBlob blob = new MonitorBlob(false, (short) 0, new String[]{}, "wrong:" + propertyName, any);
		MonitorBlob[] blobs = new MonitorBlob[] {blob};
		long startTime = BASE_TIME + 100;
		long stopTime = BASE_TIME + 101;
		MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
		String[] clobsExpected = new String[] { 
				BASE_TIME + "|1.0|"  + (BASE_TIME+1) + "|11.1|" + (BASE_TIME+2) + "|21.0\n",
				BASE_TIME + "|2.0|"  + (BASE_TIME+1) + "|12.2|" + (BASE_TIME+2) + "|22.0\n", 
				BASE_TIME + "|3.0|"  + (BASE_TIME+1) + "|13.3|" + (BASE_TIME+2) + "|23.0\n", 
				BASE_TIME + "|4.0|"  + (BASE_TIME+1) + "|14.4|" + (BASE_TIME+2) + "|24.0\n", 
				BASE_TIME + "|5.0|"  + (BASE_TIME+1) + "|15.5|" + (BASE_TIME+2) + "|25.0\n", 
				BASE_TIME + "|6.0|"  + (BASE_TIME+1) + "|16.6|" + (BASE_TIME+2) + "|26.0\n", 
				BASE_TIME + "|7.0|"  + (BASE_TIME+1) + "|17.7|" + (BASE_TIME+2) + "|27.0\n", 
				BASE_TIME + "|8.0|"  + (BASE_TIME+1) + "|18.8|" + (BASE_TIME+2) + "|28.0\n", 
				BASE_TIME + "|9.0|"  + (BASE_TIME+1) + "|19.9|" + (BASE_TIME+2) + "|29.0\n", 
				BASE_TIME + "|10.0|" + (BASE_TIME+1) + "|20.0|" + (BASE_TIME+2) + "|30.0\n" };
		String[] statisticsExpected = {
				"min: 1.0 max: 21.0 mean: 11.033333333333333 stdDev: 10.000166665277801\n",
				"min: 2.0 max: 22.0 mean: 12.066666666666666 stdDev: 10.000666644445925\n",
				"min: 3.0 max: 23.0 mean: 13.100000000000001 stdDev: 10.001499887516873\n",
				"min: 4.0 max: 24.0 mean: 14.133333333333333 stdDev: 10.002666311205894\n",
				"min: 5.0 max: 25.0 mean: 15.166666666666668 stdDev: 10.004165798972613\n",
				"min: 6.0 max: 26.0 mean: 16.2 stdDev: 10.00599820107919\n",
				"min: 7.0 max: 27.0 mean: 17.233333333333334 stdDev: 10.008163334665024\n",
				"min: 8.0 max: 28.0 mean: 18.266666666666666 stdDev: 10.010660983837848\n",
				"min: 9.0 max: 29.0 mean: 19.3 stdDev: 10.013490899781155\n",
				"min: 10.0 max: 30.0 mean: 20.0 stdDev: 10.0\n"
		};
		for (int i = 0; i < extractedData.size(); i++) {
			BlobData blobData = BlobberWorker.createBlobData(block, blob, extractedData.get(i), propertyName, serialNumber, logger);
			checkComponentData(blobData, clobsExpected[i], 3, componentName, propertyName, serialNumber, startTime, stopTime, i, statisticsExpected[i]);
		}
	}

	
	/**
	 * Test of extractData method for 'floatBlobDataSeq' data.
	 */
	@Test
	public void testExtractData_floatBlobDataSeq() throws Exception {
		
		String propertyName = "MODULE_MODE_STATUS";

		Any any = create_any();
		float[] floatDataArray = {1f, 2.1f, 3f, 4f, 5f, 6.666666f, 7f, 8f, 9f, 10f};
		floatBlobData[] floatBlobDataArray = createFloatBlobData(floatDataArray);
		floatBlobDataSeqHelper.insert(any, floatBlobDataArray);
		
		List<MonitorPointTimeSeries> extractedData = anyExtractor.extractData(any, propertyName);
		assertThat(extractedData, hasSize(1));

		// Check the raw data
		MonitorPointTimeSeries mpTs = extractedData.get(0);
		assertThat(mpTs.getCorbaTypeId(), equalTo("IDL:alma/TMCDB/floatBlobDataSeq:1.0"));
		assertThat(mpTs.getMonitorPointIndex(), equalTo(0));
		List<MonitorPointValue> dataList = mpTs.getDataList();
		assertThat(dataList, hasSize(floatDataArray.length));
		for (int i = 0; i < floatBlobDataArray.length; i++) {
			MonitorPointValue mpVal = dataList.get(i);
			assertThat(mpVal.getTime(), equalTo(BASE_TIME + i));
			assertThat(mpVal.getData(), contains((Object) new Float(floatDataArray[i])));
		}

		// Generate and check the CLOB data
		String clobActual = clobber.generateClob(mpTs);
		String clobExpected = 
				 BASE_TIME    + "|1.0|" + 
				(BASE_TIME+1) + "|2.1|" + 
				(BASE_TIME+2) + "|3.0|" + 
				(BASE_TIME+3) + "|4.0|" + 
				(BASE_TIME+4) + "|5.0|" + 
				(BASE_TIME+5) + "|6.666666|" + 
				(BASE_TIME+6) + "|7.0|" + 
				(BASE_TIME+7) + "|8.0|" + 
				(BASE_TIME+8) + "|9.0|" + 
				(BASE_TIME+9) + "|10.0\n";
		assertThat(clobActual, equalTo(clobExpected));

		logger.info("Validated floatBlobDataSeq.");
	}
	
	
	/**
	 * Test of extractData method for 'floatSeqBlobDataSeq' data for a multi-valued MP.
	 */
	@Test
	public void testExtractData_floatSeqBlobDataSeq_multivalued() throws Exception {
		String propertyName = "MODULE_MODE_STATUS";

		Any any = create_any();
		float[] floatData_time1 = { 1.111111f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 9f, 10f };
		float[] floatData_time2 = { 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
		float[] floatData_time3 = { 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 };
		float[][] floatDataMatrix = { 
				floatData_time1, 
				floatData_time2, 
				floatData_time3 };
		floatSeqBlobData[] floatSeqBlobDataArray = createFloatSeqBlobData(floatDataMatrix);
		floatSeqBlobDataSeqHelper.insert(any, floatSeqBlobDataArray);
		
		monitorPointExpert.setMultivalued(propertyName, true);
		
		// Test the AnyExtractor stand-alone
		
		List<MonitorPointTimeSeries> extractedData = anyExtractor.extractData(any, propertyName);
		assertThat("No demultiplexing into several MonitorPointTimeSeries instances expected", extractedData, hasSize(1));
		
		// Check the raw data
		MonitorPointTimeSeries mpTs = extractedData.get(0);
		assertThat(mpTs.getCorbaTypeId(), equalTo("IDL:alma/TMCDB/floatSeqBlobDataSeq:1.0"));
		assertThat(mpTs.getMonitorPointIndex(), equalTo(0));
		List<MonitorPointValue> dataList = mpTs.getDataList();
		assertThat(dataList, hasSize(floatDataMatrix.length));
		for (int i = 0; i < floatDataMatrix.length; i++) {
			MonitorPointValue mpVal = dataList.get(i);
			assertThat(mpVal.getTime(), equalTo(BASE_TIME + i));
			List<Object> data = mpVal.getData();
			assertThat(data, hasSize(floatData_time1.length));
			for (int j = 0; j < floatData_time1.length; j++) {
				Object dataPart = data.get(j);
				assertThat(dataPart, instanceOf(Float.class));
				float dataPartExpected = floatDataMatrix[i][j];
				assertThat(((Float)dataPart).floatValue(), equalTo(dataPartExpected));
			}
		}
		logger.info("Validated floatSeqBlobDataSeq interpreted as coming from a multivalued MP.");
		
		// As a variation we test also "BlobberWorker.createBlobData" which in real life surrounds the AnyExtractor call 
		
		String componentName = "CONTROL/DV01/PSA";
		String serialNumber = "3456328928847";
		MonitorBlob blob = new MonitorBlob(false, (short) 0, new String[]{}, "wrong:" + propertyName, any);
		MonitorBlob[] blobs = new MonitorBlob[] {blob};
		long startTime = BASE_TIME + 100;
		long stopTime = BASE_TIME + 101;
		MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
		BlobData blobData = BlobberWorker.createBlobData(block, blob, extractedData.get(0), propertyName, serialNumber, logger);
		String clobExpected = 
				 BASE_TIME      + "|1.111111 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0|" +
				(BASE_TIME + 1) + "|11.0 12.0 13.0 14.0 15.0 16.0 17.0 18.0 19.0 20.0|" +
				(BASE_TIME + 2) + "|21.0 22.0 23.0 24.0 25.0 26.0 27.0 28.0 29.0 30.0\n";
		checkComponentData(blobData, clobExpected, 3, componentName, propertyName, serialNumber, startTime, stopTime, 0, null);
	}


	/**
	 * Test of extractData method for 'longLongBlobDataSeq' data.
	 */
	@Test
	public void testExtractData_longLongBlobDataSeq() throws Exception {
		
		String propertyName = "VOLTAGE_MID_1";

		Any any = create_any();
		// We try out a rather large outlyer value. Note that for even larger numbers
		// such as 0x7f0fffffffffffffL, the double-based statistics yield a max value wrong by +-1,
		// which is probably acceptable, but we avoid it here in the test and use
		// only a number of the order of a quadrillion.
		long[] longLongDataArray = {1, 2, 3, 4, 5, 6, 7, 8, 9, 0x000fffffffffffffL};
		longLongBlobData[] longLongBlobDataArray = createLongLongBlobData(longLongDataArray);
		longLongBlobDataSeqHelper.insert(any, longLongBlobDataArray);
		
		List<MonitorPointTimeSeries> extractedData = anyExtractor.extractData(any, propertyName);
		assertThat(extractedData, hasSize(1));

		// Check the raw data
		MonitorPointTimeSeries mpTs = extractedData.get(0);
		assertThat(mpTs.getCorbaTypeId(), equalTo("IDL:alma/TMCDB/longLongBlobDataSeq:1.0"));
		assertThat(mpTs.getMonitorPointIndex(), equalTo(0));
		List<MonitorPointValue> dataList = mpTs.getDataList();
		assertThat(dataList, hasSize(longLongBlobDataArray.length));
		for (int i = 0; i < longLongBlobDataArray.length; i++) {
			MonitorPointValue mpVal = dataList.get(i);
			assertThat(mpVal.getTime(), equalTo(BASE_TIME + i));
			assertThat(mpVal.getData(), contains((Object) new Long(longLongDataArray[i])));
		}
		logger.info("Validated longLongBlobDataSeq.");

		// As a variation we test also "BlobberWorker.createBlobData" which in real life surrounds the AnyExtractor call 
		String componentName = "CONTROL/DV01/PSA";
		String serialNumber = "3456328928847";
		MonitorBlob blob = new MonitorBlob(false, (short) 0, new String[]{}, "wrong:" + propertyName, any);
		MonitorBlob[] blobs = new MonitorBlob[] {blob};
		long startTime = BASE_TIME + 100;
		long stopTime = BASE_TIME + 101;
		MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
		BlobData blobData = BlobberWorker.createBlobData(block, blob, extractedData.get(0), propertyName, serialNumber, logger);
		String clobExpected = 
				 BASE_TIME    + "|1|" + 
				(BASE_TIME+1) + "|2|" + 
				(BASE_TIME+2) + "|3|" + 
				(BASE_TIME+3) + "|4|" + 
				(BASE_TIME+4) + "|5|" + 
				(BASE_TIME+5) + "|6|" + 
				(BASE_TIME+6) + "|7|" + 
				(BASE_TIME+7) + "|8|" + 
				(BASE_TIME+8) + "|9|" + 
				(BASE_TIME+9) + "|4503599627370495\n";
		String statisticsExpected = "min: 1 max: 4503599627370495 mean: 4.50359962737054E14 stdDev: 1.4241632491976338E15\n";
		checkComponentData(blobData, clobExpected, 10, componentName, propertyName, serialNumber, startTime, stopTime, 0, statisticsExpected);
	}
	
	
	/**
	 * Test of extractData method for 'booleanSeqBlobDataSeq' data for a sequence property 
	 * that expands into multiple single-valued MPs.
	 */
	@Test
	public void testExtractData_booleanSeqBlobDataSeq_singlevalued() throws Exception {
		String propertyName = "SYSTEM_STATUS";

		Any any = create_any();
		boolean[] booleanData_time1 = {false, true, false};
		boolean[] booleanData_time2 = {true, true, true};
		boolean[][] booleanDataMatrix = { 
				booleanData_time1, 
				booleanData_time2 };
		booleanSeqBlobData[] booleanSeqBlobDataArray = createBooleanSeqBlobData(booleanDataMatrix);
		booleanSeqBlobDataSeqHelper.insert(any, booleanSeqBlobDataArray);
		
		monitorPointExpert.setMultivalued(propertyName, false);
		
		// Test the AnyExtractor stand-alone
		
		List<MonitorPointTimeSeries> extractedData = anyExtractor.extractData(any, propertyName);
		assertThat("Demultiplexing into several MonitorPointTimeSeries instances expected", extractedData, hasSize(booleanData_time1.length));
		
		for (int index = 0; index < extractedData.size(); index++) {
			// check one of the expanded logical properties at a time
			MonitorPointTimeSeries mpTs = extractedData.get(index);
			assertThat(mpTs.getCorbaTypeId(), equalTo("IDL:alma/TMCDB/booleanBlobDataSeq:1.0"));
			assertThat(mpTs.getMonitorPointIndex(), equalTo(index));
			List<MonitorPointValue> dataList = mpTs.getDataList();
			assertThat(dataList, hasSize(booleanDataMatrix.length));

			for (int i = 0; i < booleanDataMatrix.length; i++) {
				MonitorPointValue mpVal = dataList.get(i);
				assertThat(mpVal.getTime(), equalTo(BASE_TIME + i));
				// This should be the transpose of matrix booleanDataMatrix
				assertThat(mpVal.getData(), contains((Object) new Boolean(booleanDataMatrix[i][index])));
			}
		}
		
		logger.info("Validated booleanSeqBlobDataSeq interpreted as coming from a multiple single-valued MPs.");

		// As a variation we test also "BlobberWorker.createBlobData" which in real life surrounds the AnyExtractor call.
		// It includes generation of CLOB data.
		
		String componentName = "CONTROL/DV01/PSA";
		String serialNumber = "3456328928847";
		MonitorBlob blob = new MonitorBlob(false, (short) 0, new String[]{}, "wrong:" + propertyName, any);
		MonitorBlob[] blobs = new MonitorBlob[] {blob};
		long startTime = BASE_TIME + 100;
		long stopTime = BASE_TIME + 101;
		MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
		String[] clobsExpected = new String[] { 
				BASE_TIME + "|0|"  + (BASE_TIME+1) + "|1\n",
				BASE_TIME + "|1|"  + (BASE_TIME+1) + "|1\n",
				BASE_TIME + "|0|"  + (BASE_TIME+1) + "|1\n" };
		String[] statisticsExpected = {null, null, null};
		for (int i = 0; i < extractedData.size(); i++) {
			BlobData blobData = BlobberWorker.createBlobData(block, blob, extractedData.get(i), propertyName, serialNumber, logger);
			checkComponentData(blobData, clobsExpected[i], 2, componentName, propertyName, serialNumber, startTime, stopTime, i, statisticsExpected[i]);
		}
	}


	
	
	private Any create_any() {
		Any ret = acsCorba.getORB().create_any();
		assertThat(ret, notNullValue());
		return ret;
	}
	
	private doubleBlobData[] createDoubleBlobData(double[] inValue) {
		doubleBlobData[] outArray = new doubleBlobData[inValue.length];
		int index = 0;
		for (double value : inValue) {
			outArray[index] = new doubleBlobData(BASE_TIME + index, value);
			index++;
		}
		return outArray;
	}

	private doubleSeqBlobData[] createDoubleSeqBlobData(double[][] inValue) {
		doubleSeqBlobData[] array = new doubleSeqBlobData[inValue.length];
		int index = 0;
		for (double[] value : inValue) {
			array[index] = new doubleSeqBlobData(BASE_TIME + index, value);
			index++;
		}
		return array;
	}

	private floatBlobData[] createFloatBlobData(float[] inValue) {
		floatBlobData[] outArray = new floatBlobData[inValue.length];
		int index = 0;
		for (float value : inValue) {
			outArray[index] = new floatBlobData(BASE_TIME + index, value);
			index++;
		}
		return outArray;
	}
	
	private floatSeqBlobData[] createFloatSeqBlobData(float[][] inValue) {
		floatSeqBlobData[] array = new floatSeqBlobData[inValue.length];
		int index = 0;
		for (float[] value : inValue) {
			array[index] = new floatSeqBlobData(BASE_TIME + index, value);
			index++;
		}
		return array;
	}

	private longLongBlobData[] createLongLongBlobData(long[] inValue) {
		longLongBlobData[] outArray = new longLongBlobData[inValue.length];
		int index = 0;
		for (long value : inValue) {
			outArray[index] = new longLongBlobData(BASE_TIME + index, value);
			index++;
		}
		return outArray;
	}

	private booleanSeqBlobData[] createBooleanSeqBlobData(boolean[][] inValue) {
		booleanSeqBlobData[] array = new booleanSeqBlobData[inValue.length];
		int index = 0;
		for (boolean[] value : inValue) {
			array[index] = new booleanSeqBlobData(BASE_TIME + index, value);
			index++;
		}
		return array;
	}


	private void checkComponentData(ComponentData inData, String clobExpected, int sampleSize, String componentName,
			String propertyName, String serialNumber, long startTime, long stopTime, Integer index, String statistics) {
		
		assertThat(inData.clob, equalTo(clobExpected));
		assertThat(inData.sampleSize, equalTo(sampleSize));
		assertThat(inData.componentName, equalTo(componentName));
		assertThat(inData.propertyName, equalTo(propertyName));
		assertThat(inData.serialNumber, equalTo(serialNumber));
		assertThat(inData.startTime, equalTo(startTime));
		assertThat(inData.stopTime, equalTo(stopTime));
		assertThat(inData.index, equalTo(index));
		if (statistics == null) {
			assertThat(inData.statistics, nullValue());
		}
		else {
			assertThat(inData.statistics, notNullValue());
			assertThat(inData.statistics.toString(), equalTo(statistics));
		}
	}
	
	
	
}
