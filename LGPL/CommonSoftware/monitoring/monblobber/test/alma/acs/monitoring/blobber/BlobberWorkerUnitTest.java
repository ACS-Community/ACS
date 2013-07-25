package alma.acs.monitoring.blobber;

import java.util.ArrayList;
import java.util.List;

import org.omg.CORBA.Any;

import alma.MonitorArchiver.CollectorListStatus;
import alma.TMCDB.MonitorBlob;
import alma.TMCDB.MonitorDataBlock;
import alma.TMCDB.anyBlobData;
import alma.TMCDB.anyBlobDataSeqHelper;
import alma.TMCDB.booleanSeqBlobData;
import alma.TMCDB.doubleBlobData;
import alma.TMCDB.doubleBlobDataSeqHelper;
import alma.TMCDB.doubleSeqBlobData;
import alma.TMCDB.doubleSeqBlobDataSeqHelper;
import alma.TMCDB.enumBlobData;
import alma.TMCDB.enumBlobDataSeqHelper;
import alma.TMCDB.floatBlobData;
import alma.TMCDB.floatBlobDataSeqHelper;
import alma.TMCDB.floatSeqBlobData;
import alma.TMCDB.floatSeqBlobDataSeqHelper;
import alma.TMCDB.longBlobData;
import alma.TMCDB.longBlobDataSeqHelper;
import alma.TMCDB.longLongBlobData;
import alma.TMCDB.longLongBlobDataSeqHelper;
import alma.TMCDB.longLongSeqBlobData;
import alma.TMCDB.longLongSeqBlobDataSeqHelper;
import alma.TMCDB.longSeqBlobData;
import alma.TMCDB.longSeqBlobDataSeqHelper;
import alma.TMCDB.patternBlobData;
import alma.TMCDB.patternBlobDataSeqHelper;
import alma.TMCDB.stringBlobData;
import alma.TMCDB.stringBlobDataSeqHelper;
import alma.TMCDB.stringSeqBlobData;
import alma.TMCDB.stringSeqBlobDataSeqHelper;
import alma.TMCDB.uLongLongBlobData;
import alma.TMCDB.uLongLongBlobDataSeqHelper;
import alma.TMCDB.uLongLongSeqBlobData;
import alma.TMCDB.uLongLongSeqBlobDataSeqHelper;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.logging.ClientLogManager;
import alma.acs.monitoring.DAO.ComponentData;
import alma.acs.monitoring.blobber.BlobberWorker.AnyDataContainer;

/**
 * Tests the BlobberImpl and BlobberWorker classes, using local instances instead of running these
 * components remotely inside a container.
 * <p>
 * @TODO As this test was converted from TestNG to JUnit, we should also convert the Java assert statements
 * to JUnit asserts. 
 * <p>
 * @TODO Perhaps rename to BlobberImplTest, because it tests the TestBlobber which extends BlobberImpl
 * (although it also creates TestBlobberWorker instead of BlobberWorker...)
 */
public class BlobberWorkerUnitTest extends ComponentClientTestCase {

	/**
	 * This constant controls if the test data should be saved to a database as well. The database is selected by the
	 * dbConfig.properties file stored in the users current working directory.
	 */
	private static final boolean USE_DATABASE = false;

	/**
	 * Base time is using Array time which has an epoch of 1858, Nov 17, hence the addition.
	 */
	private static final long BASE_TIME = (System.currentTimeMillis() + 3506716800000L) * 1000000;

	private TestBlobber blobber;

	public BlobberWorkerUnitTest() throws Exception {
		super(BlobberWorkerUnitTest.class.getSimpleName());
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		m_logger.info("------------- BEGIN " + getClass().getSimpleName() + "#" + getName() + " -------------------------------");
		blobber = new TestBlobber(getContainerServices());
		m_logger.fine("setUp - about to call blobber.initialize");
		blobber.initialize(getContainerServices(), "TEST_BLOBBER_WORKER", USE_DATABASE);
		m_logger.fine("setUp - about to call blobber.execute");
		blobber.execute();
		m_logger.fine("setUp - returned from blobber.execute");
	}

	/**
	 * Calls {@link BlobberImpl#cleanUp()}
	 */
	protected void tearDown() throws Exception {
		blobber.cleanUp();
		m_logger.info("------------- DONE " + getClass().getSimpleName() + "#" + getName() + " -------------------------------");
		super.tearDown();
	}

	/**
	 * Overridden to disable remote logging instead of initializing it.
	 */
	@Override
	protected void initRemoteLogging() {
		ClientLogManager.getAcsLogManager().suppressRemoteLogging();
	}

	/**
	 * Returns the TestBlobber's TestBlobberWorker
	 */
	protected TestBlobberWorker getTestWorker() {
		return (TestBlobberWorker) blobber.myWorker;
	}

    /**
     * Tests the BlobberWorker's methods addCollector, containsCollector, removeCollector
     * for good and erroneous calls. The blobber worker most likely does not access the collector
     * while it's being added or removed.
     * <p>
     * @TODO: Also test for concurrency problems, e.g. removing the collector while the blobber worker gets data from it.  
     */
    public void testCollectorHandling() {

        String coll1 = "COLL1";
        
        TestBlobberWorker worker = getTestWorker();
        assertNotNull(worker);
        
        CollectorListStatus addStatus = worker.addCollector(coll1);
		assertEquals("Collector " + coll1 + " was not added.", CollectorListStatus.ADDED, addStatus);

        // Fake data is needed since the worker blocks until the first access to the first collector has finished.
//        worker.getCollector().setMonitorData(new MonitorDataBlock[0]);
        
        assertEquals("Collector " + coll1 + " was incorrectly added twice.",
        		CollectorListStatus.KNOWN, worker.addCollector(coll1));
        assertEquals("Collector list does not contain " + coll1, 
        		CollectorListStatus.KNOWN, worker.containsCollector(coll1)); 
        assertEquals("Collector " + coll1 + " was not removed.",
        		CollectorListStatus.REMOVED, worker.removeCollector(coll1));
        assertEquals("Collector " + coll1 + " was not removed.",
        		CollectorListStatus.UNKNOWN, worker.removeCollector(coll1));
        assertEquals("Collector list still contains " + coll1,
        		CollectorListStatus.UNKNOWN, worker.containsCollector(coll1));
        assertEquals("Collector " + coll1 + " was not added.",
        		CollectorListStatus.ADDED, worker.addCollector(coll1)); 
        assertEquals("Collector " + coll1 + " was incorrectly added twice.",
        		CollectorListStatus.KNOWN, worker.addCollector(coll1));
        assertEquals("Collector " + coll1 + " was not removed.",
        		CollectorListStatus.REMOVED, worker.removeCollector(coll1));
        assertEquals("Collector " + coll1 + " was not removed.",
        		CollectorListStatus.UNKNOWN, worker.removeCollector(coll1));
    }

    public void testStandardHandling() throws Exception {
    	blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any anyUp = create_any();
        double[] doubleDataUp = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
        doubleBlobData[] dataUp = createDoubleBlobData(doubleDataUp);
        doubleBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, anyUp);

        Any anyDown = create_any();
        double[] doubleDataDown = { 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0 };
        doubleBlobData[] dataDown = createDoubleBlobData(doubleDataDown);
        doubleBlobDataSeqHelper.insert(anyDown, dataDown);
        MonitorBlob blobDown = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, anyDown);

        Any anyNegUp = create_any();
		double[] doubleDataNegUp = { -10.0, -9.0, -8.0, -7.0, -6.0, -5.0, -4.0, -3.0, -2.0, -1.0 };
        doubleBlobData[] dataNegUp = createDoubleBlobData(doubleDataNegUp);
        doubleBlobDataSeqHelper.insert(anyNegUp, dataNegUp);
		MonitorBlob blobNegUp = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, anyNegUp);

        Any anyNegDown = create_any();
		double[] doubleDataNegDown = { -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0, -8.0, -9.0, -10.0 };
        doubleBlobData[] dataNegDown = createDoubleBlobData(doubleDataNegDown);
        doubleBlobDataSeqHelper.insert(anyNegDown, dataNegDown);
		MonitorBlob blobNegDown = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, anyNegDown);

        MonitorBlob[] blobs = new MonitorBlob[4];
        blobs[0] = blobUp;
        blobs[1] = blobDown;
        blobs[2] = blobNegUp;
        blobs[3] = blobNegDown;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
		MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
		MonitorDataBlock[] blocks = new MonitorDataBlock[] {block};
        
        // Feeds the above test data to the mock monitor collector
        getTestWorker().getCollector().setMonitorData(blocks);
        
        // Reads data for the 4 properties from the test blobber worker.
        // Fetching the data blocks until data arrives from the mock collector in the next blobber cycle.
        m_logger.info("Will wait for blobber worker to read data from collector.");
        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1.0|" + (BASE_TIME + 1) + "|2.0|"
                + (BASE_TIME + 2) + "|3.0|" + (BASE_TIME + 3) + "|4.0|"
                + (BASE_TIME + 4) + "|5.0|" + (BASE_TIME + 5) + "|6.0|"
                + (BASE_TIME + 6) + "|7.0|" + (BASE_TIME + 7) + "|8.0|"
                + (BASE_TIME + 8) + "|9.0|" + (BASE_TIME + 9) + "|10.0\n";
		checkData(data, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        checkStatistics(data, 1.0, 10.0, 5.5, 3.0276504091);
        m_logger.info("Validated collector data retrieved from blobber worker.");
        
        // the data for the second property is available in the same blobber worker cycle,
        // as soon as the previous property data has been fetched.
        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|10.0|" + (BASE_TIME + 1) + "|9.0|"
                + (BASE_TIME + 2) + "|8.0|" + (BASE_TIME + 3) + "|7.0|"
                + (BASE_TIME + 4) + "|6.0|" + (BASE_TIME + 5) + "|5.0|"
                + (BASE_TIME + 6) + "|4.0|" + (BASE_TIME + 7) + "|3.0|"
                + (BASE_TIME + 8) + "|2.0|" + (BASE_TIME + 9) + "|1.0\n";
		checkData(data, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        checkStatistics(data, 1.0, 10.0, 5.5, 3.0276504091);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|-10.0|" + (BASE_TIME + 1) + "|-9.0|"
                + (BASE_TIME + 2) + "|-8.0|" + (BASE_TIME + 3) + "|-7.0|"
                + (BASE_TIME + 4) + "|-6.0|" + (BASE_TIME + 5) + "|-5.0|"
                + (BASE_TIME + 6) + "|-4.0|" + (BASE_TIME + 7) + "|-3.0|"
                + (BASE_TIME + 8) + "|-2.0|" + (BASE_TIME + 9) + "|-1.0\n";
        checkStatistics(data, -10.0, -1.0, -5.5, 3.0276504091);
		checkData(data, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        checkStatistics(data, -10.0, -1.0, -5.5, 3.0276504091);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|-1.0|" + (BASE_TIME + 1) + "|-2.0|"
                + (BASE_TIME + 2) + "|-3.0|" + (BASE_TIME + 3) + "|-4.0|"
                + (BASE_TIME + 4) + "|-5.0|" + (BASE_TIME + 5) + "|-6.0|"
                + (BASE_TIME + 6) + "|-7.0|" + (BASE_TIME + 7) + "|-8.0|"
                + (BASE_TIME + 8) + "|-9.0|" + (BASE_TIME + 9) + "|-10.0\n";
		checkData(data, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        checkStatistics(data, -10.0, -1.0, -5.5, 3.0276504091);
        
        Any anyEmpty = create_any();
        double[] doubleDataEmpty = {};
        doubleBlobData[] dataEmpty = createDoubleBlobData(doubleDataEmpty);
        doubleBlobDataSeqHelper.insert(anyEmpty, dataEmpty);
		MonitorBlob blobEmpty = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, anyEmpty);

        blobs = new MonitorBlob[2];
        blobs[0] = blobEmpty;
        blobs[1] = blobUp;

		block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
        blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|1.0|" + (BASE_TIME + 1) + "|2.0|"
                + (BASE_TIME + 2) + "|3.0|" + (BASE_TIME + 3) + "|4.0|"
                + (BASE_TIME + 4) + "|5.0|" + (BASE_TIME + 5) + "|6.0|"
                + (BASE_TIME + 6) + "|7.0|" + (BASE_TIME + 7) + "|8.0|"
                + (BASE_TIME + 8) + "|9.0|" + (BASE_TIME + 9) + "|10.0\n";
		checkData(data, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        checkStatistics(data, 1.0, 10.0, 5.5, 3.0276504091);
    }

    public void testCorrelatorHandling() throws Exception {
    	blobber.setCollectorIntervalSeconds(100); 
        getTestWorker().setCanHandle(true);
        // Collector "TestCase" is just a dummy name to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCase" is not used at all.
        getTestWorker().addCollector("TestCase");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any anyUp = create_any();
        double[] doubleDataUp1 = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
        double[] doubleDataUp2 = { 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0 };
        double[] doubleDataUp3 = { 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0 };
        double[][] doubleDataUpMatrix = { doubleDataUp1, doubleDataUp2, doubleDataUp3 };
        String[] serialNumbers = { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J" };
        doubleSeqBlobData[] dataUp = createDoubleSeqBlobData(doubleDataUpMatrix);
        doubleSeqBlobDataSeqHelper.insert(anyUp, dataUp);
		MonitorBlob blobUp = new MonitorBlob(false, (short) 0, serialNumbers, "wrong:" + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;
        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

    m_logger.fine("About to call getTestWorker().fetchData()");
    
        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1.0|" + (BASE_TIME + 1) + "|11.0|" + (BASE_TIME + 2) + "|21.0\n";
        checkData(data, clob, 3, componentName, propertyName, "A", startTime, stopTime);
        checkStatistics(data, 1.0, 21.0, 11.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|2.0|" + (BASE_TIME + 1) + "|12.0|" + (BASE_TIME + 2) + "|22.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "B", startTime, stopTime);
        checkStatistics(data, 2.0, 22.0, 12.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|3.0|" + (BASE_TIME + 1) + "|13.0|" + (BASE_TIME + 2) + "|23.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "C", startTime, stopTime);
        checkStatistics(data, 3.0, 23.0, 13.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|4.0|" + (BASE_TIME + 1) + "|14.0|" + (BASE_TIME + 2) + "|24.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "D", startTime, stopTime);
        checkStatistics(data, 4.0, 24.0, 14.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|5.0|" + (BASE_TIME + 1) + "|15.0|" + (BASE_TIME + 2) + "|25.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "E", startTime, stopTime);
        checkStatistics(data, 5.0, 25.0, 15.0, 10.0);

        data = getTestWorker().fetchData();
		clob = BASE_TIME + "|6.0|" + (BASE_TIME + 1) + "|16.0|" + (BASE_TIME + 2) + "|26.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "F", startTime, stopTime);
        checkStatistics(data, 6.0, 26.0, 16.0, 10.0);

        data = getTestWorker().fetchData();
		clob = BASE_TIME + "|7.0|" + (BASE_TIME + 1) + "|17.0|" + (BASE_TIME + 2) + "|27.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "G", startTime, stopTime);
        checkStatistics(data, 7.0, 27.0, 17.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|8.0|" + (BASE_TIME + 1) + "|18.0|" + (BASE_TIME + 2) + "|28.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "H", startTime, stopTime);
        checkStatistics(data, 8.0, 28.0, 18.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|9.0|" + (BASE_TIME + 1) + "|19.0|" + (BASE_TIME + 2) + "|29.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "I", startTime, stopTime);
        checkStatistics(data, 9.0, 29.0, 19.0, 10.0);

        data = getTestWorker().fetchData();
		clob = BASE_TIME + "|10.0|" + (BASE_TIME + 1) + "|20.0|" + (BASE_TIME + 2) + "|30.0\n";
        propertyName = "" + propertyName;
		checkData(data, clob, 3, componentName, propertyName, "J", startTime, stopTime);
        checkStatistics(data, 10.0, 30.0, 20.0, 10.0);

    }

    public void testStandardFloat() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any anyUp = create_any();
        float[] dataArrayUp = { 1.0F, 2.0F, 3.0F, 4.0F, 5.0F, 6.0F, 7.0F, 8.0F,
                9.0F, 10.0F };
        floatBlobData[] dataUp = new floatBlobData[dataArrayUp.length];
        int index = 0;
        for (float value : dataArrayUp) {
            dataUp[index] = new floatBlobData(BASE_TIME + index, value);
            index++;
        }
        floatBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1.0|" + (BASE_TIME + 1) + "|2.0|"
                + (BASE_TIME + 2) + "|3.0|" + (BASE_TIME + 3) + "|4.0|"
                + (BASE_TIME + 4) + "|5.0|" + (BASE_TIME + 5) + "|6.0|"
                + (BASE_TIME + 6) + "|7.0|" + (BASE_TIME + 7) + "|8.0|"
                + (BASE_TIME + 8) + "|9.0|" + (BASE_TIME + 9) + "|10.0\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime, stopTime);
        checkStatistics(data, 1.0, 10.0, 5.5, 3.0276504091);
    }

    public void testStandardLong() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        // Note that CORBA long means Java int.
        Any anyUp = create_any();
        int[] dataArrayUp = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        longBlobData[] dataUp = new longBlobData[dataArrayUp.length];
        int index = 0;
        for (int value : dataArrayUp) {
            dataUp[index] = new longBlobData(BASE_TIME + index, value);
            index++;
        }
        longBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1|" + (BASE_TIME + 1) + "|2|"
                + (BASE_TIME + 2) + "|3|" + (BASE_TIME + 3) + "|4|"
                + (BASE_TIME + 4) + "|5|" + (BASE_TIME + 5) + "|6|"
                + (BASE_TIME + 6) + "|7|" + (BASE_TIME + 7) + "|8|"
                + (BASE_TIME + 8) + "|9|" + (BASE_TIME + 9) + "|10\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime, stopTime);
        checkStatistics(data, 1.0, 10.0, 5.5, 3.0276504091);
    }

    public void testStandardLongLong() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        // Note that CORBA longlong means Java long.
        Any anyUp = create_any();
        long[] dataArrayUp = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        longLongBlobData[] dataUp = new longLongBlobData[dataArrayUp.length];
        int index = 0;
        for (long value : dataArrayUp) {
            dataUp[index] = new longLongBlobData(BASE_TIME + index, value);
            index++;
        }
        longLongBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1|" + (BASE_TIME + 1) + "|2|"
                + (BASE_TIME + 2) + "|3|" + (BASE_TIME + 3) + "|4|"
                + (BASE_TIME + 4) + "|5|" + (BASE_TIME + 5) + "|6|"
                + (BASE_TIME + 6) + "|7|" + (BASE_TIME + 7) + "|8|"
                + (BASE_TIME + 8) + "|9|" + (BASE_TIME + 9) + "|10\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime, stopTime);
        checkStatistics(data, 1.0, 10.0, 5.5, 3.0276504091);
    }

    public void testStandardULongLong() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        // Note that CORBA ulonglong means Java long.
        Any anyUp = create_any();
        long[] dataArrayUp = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        uLongLongBlobData[] dataUp = new uLongLongBlobData[dataArrayUp.length];
        int index = 0;
        for (long value : dataArrayUp) {
            dataUp[index] = new uLongLongBlobData(BASE_TIME + index, value);
            index++;
        }
        uLongLongBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1|" + (BASE_TIME + 1) + "|2|"
                + (BASE_TIME + 2) + "|3|" + (BASE_TIME + 3) + "|4|"
                + (BASE_TIME + 4) + "|5|" + (BASE_TIME + 5) + "|6|"
                + (BASE_TIME + 6) + "|7|" + (BASE_TIME + 7) + "|8|"
                + (BASE_TIME + 8) + "|9|" + (BASE_TIME + 9) + "|10\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime, stopTime);
        checkStatistics(data, 1.0, 10.0, 5.5, 3.0276504091);
    }

    public void testStandardPattern() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any anyUp = create_any();
        long[] dataArrayUp = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        patternBlobData[] dataUp = new patternBlobData[dataArrayUp.length];
        int index = 0;
        for (long value : dataArrayUp) {
            dataUp[index] = new patternBlobData(BASE_TIME + index, (int) value);
            index++;
        }
        patternBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1|" + (BASE_TIME + 1) + "|2|"
                + (BASE_TIME + 2) + "|3|" + (BASE_TIME + 3) + "|4|"
                + (BASE_TIME + 4) + "|5|" + (BASE_TIME + 5) + "|6|"
                + (BASE_TIME + 6) + "|7|" + (BASE_TIME + 7) + "|8|"
                + (BASE_TIME + 8) + "|9|" + (BASE_TIME + 9) + "|10\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime, stopTime);
        checkStatistics(data);
    }

    public void testStandardString() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any anyUp = create_any();
        String[] dataArrayUp = { "Q", "R", "S", "T", "U", "V", "W", "X", "Y",
                "Z" };
        stringBlobData[] dataUp = new stringBlobData[dataArrayUp.length];
        int index = 0;
        for (String value : dataArrayUp) {
            dataUp[index] = new stringBlobData(BASE_TIME + index, value);
            index++;
        }
        stringBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|Q|" + (BASE_TIME + 1) + "|R|"
                + (BASE_TIME + 2) + "|S|" + (BASE_TIME + 3) + "|T|"
                + (BASE_TIME + 4) + "|U|" + (BASE_TIME + 5) + "|V|"
                + (BASE_TIME + 6) + "|W|" + (BASE_TIME + 7) + "|X|"
                + (BASE_TIME + 8) + "|Y|" + (BASE_TIME + 9) + "|Z\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime, stopTime);
        checkStatistics(data);
    }

    public void testStandardEnum() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any anyUp = create_any();
        int[] dataArrayUp = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        enumBlobData[] dataUp = new enumBlobData[dataArrayUp.length];
        int index = 0;
        for (int value : dataArrayUp) {
            dataUp[index] = new enumBlobData(BASE_TIME + index, value);
            index++;
        }
        enumBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1|" + (BASE_TIME + 1) + "|2|"
                + (BASE_TIME + 2) + "|3|" + (BASE_TIME + 3) + "|4|"
                + (BASE_TIME + 4) + "|5|" + (BASE_TIME + 5) + "|6|"
                + (BASE_TIME + 6) + "|7|" + (BASE_TIME + 7) + "|8|"
                + (BASE_TIME + 8) + "|9|" + (BASE_TIME + 9) + "|10\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime, stopTime);
        checkStatistics(data);
    }

    public void testStandardAny() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any anyUp = create_any();
        Any any1 = create_any();
        any1.insert_boolean(true);
        Any any2 = create_any();
        any2.insert_char('A');
        Any any3 = create_any();
        any3.insert_double(1.0);
        Any any4 = create_any();
        any4.insert_float(2.0F);
        Any any5 = create_any();
        any5.insert_long(123);
        Any any6 = create_any();
        any6.insert_longlong(678);
        Any any7 = create_any();
        any7.insert_octet((byte) 11);
        Any any8 = create_any();
        any8.insert_short((short) 34);
        Any any9 = create_any();
        any9.insert_string("Q");
        Any any10 = create_any();
        Any[] dataArrayUp = { any1, any2, any3, any4, any5, any6, any7, any8,
                any9, any10 };
        anyBlobData[] dataUp = new anyBlobData[dataArrayUp.length];
        int index = 0;
        for (Any value : dataArrayUp) {
            dataUp[index] = new anyBlobData(BASE_TIME + index, value);
            index++;
        }
        anyBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|true|" + (BASE_TIME + 1) + "|A|"
                + (BASE_TIME + 2) + "|1.0|" + (BASE_TIME + 3) + "|2.0|"
                + (BASE_TIME + 4) + "|123|" + (BASE_TIME + 5) + "|678|"
                + (BASE_TIME + 6) + "|11|" + (BASE_TIME + 7) + "|34|"
                + (BASE_TIME + 8) + "|Q|" + (BASE_TIME + 9) + "|null\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime, stopTime);
        checkStatistics(data);
    }

    public void testCorrelatorFloat() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any anyUp = create_any();
        float[] dataArrayUp1 = { 1.0F, 2.0F, 3.0F };
        float[] dataArrayUp2 = { 11.0F, 12.0F, 13.0F };
        float[] dataArrayUp3 = { 21.0F, 22.0F, 23.0F };
        float[][] dataUpMatrix = { dataArrayUp1, dataArrayUp2, dataArrayUp3 };
        String[] serialNumbers = { "A", "B", "C" };
        floatSeqBlobData[] dataUp = new floatSeqBlobData[dataUpMatrix.length];
        int index = 0;
        for (float[] value : dataUpMatrix) {
            dataUp[index] = new floatSeqBlobData(BASE_TIME + index, value);
            index++;
        }
        floatSeqBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, serialNumbers,
                "wrong:" + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;
        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1.0|" + (BASE_TIME + 1) + "|11.0|"
                + (BASE_TIME + 2) + "|21.0\n";
        checkData(data, clob, 3, componentName, propertyName, "A", startTime,
                stopTime);
        checkStatistics(data, 1.0, 21.0, 11.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|2.0|" + (BASE_TIME + 1) + "|12.0|"
                + (BASE_TIME + 2) + "|22.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "B", startTime,
                stopTime);
        checkStatistics(data, 2.0, 22.0, 12.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|3.0|" + (BASE_TIME + 1) + "|13.0|"
                + (BASE_TIME + 2) + "|23.0\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "C", startTime,
                stopTime);
        checkStatistics(data, 3.0, 23.0, 13.0, 10.0);

    }

    public void testCorrelatorLong() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        // Note that CORBA long means Java int.
        Any anyUp = create_any();
        int[] dataArrayUp1 = { 1, 2, 3 };
        int[] dataArrayUp2 = { 11, 12, 13 };
        int[] dataArrayUp3 = { 21, 22, 23 };
        int[][] dataUpMatrix = { dataArrayUp1, dataArrayUp2, dataArrayUp3 };
        String[] serialNumbers = { "A", "B", "C" };
        longSeqBlobData[] dataUp = new longSeqBlobData[dataUpMatrix.length];
        int index = 0;
        for (int[] value : dataUpMatrix) {
            dataUp[index] = new longSeqBlobData(BASE_TIME + index, value);
            index++;
        }
        longSeqBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, serialNumbers,
                "wrong:" + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;
        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1|" + (BASE_TIME + 1) + "|11|"
                + (BASE_TIME + 2) + "|21\n";
        checkData(data, clob, 3, componentName, propertyName, "A", startTime,
                stopTime);
        checkStatistics(data, 1.0, 21.0, 11.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|2|" + (BASE_TIME + 1) + "|12|" + (BASE_TIME + 2)
                + "|22\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "B", startTime,
                stopTime);
        checkStatistics(data, 2.0, 22.0, 12.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|3|" + (BASE_TIME + 1) + "|13|" + (BASE_TIME + 2)
                + "|23\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "C", startTime,
                stopTime);
        checkStatistics(data, 3.0, 23.0, 13.0, 10.0);

    }

    public void testCorrelatorLongLong() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        // Note that CORBA longlong means Java long.
        Any anyUp = create_any();
        long[] dataArrayUp1 = { 1, 2, 3 };
        long[] dataArrayUp2 = { 11, 12, 13 };
        long[] dataArrayUp3 = { 21, 22, 23 };
        long[][] dataUpMatrix = { dataArrayUp1, dataArrayUp2, dataArrayUp3 };
        String[] serialNumbers = { "A", "B", "C" };
        longLongSeqBlobData[] dataUp = new longLongSeqBlobData[dataUpMatrix.length];
        int index = 0;
        for (long[] value : dataUpMatrix) {
            dataUp[index] = new longLongSeqBlobData(BASE_TIME + index, value);
            index++;
        }
        longLongSeqBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, serialNumbers,
                "wrong:" + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;
        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1|" + (BASE_TIME + 1) + "|11|"
                + (BASE_TIME + 2) + "|21\n";
        checkData(data, clob, 3, componentName, propertyName, "A", startTime,
                stopTime);
        checkStatistics(data, 1.0, 21.0, 11.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|2|" + (BASE_TIME + 1) + "|12|" + (BASE_TIME + 2)
                + "|22\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "B", startTime,
                stopTime);
        checkStatistics(data, 2.0, 22.0, 12.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|3|" + (BASE_TIME + 1) + "|13|" + (BASE_TIME + 2)
                + "|23\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "C", startTime,
                stopTime);
        checkStatistics(data, 3.0, 23.0, 13.0, 10.0);

    }

    public void testCorrelatorULongLong() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        // Note that CORBA ulonglong means Java long.
        Any anyUp = create_any();
        long[] dataArrayUp1 = { 1, 2, 3 };
        long[] dataArrayUp2 = { 11, 12, 13 };
        long[] dataArrayUp3 = { 21, 22, 23 };
        long[][] dataUpMatrix = { dataArrayUp1, dataArrayUp2, dataArrayUp3 };
        String[] serialNumbers = { "A", "B", "C" };
        uLongLongSeqBlobData[] dataUp = new uLongLongSeqBlobData[dataUpMatrix.length];
        int index = 0;
        for (long[] value : dataUpMatrix) {
            dataUp[index] = new uLongLongSeqBlobData(BASE_TIME + index, value);
            index++;
        }
        uLongLongSeqBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, serialNumbers,
                "wrong:" + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;
        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|1|" + (BASE_TIME + 1) + "|11|"
                + (BASE_TIME + 2) + "|21\n";
        checkData(data, clob, 3, componentName, propertyName, "A", startTime,
                stopTime);
        checkStatistics(data, 1.0, 21.0, 11.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|2|" + (BASE_TIME + 1) + "|12|" + (BASE_TIME + 2)
                + "|22\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "B", startTime,
                stopTime);
        checkStatistics(data, 2.0, 22.0, 12.0, 10.0);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|3|" + (BASE_TIME + 1) + "|13|" + (BASE_TIME + 2)
                + "|23\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "C", startTime,
                stopTime);
        checkStatistics(data, 3.0, 23.0, 13.0, 10.0);

    }

    public void testCorrelatorString() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        // Note that CORBA ulonglong means Java long.
        Any anyUp = create_any();
        String[] dataArrayUp1 = { "R", "S", "T" };
        String[] dataArrayUp2 = { "U", "V", "W" };
        String[] dataArrayUp3 = { "X", "Y", "Z" };
        String[][] dataUpMatrix = { dataArrayUp1, dataArrayUp2, dataArrayUp3 };
        String[] serialNumbers = { "A", "B", "C" };
        stringSeqBlobData[] dataUp = new stringSeqBlobData[dataUpMatrix.length];
        int index = 0;
        for (String[] value : dataUpMatrix) {
            dataUp[index] = new stringSeqBlobData(BASE_TIME + index, value);
            index++;
        }
        stringSeqBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, serialNumbers,
                "wrong:" + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;
        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = BASE_TIME + "|R|" + (BASE_TIME + 1) + "|U|"
                + (BASE_TIME + 2) + "|X\n";
        checkData(data, clob, 3, componentName, propertyName, "A", startTime,
                stopTime);
        checkStatistics(data);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|S|" + (BASE_TIME + 1) + "|V|" + (BASE_TIME + 2)
                + "|Y\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "B", startTime,
                stopTime);
        checkStatistics(data);

        data = getTestWorker().fetchData();
        clob = BASE_TIME + "|T|" + (BASE_TIME + 1) + "|W|" + (BASE_TIME + 2)
                + "|Z\n";
        propertyName = "" + propertyName;
        checkData(data, clob, 3, componentName, propertyName, "C", startTime,
                stopTime);
        checkStatistics(data);

    }

    public void testTimestamps() throws Exception {
        blobber.setCollectorIntervalSeconds(1); // was 0, which is now illegal
        getTestWorker().setCanHandle(true);
        // Collector "TestCollector" is just a dummy to trick the worker to believe that 
        // there is a collector registered and start operating. "TestCollector" is not used at all.
        // The TestBlobberWorker creates its own single collector from which the blobber worker will get data. 
        getTestWorker().addCollector("TestCollector");

        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        long startTime1 = BASE_TIME + 2000000;
        long stopTime1 = BASE_TIME + 2010000;
        Any anyUp = create_any();
        int[] dataArrayUp1 = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        enumBlobData[] dataUp = new enumBlobData[dataArrayUp1.length];
        int index = 0;
        for (int value : dataArrayUp1) {
            dataUp[index] = new enumBlobData(startTime1 + index, value);
            index++;
        }
        enumBlobDataSeqHelper.insert(anyUp, dataUp);
        MonitorBlob blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        MonitorDataBlock block = new MonitorDataBlock(startTime1, stopTime1,
                componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        blobber.setCollectorIntervalSeconds(2);
        getTestWorker().getCollector().setMonitorData(blocks);

        long startTime2 = BASE_TIME + 4000000;
        long stopTime2 = BASE_TIME + 4010000;
        anyUp = create_any();
        int[] dataArrayUp2 = { 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
        dataUp = new enumBlobData[dataArrayUp2.length];
        index = 0;
        for (int value : dataArrayUp2) {
            dataUp[index] = new enumBlobData(startTime2 + index, value);
            index++;
        }
        enumBlobDataSeqHelper.insert(anyUp, dataUp);
        blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        block = new MonitorDataBlock(startTime2, stopTime2, componentName,
                serialNumber, blobs);
        blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        getTestWorker().getCollector().setMonitorData(blocks);

        ComponentData data = getTestWorker().fetchData();
        String clob = startTime1 + "|1|" + (startTime1 + 1) + "|2|"
                + (startTime1 + 2) + "|3|" + (startTime1 + 3) + "|4|"
                + (startTime1 + 4) + "|5|" + (startTime1 + 5) + "|6|"
                + (startTime1 + 6) + "|7|" + (startTime1 + 7) + "|8|"
                + (startTime1 + 8) + "|9|" + (startTime1 + 9) + "|10\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime1, stopTime1);
        checkStatistics(data);
        data = getTestWorker().fetchData();
        clob = startTime2 + "|11|" + (startTime2 + 1) + "|12|"
                + (startTime2 + 2) + "|13|" + (startTime2 + 3) + "|14|"
                + (startTime2 + 4) + "|15|" + (startTime2 + 5) + "|16|"
                + (startTime2 + 6) + "|17|" + (startTime2 + 7) + "|18|"
                + (startTime2 + 8) + "|19|" + (startTime2 + 9) + "|20\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber,
                startTime2, stopTime2);
        checkStatistics(data);

        long startTime3 = BASE_TIME + 6000000;
        long stopTime3 = BASE_TIME + 6010000;
        anyUp = create_any();
        int[] dataArrayUp3 = { 21, 22, 23, 24, 25, 26, 27, 28, 29, 20 };
        dataUp = new enumBlobData[dataArrayUp3.length];
        index = 0;
        for (int value : dataArrayUp3) {
            dataUp[index] = new enumBlobData(startTime3 + index, value);
            index++;
        }
        enumBlobDataSeqHelper.insert(anyUp, dataUp);
        blobUp = new MonitorBlob(false, (short) 0, null, "wrong:"
                + propertyName, anyUp);

        blobs = new MonitorBlob[1];
        blobs[0] = blobUp;

        block = new MonitorDataBlock(startTime3, stopTime3, componentName,
                serialNumber, blobs);
        blocks = new MonitorDataBlock[1];
        blocks[0] = block;
        //blobber.setCollectorIntervalSeconds(0);
        getTestWorker().getCollector().setMonitorData(blocks);

        data = getTestWorker().fetchData();
        clob = startTime3 + "|21|" + (startTime3 + 1) + "|22|"
                + (startTime3 + 2) + "|23|" + (startTime3 + 3) + "|24|"
                + (startTime3 + 4) + "|25|" + (startTime3 + 5) + "|26|"
                + (startTime3 + 6) + "|27|" + (startTime3 + 7) + "|28|"
                + (startTime3 + 8) + "|29|" + (startTime3 + 9) + "|20\n";
        checkData(data, clob, 10, componentName, propertyName, serialNumber, startTime3, stopTime3);
        checkStatistics(data);
    }

    /**
     * Test of extractData method
     * doubleBlobDataSeq
     *
     * @author pmerino@alma.cl
     */
    public void testExtractData_doubleBlobDataSeq() throws Exception {
        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "MODULE_MODE_STATUS";

	Any any = create_any();
        any.type(doubleBlobDataSeqHelper.type());
        double[] doubleDataArray = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
        doubleBlobData[] doubleBlobDataArray = createDoubleBlobData(doubleDataArray);
        doubleBlobDataSeqHelper.insert(any, doubleBlobDataArray);
        MonitorBlob blob = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, any);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blob;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[] {block};

        // Feeds the above test data to the mock monitor collector
        getTestWorker().getCollector().setMonitorData(blocks);

        // Reads data for the 4 properties from the test blobber worker.
        // Fetching the data blocks until data arrives from the mock collector in the next blobber cycle.
        m_logger.info("Will wait for blobber worker to read data from collector.");
        ComponentData componentData = getTestWorker().fetchData();
        String clob =
          BASE_TIME + "|1.0|" +
          (BASE_TIME + 1) + "|2.0|" +
          (BASE_TIME + 2) + "|3.0|" +
          (BASE_TIME + 3) + "|4.0|" +
          (BASE_TIME + 4) + "|5.0|" +
          (BASE_TIME + 5) + "|6.0|" +
          (BASE_TIME + 6) + "|7.0|" +
          (BASE_TIME + 7) + "|8.0|" +
          (BASE_TIME + 8) + "|9.0|" +
          (BASE_TIME + 9) + "|10.0\n";
        checkData(componentData, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        m_logger.info("Validated collector data retrieved from blobber worker.");

	// Create the expected result
        doubleBlobData[] blobDataMatrix = new doubleBlobData[10];
        blobDataMatrix[0] = new doubleBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new Double(1.0);
        blobDataMatrix[1] = new doubleBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new Double(2.0);
        blobDataMatrix[2] = new doubleBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new Double(3.0);
        blobDataMatrix[3] = new doubleBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new Double(4.0);
        blobDataMatrix[4] = new doubleBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new Double(5.0);
        blobDataMatrix[5] = new doubleBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new Double(6.0);
        blobDataMatrix[6] = new doubleBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new Double(7.0);
        blobDataMatrix[7] = new doubleBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new Double(8.0);
        blobDataMatrix[8] = new doubleBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new Double(9.0);
        blobDataMatrix[9] = new doubleBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new Double(10.0);
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (doubleBlobData blobData : blobDataMatrix)
            getTestWorker().populateContainerNumeric(container, blobData.time, (Number) blobData.value, index);
        outList.add(container);

	// Gets the real result
        List<AnyDataContainer> resultList = getTestWorker().extractData(any, propertyName);

        assertNotNull(resultList);
        assertEquals(outList, resultList);
    }

    /**
     * Test of extractData method
     * doubleSeqBlobDataSeq
     * monitorPointUnique
     *
     * @author pmerino@alma.cl
     */
    public void testExtractData_doubleSeqBlobData_monitorPointUnique() throws Exception {
        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "MODULE_MODE_STATUS";

        Any any = create_any();
        double[] doubleData1Array = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
        double[] doubleData2Array = { 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0 };
        double[] doubleData3Array = { 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0 };
        double[][] doubleDataMatrix = { doubleData1Array, doubleData2Array, doubleData3Array };
        String[] serialNumbers = { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J" };
        doubleSeqBlobData[] doubleSeqBlobDataArray = createDoubleSeqBlobData(doubleDataMatrix);
        doubleSeqBlobDataSeqHelper.insert(any, doubleSeqBlobDataArray);
        MonitorBlob blob = new MonitorBlob(false, (short) 0, serialNumbers, "wrong:" + propertyName, any);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blob;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[] {block};

        // Feeds the above test data to the mock monitor collector
        getTestWorker().getCollector().setMonitorData(blocks);

        // Reads data for the 4 properties from the test blobber worker.
        // Fetching the data blocks until data arrives from the mock collector in the next blobber cycle.
        m_logger.info("Will wait for blobber worker to read data from collector.");
        ComponentData componentData = getTestWorker().fetchData();
        String clob =
          BASE_TIME + "|1.0 11.0 21.0|" +
          (BASE_TIME + 1) + "|2.0 12.0 22.0|" +
          (BASE_TIME + 2) + "|3.0 13.0 23.0|" +
          (BASE_TIME + 3) + "|4.0 14.0 24.0|" +
          (BASE_TIME + 4) + "|5.0 15.0 25.0|" +
          (BASE_TIME + 5) + "|6.0 16.0 26.0|" +
          (BASE_TIME + 6) + "|7.0 17.0 27.0|" +
          (BASE_TIME + 7) + "|8.0 18.0 28.0|" +
          (BASE_TIME + 8) + "|9.0 19.0 29.0|" +
          (BASE_TIME + 9) + "|10.0 20.0 30.0\n";
        checkData(componentData, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        m_logger.info("Validated collector data retrieved from blobber worker.");

        // Create the expected result
        doubleSeqBlobData[] blobDataMatrix = new doubleSeqBlobData[10];
        blobDataMatrix[0] = new doubleSeqBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new double[3];
        blobDataMatrix[0].value[0] = 1.0;
        blobDataMatrix[0].value[1] = 11.0;
        blobDataMatrix[0].value[2] = 21.0;
        blobDataMatrix[1] = new doubleSeqBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new double[3];
        blobDataMatrix[1].value[0] = 2.0;
        blobDataMatrix[1].value[1] = 12.0;
        blobDataMatrix[1].value[2] = 22.0;
        blobDataMatrix[2] = new doubleSeqBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new double[3];
        blobDataMatrix[2].value[0] = 3.0;
        blobDataMatrix[2].value[1] = 13.0;
        blobDataMatrix[2].value[2] = 23.0;
        blobDataMatrix[3] = new doubleSeqBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new double[3];
        blobDataMatrix[3].value[0] = 4.0;
        blobDataMatrix[3].value[1] = 14.0;
        blobDataMatrix[3].value[2] = 24.0;
        blobDataMatrix[4] = new doubleSeqBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new double[3];
        blobDataMatrix[4].value[0] = 5.0;
        blobDataMatrix[4].value[1] = 15.0;
        blobDataMatrix[4].value[2] = 25.0;
        blobDataMatrix[5] = new doubleSeqBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new double[3];
        blobDataMatrix[5].value[0] = 6.0;
        blobDataMatrix[5].value[1] = 16.0;
        blobDataMatrix[5].value[2] = 26.0;
        blobDataMatrix[6] = new doubleSeqBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new double[3];
        blobDataMatrix[6].value[0] = 7.0;
        blobDataMatrix[6].value[1] = 17.0;
        blobDataMatrix[6].value[2] = 27.0;
        blobDataMatrix[7] = new doubleSeqBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new double[3];
        blobDataMatrix[7].value[0] = 8.0;
        blobDataMatrix[7].value[1] = 18.0;
        blobDataMatrix[7].value[2] = 28.0;
        blobDataMatrix[8] = new doubleSeqBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new double[3];
        blobDataMatrix[8].value[0] = 9.0;
        blobDataMatrix[8].value[1] = 19.0;
        blobDataMatrix[8].value[2] = 29.0;
        blobDataMatrix[9] = new doubleSeqBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new double[3];
        blobDataMatrix[9].value[0] = 10.0;
        blobDataMatrix[9].value[1] = 20.0;
        blobDataMatrix[9].value[2] = 30.0;
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (doubleSeqBlobData blobDataArray : blobDataMatrix)
            getTestWorker().populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
        outList.add(container);
        List<AnyDataContainer> resultList = getTestWorker().extractData(any, propertyName);

        assertNotNull(resultList);
        assertEquals(outList, resultList);
    }

    /**
     * Test of extractData method
     * floatBlobDataSeq
     *
     * @author pmerino@alma.cl
     */
    public void testExtractData_floatBlobDataSeq() throws Exception {
        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "MODULE_MODE_STATUS";

        Any any = create_any();
        any.type(floatBlobDataSeqHelper.type());
        float[] floatDataArray = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        floatBlobData[] floatBlobDataArray = createFloatBlobData(floatDataArray);
        floatBlobDataSeqHelper.insert(any, floatBlobDataArray);
        MonitorBlob blob = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, any);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blob;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[] {block};

        // Feeds the above test data to the mock monitor collector
        getTestWorker().getCollector().setMonitorData(blocks);

        // Reads data for the 4 properties from the test blobber worker.
        // Fetching the data blocks until data arrives from the mock collector in the next blobber cycle.
        m_logger.info("Will wait for blobber worker to read data from collector.");
        ComponentData componentData = getTestWorker().fetchData();
        String clob =
          BASE_TIME + "|1.0|" +
          (BASE_TIME + 1) + "|2.0|" +
          (BASE_TIME + 2) + "|3.0|" +
          (BASE_TIME + 3) + "|4.0|" +
          (BASE_TIME + 4) + "|5.0|" +
          (BASE_TIME + 5) + "|6.0|" +
          (BASE_TIME + 6) + "|7.0|" +
          (BASE_TIME + 7) + "|8.0|" +
          (BASE_TIME + 8) + "|9.0|" +
          (BASE_TIME + 9) + "|10.0\n";
        checkData(componentData, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        m_logger.info("Validated collector data retrieved from blobber worker.");

        // Create the expected result
        floatBlobData[] blobDataMatrix = new floatBlobData[10];
        blobDataMatrix[0] = new floatBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new Float(1.0);
        blobDataMatrix[1] = new floatBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new Float(2.0);
        blobDataMatrix[2] = new floatBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new Float(3.0);
        blobDataMatrix[3] = new floatBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new Float(4.0);
        blobDataMatrix[4] = new floatBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new Float(5.0);
        blobDataMatrix[5] = new floatBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new Float(6.0);
        blobDataMatrix[6] = new floatBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new Float(7.0);
        blobDataMatrix[7] = new floatBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new Float(8.0);
        blobDataMatrix[8] = new floatBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new Float(9.0);
        blobDataMatrix[9] = new floatBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new Float(10.0);
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (floatBlobData blobDataArray : blobDataMatrix)
            getTestWorker().populateContainerNumeric(container, blobDataArray.time, blobDataArray.value, index);
        outList.add(container);
        List<AnyDataContainer> resultList = getTestWorker().extractData(any, propertyName);

        assertNotNull(resultList);
        assertEquals(outList, resultList);
    }

    /**
     * Test of extractData method
     * floatSeqBlobDataSeq
     * monitorPointUnique
     *
     * @author pmerino@alma.cl
     */
    public void testExtractData_floatSeqBlobDataSeq_monitorPointUnique() throws Exception {
        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "MODULE_MODE_STATUS";

        Any any = create_any();
        float[] floatData1Array = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        float[] floatData2Array = { 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
        float[] floatData3Array = { 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 };
        float[][] floatDataMatrix = { floatData1Array, floatData2Array, floatData3Array };
        String[] serialNumbers = { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J" };
        floatSeqBlobData[] floatSeqBlobDataArray = createFloatSeqBlobData(floatDataMatrix);
        floatSeqBlobDataSeqHelper.insert(any, floatSeqBlobDataArray);
        MonitorBlob blob = new MonitorBlob(false, (short) 0, serialNumbers, "wrong:" + propertyName, any);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blob;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[] {block};

        // Feeds the above test data to the mock monitor collector
        getTestWorker().getCollector().setMonitorData(blocks);

        // Reads data for the 4 properties from the test blobber worker.
        // Fetching the data blocks until data arrives from the mock collector in the next blobber cycle.
        m_logger.info("Will wait for blobber worker to read data from collector.");
        ComponentData componentData = getTestWorker().fetchData();
        String clob =
          BASE_TIME + "|1.0 11.0 21.0|" +
          (BASE_TIME + 1) + "|2.0 12.0 22.0|" +
          (BASE_TIME + 2) + "|3.0 13.0 23.0|" +
          (BASE_TIME + 3) + "|4.0 14.0 24.0|" +
          (BASE_TIME + 4) + "|5.0 15.0 25.0|" +
          (BASE_TIME + 5) + "|6.0 16.0 26.0|" +
          (BASE_TIME + 6) + "|7.0 17.0 27.0|" +
          (BASE_TIME + 7) + "|8.0 18.0 28.0|" +
          (BASE_TIME + 8) + "|9.0 19.0 29.0|" +
          (BASE_TIME + 9) + "|10.0 20.0 30.0\n";
        checkData(componentData, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        m_logger.info("Validated collector data retrieved from blobber worker.");

        // Create the expected result
        floatSeqBlobData[] blobDataMatrix = new floatSeqBlobData[10];
        blobDataMatrix[0] = new floatSeqBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new float[3];
        blobDataMatrix[0].value[0] = 1;
        blobDataMatrix[0].value[1] = 11;
        blobDataMatrix[0].value[2] = 21;
        blobDataMatrix[1] = new floatSeqBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new float[3];
        blobDataMatrix[1].value[0] = 2;
        blobDataMatrix[1].value[1] = 12;
        blobDataMatrix[1].value[2] = 22;
        blobDataMatrix[2] = new floatSeqBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new float[3];
        blobDataMatrix[2].value[0] = 3;
        blobDataMatrix[2].value[1] = 13;
        blobDataMatrix[2].value[2] = 23;
        blobDataMatrix[3] = new floatSeqBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new float[3];
        blobDataMatrix[3].value[0] = 4;
        blobDataMatrix[3].value[1] = 14;
        blobDataMatrix[3].value[2] = 24;
        blobDataMatrix[4] = new floatSeqBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new float[3];
        blobDataMatrix[4].value[0] = 5;
        blobDataMatrix[4].value[1] = 15;
        blobDataMatrix[4].value[2] = 25;
        blobDataMatrix[5] = new floatSeqBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new float[3];
        blobDataMatrix[5].value[0] = 6;
        blobDataMatrix[5].value[1] = 16;
        blobDataMatrix[5].value[2] = 26;
        blobDataMatrix[6] = new floatSeqBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new float[3];
        blobDataMatrix[6].value[0] = 7;
        blobDataMatrix[6].value[1] = 17;
        blobDataMatrix[6].value[2] = 27;
        blobDataMatrix[7] = new floatSeqBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new float[3];
        blobDataMatrix[7].value[0] = 8;
        blobDataMatrix[7].value[1] = 18;
        blobDataMatrix[7].value[2] = 28;
        blobDataMatrix[8] = new floatSeqBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new float[3];
        blobDataMatrix[8].value[0] = 9;
        blobDataMatrix[8].value[1] = 19;
        blobDataMatrix[8].value[2] = 29;
        blobDataMatrix[9] = new floatSeqBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new float[3];
        blobDataMatrix[9].value[0] = 10;
        blobDataMatrix[9].value[1] = 20;
        blobDataMatrix[9].value[2] = 30;
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (floatSeqBlobData blobDataArray : blobDataMatrix)
            getTestWorker().populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
        outList.add(container);
        List<AnyDataContainer> resultList = getTestWorker().extractData(any, propertyName);

        assertNotNull(resultList);
        assertEquals(outList, resultList);
    }

    /**
     * Test of extractData method
     * doubleSeqBlobDataSeq
     * monitorPointNotUnique
     *
     * @author pmerino@alma.cl
     */
    public void testExtractData_doubleSeqBlobData_monitorPointNotUnique() throws Exception {
        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "SYSTEM_STATUS";

        Any any = create_any();
        any.type(doubleBlobDataSeqHelper.type());
        double[] doubleDataArray = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
        doubleBlobData[] doubleBlobDataArray = createDoubleBlobData(doubleDataArray);
        doubleBlobDataSeqHelper.insert(any, doubleBlobDataArray);
        MonitorBlob blob = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, any);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blob;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[] {block};

        // Feeds the above test data to the mock monitor collector
        getTestWorker().getCollector().setMonitorData(blocks);

        // Reads data for the 4 properties from the test blobber worker.
        // Fetching the data blocks until data arrives from the mock collector in the next blobber cycle.
        m_logger.info("Will wait for blobber worker to read data from collector.");
        ComponentData componentData = getTestWorker().fetchData();
        String clob =
          BASE_TIME + "|1.0|" +
          (BASE_TIME + 1) + "|2.0|" +
          (BASE_TIME + 2) + "|3.0|" +
          (BASE_TIME + 3) + "|4.0|" +
          (BASE_TIME + 4) + "|5.0|" +
          (BASE_TIME + 5) + "|6.0|" +
          (BASE_TIME + 6) + "|7.0|" +
          (BASE_TIME + 7) + "|8.0|" +
          (BASE_TIME + 8) + "|9.0|" +
          (BASE_TIME + 9) + "|10.0\n";
        checkData(componentData, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        m_logger.info("Validated collector data retrieved from blobber worker.");

        // Create the expected result
        doubleBlobData[] blobDataMatrix = new doubleBlobData[10];
        blobDataMatrix[0] = new doubleBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new Double(1.0);
        blobDataMatrix[1] = new doubleBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new Double(2.0);
        blobDataMatrix[2] = new doubleBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new Double(3.0);
        blobDataMatrix[3] = new doubleBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new Double(4.0);
        blobDataMatrix[4] = new doubleBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new Double(5.0);
        blobDataMatrix[5] = new doubleBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new Double(6.0);
        blobDataMatrix[6] = new doubleBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new Double(7.0);
        blobDataMatrix[7] = new doubleBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new Double(8.0);
        blobDataMatrix[8] = new doubleBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new Double(9.0);
        blobDataMatrix[9] = new doubleBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new Double(10.0);
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (doubleBlobData blobData : blobDataMatrix)
            getTestWorker().populateContainerNumeric(container, blobData.time, (Number) blobData.value, index);
        outList.add(container);

        // Gets the real result
        List<AnyDataContainer> resultList = getTestWorker().extractData(any, propertyName);

        assertNotNull(resultList);
        assertEquals(outList, resultList);
    }

    /**
     * Test of extractData method
     * longBlobDataSeq
     *
     * @author pmerino@alma.cl
     */
    public void testExtractData_longBlobDataSeq() throws Exception {
        String componentName = "CONTROL/DV01/PSA";
        String serialNumber = "3456328928847";
        String propertyName = "VOLTAGE_MID_1";

        Any any = create_any();
        any.type(longBlobDataSeqHelper.type());
        long[] longDataArray = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        longBlobData[] longBlobDataArray = createLongBlobData(longDataArray);
        longBlobDataSeqHelper.insert(any, longBlobDataArray);
        MonitorBlob blob = new MonitorBlob(false, (short) 0, null, "wrong:" + propertyName, any);

        MonitorBlob[] blobs = new MonitorBlob[1];
        blobs[0] = blob;

        long startTime = BASE_TIME + 100;
        long stopTime = BASE_TIME + 101;
        MonitorDataBlock block = new MonitorDataBlock(startTime, stopTime, componentName, serialNumber, blobs);
        MonitorDataBlock[] blocks = new MonitorDataBlock[] {block};

        // Feeds the above test data to the mock monitor collector
        getTestWorker().getCollector().setMonitorData(blocks);

        // Reads data for the 4 properties from the test blobber worker.
        // Fetching the data blocks until data arrives from the mock collector in the next blobber cycle.
        m_logger.info("Will wait for blobber worker to read data from collector.");
        ComponentData componentData = getTestWorker().fetchData();
        String clob =
          BASE_TIME + "|1.0|" +
          (BASE_TIME + 1) + "|2.0|" +
          (BASE_TIME + 2) + "|3.0|" +
          (BASE_TIME + 3) + "|4.0|" +
          (BASE_TIME + 4) + "|5.0|" +
          (BASE_TIME + 5) + "|6.0|" +
          (BASE_TIME + 6) + "|7.0|" +
          (BASE_TIME + 7) + "|8.0|" +
          (BASE_TIME + 8) + "|9.0|" +
          (BASE_TIME + 9) + "|10.0\n";
        checkData(componentData, clob, 10, componentName, propertyName, serialNumber, startTime, stopTime);
        m_logger.info("Validated collector data retrieved from blobber worker.");

        // Create the expected result
        longBlobData[] blobDataMatrix = new longBlobData[10];
        blobDataMatrix[0] = new longBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = 1;
        blobDataMatrix[1] = new longBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = 2;
        blobDataMatrix[2] = new longBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = 3;
        blobDataMatrix[3] = new longBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = 4;
        blobDataMatrix[4] = new longBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = 5;
        blobDataMatrix[5] = new longBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = 6;
        blobDataMatrix[6] = new longBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = 7;
        blobDataMatrix[7] = new longBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = 8;
        blobDataMatrix[8] = new longBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = 9;
        blobDataMatrix[9] = new longBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = 10;
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (longBlobData blobDataArray : blobDataMatrix)
            getTestWorker().populateContainerNumeric(container, blobDataArray.time, blobDataArray.value, index);
        outList.add(container);
        List<AnyDataContainer> resultList = getTestWorker().extractData(any, propertyName);

        assertNotNull(resultList);
        assertEquals(outList, resultList);
    }

    private void checkStatistics(ComponentData inData) {
        assert inData.statistics == null : "Statistical data found but not expected.";
    }

    private void checkStatistics(ComponentData inData, double min, double max, double mean, double stdDev) {
        assertNotNull("Statistical data expected but not supplied.", inData.statistics);
        assertEquals("Incorrect min: Found [" + inData.statistics.min + "] Expected [" + min + "] (excluding [] in both statements.)",
        		min, inData.statistics.min.doubleValue(), 0.000000001);
        assertEquals("Incorrect max: Found [" + inData.statistics.max + "] Expected [" + max + "] (excluding [] in both statements.)",
        		max, inData.statistics.max.doubleValue(), 0.000000001);
        assertEquals("Incorrect mean: Found [" + inData.statistics.mean + "] Expected [" + mean + "] (excluding [] in both statements.)",
        		mean, inData.statistics.mean.doubleValue(), 0.000000001);
        assertEquals("Incorrect stdDev: Found [" + inData.statistics.stdDev + "] Expected [" + stdDev + "] (excluding [] in both statements.)",
        		stdDev, inData.statistics.stdDev.doubleValue(), 0.000000001);
        m_logger.fine("Passed checkStatistics for min=" + min + " max=" + max + " mean=" + mean);
    }

    private void checkData(ComponentData inData, String clob, int sampleSize,
            String componentName, String propertyName, String serialNumber,
            long startTime, long stopTime) {
        assert clob.equals(inData.clob) : "\nIncorrect clob\nFound    ["
                + inData.clob + "]\nExpected [" + clob
                + "] (excluding [] in both statements.)";
        assert inData.sampleSize == sampleSize : "\nIncorrect sampleSize\nFound    ["
                + inData.sampleSize
                + "]\nExpected ["
                + sampleSize
                + "] (excluding [] in both statements.)";
        assert componentName.equals(inData.componentName) : "\nIncorrect componentName\nFound    ["
                + inData.componentName
                + "]\nExpected ["
                + componentName
                + "] (excluding [] in both statements.)";
        assert propertyName.equals(inData.propertyName) : "\nIncorrect propertyName\nFound    ["
                + inData.propertyName
                + "]\nExpected ["
                + propertyName
                + "] (excluding [] in both statements.)";
        assert serialNumber.equals(inData.serialNumber) : "\nIncorrect serialNumber\nFound    ["
                + inData.serialNumber
                + "]\nExpected ["
                + serialNumber
                + "] (excluding [] in both statements.)";
        assert inData.startTime == startTime : "\nIncorrect startTime\nFound    ["
                + inData.startTime
                + "]\nExpected ["
                + startTime
                + "] (excluding [] in both statements.)";
        assert inData.stopTime == stopTime : "\nIncorrect stopTime\nFound    ["
                + inData.stopTime + "]\nExpected [" + stopTime
                + "] (excluding [] in both statements.)";
    }
    
    /**
     * Test the populateContainerNumericArray method
     */
    public void testPopulateContainerNumericArrayFloat() {
        // Create the expected result
        floatSeqBlobData[] blobDataMatrix = new floatSeqBlobData[10];
        blobDataMatrix[0] = new floatSeqBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new float[3];
        blobDataMatrix[0].value[0] = 1;
        blobDataMatrix[0].value[1] = 11;
        blobDataMatrix[0].value[2] = 21;
        blobDataMatrix[1] = new floatSeqBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new float[3];
        blobDataMatrix[1].value[0] = 2;
        blobDataMatrix[1].value[1] = 12;
        blobDataMatrix[1].value[2] = 22;
        blobDataMatrix[2] = new floatSeqBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new float[3];
        blobDataMatrix[2].value[0] = 3;
        blobDataMatrix[2].value[1] = 13;
        blobDataMatrix[2].value[2] = 23;
        blobDataMatrix[3] = new floatSeqBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new float[3];
        blobDataMatrix[3].value[0] = 4;
        blobDataMatrix[3].value[1] = 14;
        blobDataMatrix[3].value[2] = 24;
        blobDataMatrix[4] = new floatSeqBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new float[3];
        blobDataMatrix[4].value[0] = 5;
        blobDataMatrix[4].value[1] = 15;
        blobDataMatrix[4].value[2] = 25;
        blobDataMatrix[5] = new floatSeqBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new float[3];
        blobDataMatrix[5].value[0] = 6;
        blobDataMatrix[5].value[1] = 16;
        blobDataMatrix[5].value[2] = 26;
        blobDataMatrix[6] = new floatSeqBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new float[3];
        blobDataMatrix[6].value[0] = 7;
        blobDataMatrix[6].value[1] = 17;
        blobDataMatrix[6].value[2] = 27;
        blobDataMatrix[7] = new floatSeqBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new float[3];
        blobDataMatrix[7].value[0] = 8;
        blobDataMatrix[7].value[1] = 18;
        blobDataMatrix[7].value[2] = 28;
        blobDataMatrix[8] = new floatSeqBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new float[3];
        blobDataMatrix[8].value[0] = 9;
        blobDataMatrix[8].value[1] = 19;
        blobDataMatrix[8].value[2] = 29;
        blobDataMatrix[9] = new floatSeqBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new float[3];
        blobDataMatrix[9].value[0] = 10;
        blobDataMatrix[9].value[1] = 20;
        blobDataMatrix[9].value[2] = 30;
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (floatSeqBlobData blobDataArray : blobDataMatrix)
            getTestWorker().populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
        outList.add(container);
        
        assertEquals("1 11 21", container.dataList.get(0));
        assertEquals("2 12 22", container.dataList.get(1));
        assertEquals("3 13 23", container.dataList.get(2));
        assertEquals("4 14 24", container.dataList.get(3));
        assertEquals("5 15 25", container.dataList.get(4));
        assertEquals("6 16 26", container.dataList.get(5));
        assertEquals("7 17 27", container.dataList.get(6));
        assertEquals("8 18 28", container.dataList.get(7));
        assertEquals("9 19 29", container.dataList.get(8));
        assertEquals("10 20 30", container.dataList.get(9));
        assertEquals(BASE_TIME + "|1 11 21|" +
            (BASE_TIME + 1) + "|2 12 22|" +
            (BASE_TIME + 2) + "|3 13 23|" +
            (BASE_TIME + 3) + "|4 14 24|" +
            (BASE_TIME + 4) + "|5 15 25|" +
            (BASE_TIME + 5) + "|6 16 26|" +
            (BASE_TIME + 6) + "|7 17 27|" +
            (BASE_TIME + 7) + "|8 18 28|" +
            (BASE_TIME + 8) + "|9 19 29|" +
            (BASE_TIME + 9) + "|10 20 30|", container.clobBuilder);
    }

    public void testPopulateContainerNumericArrayDouble() {
        // Create the expected result
        doubleSeqBlobData[] blobDataMatrix = new doubleSeqBlobData[10];
        blobDataMatrix[0] = new doubleSeqBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new double[3];
        blobDataMatrix[0].value[0] = 1.0;
        blobDataMatrix[0].value[1] = 11.0;
        blobDataMatrix[0].value[2] = 21.0;
        blobDataMatrix[1] = new doubleSeqBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new double[3];
        blobDataMatrix[1].value[0] = 2.0;
        blobDataMatrix[1].value[1] = 12.0;
        blobDataMatrix[1].value[2] = 22.0;
        blobDataMatrix[2] = new doubleSeqBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new double[3];
        blobDataMatrix[2].value[0] = 3.0;
        blobDataMatrix[2].value[1] = 13.0;
        blobDataMatrix[2].value[2] = 23.0;
        blobDataMatrix[3] = new doubleSeqBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new double[3];
        blobDataMatrix[3].value[0] = 4.0;
        blobDataMatrix[3].value[1] = 14.0;
        blobDataMatrix[3].value[2] = 24.0;
        blobDataMatrix[4] = new doubleSeqBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new double[3];
        blobDataMatrix[4].value[0] = 5.0;
        blobDataMatrix[4].value[1] = 15.0;
        blobDataMatrix[4].value[2] = 25.0;
        blobDataMatrix[5] = new doubleSeqBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new double[3];
        blobDataMatrix[5].value[0] = 6.0;
        blobDataMatrix[5].value[1] = 16.0;
        blobDataMatrix[5].value[2] = 26.0;
        blobDataMatrix[6] = new doubleSeqBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new double[3];
        blobDataMatrix[6].value[0] = 7.0;
        blobDataMatrix[6].value[1] = 17.0;
        blobDataMatrix[6].value[2] = 27.0;
        blobDataMatrix[7] = new doubleSeqBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new double[3];
        blobDataMatrix[7].value[0] = 8.0;
        blobDataMatrix[7].value[1] = 18.0;
        blobDataMatrix[7].value[2] = 28.0;
        blobDataMatrix[8] = new doubleSeqBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new double[3];
        blobDataMatrix[8].value[0] = 9.0;
        blobDataMatrix[8].value[1] = 19.0;
        blobDataMatrix[8].value[2] = 29.0;
        blobDataMatrix[9] = new doubleSeqBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new double[3];
        blobDataMatrix[9].value[0] = 10.0;
        blobDataMatrix[9].value[1] = 20.0;
        blobDataMatrix[9].value[2] = 30.0;
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (doubleSeqBlobData blobDataArray : blobDataMatrix)
            getTestWorker().populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
        outList.add(container);

        assertEquals("1.0 11.0 21.0", container.dataList.get(0));
        assertEquals("2.0 12.0 22.0", container.dataList.get(1));
        assertEquals("3.0 13.0 23.0", container.dataList.get(2));
        assertEquals("4.0 14.0 24.0", container.dataList.get(3));
        assertEquals("5.0 15.0 25.0", container.dataList.get(4));
        assertEquals("6.0 16.0 26.0", container.dataList.get(5));
        assertEquals("7.0 17.0 27.0", container.dataList.get(6));
        assertEquals("8.0 18.0 28.0", container.dataList.get(7));
        assertEquals("9.0 19.0 29.0", container.dataList.get(8));
        assertEquals("10.0 20.0 30.0", container.dataList.get(9));
        assertEquals(BASE_TIME + "|1.0 11.0 21.0|" +
            (BASE_TIME + 1) + "|2.0 12.0 22.0|" +
            (BASE_TIME + 2) + "|3.0 13.0 23.0|" +
            (BASE_TIME + 3) + "|4.0 14.0 24.0|" +
            (BASE_TIME + 4) + "|5.0 15.0 25.0|" +
            (BASE_TIME + 5) + "|6.0 16.0 26.0|" +
            (BASE_TIME + 6) + "|7.0 17.0 27.0|" +
            (BASE_TIME + 7) + "|8.0 18.0 28.0|" +
            (BASE_TIME + 8) + "|9.0 19.0 29.0|" +
            (BASE_TIME + 9) + "|10.0 20.0 30.0|", container.clobBuilder);
    }
    
    public void testPopulateContainerNumericArrayLong() {
        // Create the expected result
        longSeqBlobData[] blobDataMatrix = new longSeqBlobData[10];
        blobDataMatrix[0] = new longSeqBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new int[3];
        blobDataMatrix[0].value[0] = 1;
        blobDataMatrix[0].value[1] = 11;
        blobDataMatrix[0].value[2] = 21;
        blobDataMatrix[1] = new longSeqBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new int[3];
        blobDataMatrix[1].value[0] = 2;
        blobDataMatrix[1].value[1] = 12;
        blobDataMatrix[1].value[2] = 22;
        blobDataMatrix[2] = new longSeqBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new int[3];
        blobDataMatrix[2].value[0] = 3;
        blobDataMatrix[2].value[1] = 13;
        blobDataMatrix[2].value[2] = 23;
        blobDataMatrix[3] = new longSeqBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new int[3];
        blobDataMatrix[3].value[0] = 4;
        blobDataMatrix[3].value[1] = 14;
        blobDataMatrix[3].value[2] = 24;
        blobDataMatrix[4] = new longSeqBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new int[3];
        blobDataMatrix[4].value[0] = 5;
        blobDataMatrix[4].value[1] = 15;
        blobDataMatrix[4].value[2] = 25;
        blobDataMatrix[5] = new longSeqBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new int[3];
        blobDataMatrix[5].value[0] = 6;
        blobDataMatrix[5].value[1] = 16;
        blobDataMatrix[5].value[2] = 26;
        blobDataMatrix[6] = new longSeqBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new int[3];
        blobDataMatrix[6].value[0] = 7;
        blobDataMatrix[6].value[1] = 17;
        blobDataMatrix[6].value[2] = 27;
        blobDataMatrix[7] = new longSeqBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new int[3];
        blobDataMatrix[7].value[0] = 8;
        blobDataMatrix[7].value[1] = 18;
        blobDataMatrix[7].value[2] = 28;
        blobDataMatrix[8] = new longSeqBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new int[3];
        blobDataMatrix[8].value[0] = 9;
        blobDataMatrix[8].value[1] = 19;
        blobDataMatrix[8].value[2] = 29;
        blobDataMatrix[9] = new longSeqBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new int[3];
        blobDataMatrix[9].value[0] = 10;
        blobDataMatrix[9].value[1] = 20;
        blobDataMatrix[9].value[2] = 30;
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (longSeqBlobData blobDataArray : blobDataMatrix)
            getTestWorker().populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
        outList.add(container);

        assertEquals("1 11 21", container.dataList.get(0));
        assertEquals("2 12 22", container.dataList.get(1));
        assertEquals("3 13 23", container.dataList.get(2));
        assertEquals("4 14 24", container.dataList.get(3));
        assertEquals("5 15 25", container.dataList.get(4));
        assertEquals("6 16 26", container.dataList.get(5));
        assertEquals("7 17 27", container.dataList.get(6));
        assertEquals("8 18 28", container.dataList.get(7));
        assertEquals("9 19 29", container.dataList.get(8));
        assertEquals("10 20 30", container.dataList.get(9));
        assertEquals(BASE_TIME + "|1 11 21|" +
            (BASE_TIME + 1) + "|2 12 22|" +
            (BASE_TIME + 2) + "|3 13 23|" +
            (BASE_TIME + 3) + "|4 14 24|" +
            (BASE_TIME + 4) + "|5 15 25|" +
            (BASE_TIME + 5) + "|6 16 26|" +
            (BASE_TIME + 6) + "|7 17 27|" +
            (BASE_TIME + 7) + "|8 18 28|" +
            (BASE_TIME + 8) + "|9 19 29|" +
            (BASE_TIME + 9) + "|10 20 30|", container.clobBuilder);
    }

    public void testPopulateContainerNumericArrayBoolean() {
        // Create the expected result
        booleanSeqBlobData[] blobDataMatrix = new booleanSeqBlobData[10];
        blobDataMatrix[0] = new booleanSeqBlobData();
        blobDataMatrix[0].time = BASE_TIME;
        blobDataMatrix[0].value = new boolean[3];
        blobDataMatrix[0].value[0] = true;
        blobDataMatrix[0].value[1] = false;
        blobDataMatrix[0].value[2] = false;
        blobDataMatrix[1] = new booleanSeqBlobData();
        blobDataMatrix[1].time = BASE_TIME + 1;
        blobDataMatrix[1].value = new boolean[3];
        blobDataMatrix[1].value[0] = true;
        blobDataMatrix[1].value[1] = true;
        blobDataMatrix[1].value[2] = false;
        blobDataMatrix[2] = new booleanSeqBlobData();
        blobDataMatrix[2].time = BASE_TIME + 2;
        blobDataMatrix[2].value = new boolean[3];
        blobDataMatrix[2].value[0] = true;
        blobDataMatrix[2].value[1] = true;
        blobDataMatrix[2].value[2] = true;
        blobDataMatrix[3] = new booleanSeqBlobData();
        blobDataMatrix[3].time = BASE_TIME + 3;
        blobDataMatrix[3].value = new boolean[3];
        blobDataMatrix[3].value[0] = false;
        blobDataMatrix[3].value[1] = true;
        blobDataMatrix[3].value[2] = false;
        blobDataMatrix[4] = new booleanSeqBlobData();
        blobDataMatrix[4].time = BASE_TIME + 4;
        blobDataMatrix[4].value = new boolean[3];
        blobDataMatrix[4].value[0] = false;
        blobDataMatrix[4].value[1] = true;
        blobDataMatrix[4].value[2] = true;
        blobDataMatrix[5] = new booleanSeqBlobData();
        blobDataMatrix[5].time = BASE_TIME + 5;
        blobDataMatrix[5].value = new boolean[3];
        blobDataMatrix[5].value[0] = false;
        blobDataMatrix[5].value[1] = false;
        blobDataMatrix[5].value[2] = true;
        blobDataMatrix[6] = new booleanSeqBlobData();
        blobDataMatrix[6].time = BASE_TIME + 6;
        blobDataMatrix[6].value = new boolean[3];
        blobDataMatrix[6].value[0] = false;
        blobDataMatrix[6].value[1] = false;
        blobDataMatrix[6].value[2] = false;
        blobDataMatrix[7] = new booleanSeqBlobData();
        blobDataMatrix[7].time = BASE_TIME + 7;
        blobDataMatrix[7].value = new boolean[3];
        blobDataMatrix[7].value[0] = false;
        blobDataMatrix[7].value[1] = false;
        blobDataMatrix[7].value[2] = true;
        blobDataMatrix[8] = new booleanSeqBlobData();
        blobDataMatrix[8].time = BASE_TIME + 8;
        blobDataMatrix[8].value = new boolean[3];
        blobDataMatrix[8].value[0] = true;
        blobDataMatrix[8].value[1] = false;
        blobDataMatrix[8].value[2] = true;
        blobDataMatrix[9] = new booleanSeqBlobData();
        blobDataMatrix[9].time = BASE_TIME + 9;
        blobDataMatrix[9].value = new boolean[3];
        blobDataMatrix[9].value[0] = true;
        blobDataMatrix[9].value[1] = true;
        blobDataMatrix[9].value[2] = true;
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        AnyDataContainer container = new AnyDataContainer();
        int index = 0;
        for (booleanSeqBlobData blobDataArray : blobDataMatrix)
            getTestWorker().populateContainerBooleanArray(container, blobDataArray.time, blobDataArray.value, index);
        outList.add(container);

        assertEquals("1 0 0", container.dataList.get(0));
        assertEquals("1 1 0", container.dataList.get(1));
        assertEquals("1 1 1", container.dataList.get(2));
        assertEquals("0 1 0", container.dataList.get(3));
        assertEquals("0 1 1", container.dataList.get(4));
        assertEquals("0 0 1", container.dataList.get(5));
        assertEquals("0 0 1", container.dataList.get(6));
        assertEquals("0 0 1", container.dataList.get(7));
        assertEquals("1 0 1", container.dataList.get(8));
        assertEquals("1 1 1", container.dataList.get(9));
        assertEquals(BASE_TIME + "|1 0 0|" +
            (BASE_TIME + 1) + "|1 1 0|" +
            (BASE_TIME + 2) + "|1 1 1|" +
            (BASE_TIME + 3) + "|0 1 0|" +
            (BASE_TIME + 4) + "|0 1 1|" +
            (BASE_TIME + 5) + "|0 0 1|" +
            (BASE_TIME + 6) + "|0 0 0|" +
            (BASE_TIME + 7) + "|0 0 1|" +
            (BASE_TIME + 8) + "|1 0 1|" +
            (BASE_TIME + 9) + "|1 1 1|", container.clobBuilder);
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

    private floatBlobData[] createFloatBlobData(float[] inValue) {
        floatBlobData[] outArray = new floatBlobData[inValue.length];
        int index = 0;
        for (float value : inValue) {
            outArray[index] = new floatBlobData(BASE_TIME + index, value);
            index++;
        }
        return outArray;
    }

    private longBlobData[] createLongBlobData(long[] inValue) {
        longBlobData[] outArray = new longBlobData[inValue.length];
        int index = 0;
        for (long value : inValue) {
            outArray[index] = new longBlobData(BASE_TIME + index, (new Long(value)).intValue());
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

    private floatSeqBlobData[] createFloatSeqBlobData(float[][] inValue) {
        floatSeqBlobData[] array = new floatSeqBlobData[inValue.length];
        int index = 0;
        for (float[] value : inValue) {
            array[index] = new floatSeqBlobData(BASE_TIME + index, value);
            index++;
        }
        return array;
    }

    private Any create_any() {
    	return getContainerServices().getAdvancedContainerServices().getAny();
	}
    
    public static void main(String[] args) {
       junit.textui.TestRunner.run(BlobberWorkerUnitTest.class);
    }
}
