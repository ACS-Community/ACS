package alma.acs.monitoring.blobber;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.sql.BatchUpdateException;

import org.exolab.castor.util.Iterator;
import org.omg.CORBA.Any;

import alma.MonitorArchiver.CollectorListStatus;
import alma.TMCDB.MonitorBlob;
import alma.TMCDB.MonitorCollector;
import alma.TMCDB.MonitorCollectorHelper;
import alma.TMCDB.MonitorDataBlock;
import alma.TMCDB.anyBlobData;
import alma.TMCDB.anyBlobDataSeqHelper;
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
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.DAO.ComponentStatistics;
import alma.acs.monitoring.DAO.MonitorDAO;
import alma.archive.tmcdb.DAO.MonitorDAOImpl;
import alma.acs.monitoring.blobber.CollectorList.BlobData;
import alma.acs.monitoring.blobber.CollectorList.CollectorData;

public class BlobberWorker implements Runnable {

    /**
     * Default interval between collecting data.
     */
    private static final long COLLECT_INTERVAL = 60;

    private static final int MAX_COLLECTOR_COUNT = 6;

    private boolean myTerminate = false;

    protected MonitorDAO myMonitorDAO;

    private CollectorList myCollectorList = new CollectorList();

    private ContainerServices myContainerServices;
    protected Logger myLogger;

    private long myStartTime;

    private Integer myMaxCollectorCount = MAX_COLLECTOR_COUNT;

    protected Long myCollectInterval = 0L;

    private HashMap<String, Object> loggedFailedStoreMap = new HashMap<String, Object>();

    public BlobberWorker(ContainerServices inContainerServices) {
        this.myContainerServices = inContainerServices;
        initWorker();
        initMonitorDAO();
    }

    protected void initWorker() {
        this.myLogger = myContainerServices.getLogger();
        setCollectInterval(COLLECT_INTERVAL);
    }

    protected void initMonitorDAO() {
        this.myLogger.fine("Initializing MonitorDAO.");
        this.myMonitorDAO = new MonitorDAOImpl(this.myLogger);
        //this.myMonitorDAO = null;
    }

    protected boolean canHandle() {
        return this.myCollectorList.size() < getMaxCollectorCount();
    }

    protected int getMaxCollectorCount() {
        synchronized (this.myMaxCollectorCount) {
            return this.myMaxCollectorCount;
        }
    }

    protected void setMaxCollectorCount(int inCount) {
        if (inCount < 0) {
            throw new IllegalArgumentException(inCount + " must be above 0.");
        }
        synchronized (this.myMaxCollectorCount) {
            this.myMaxCollectorCount = inCount;
        }
    }

    /**
     * Adds the stated collector from the blobber. If the collector is already
     * known to the blobber it will not be added again. This method will however
     * NOT wait for this, it returns immediately with one exception. When the
     * second collector is added it is first ensured that one access has
     * completed to the firstly added collector in order to wait for the access
     * statistics needed to calculate the capacity of the blobber.
     *
     * @param inCollectorComponentId a String stating the name of the collector to remove.
     * @return a CollectorListStatus stating the result. Values to expect are
     *         {@link CollectorListStatus#ADDED},
     *         {@link CollectorListStatus#KNOWN} or
     *         {@link CollectorListStatus#FULL}.
     */
    public CollectorListStatus addCollector(String inCollectorComponentId) {
        this.myLogger.fine("Trying to add collector " + inCollectorComponentId);
        CollectorListStatus outStatus = CollectorListStatus.FULL;
        try {
            if (canHandle()) {
                outStatus = this.myCollectorList.add(inCollectorComponentId);
                if (outStatus == CollectorListStatus.ADDED) {
                    this.myLogger.fine("Collector added.");
                } else {
                    this.myLogger
                            .fine("Collector already contained in collector list, not added again.");
                }
            } else {
                this.myLogger
                        .fine("The blobber has reached its capacity, collector not added.");
            }
        } catch (InterruptedException e) {
            this.myLogger
                    .warning("addCollector was interrupted while sleeping.");
        }
        return outStatus;
    }

    /**
     * Removes the stated collector from the blobber. If the worker is currently
     * working with the specified collector it will finish the operation. This
     * method will however NOT wait for the operation, it returns immediately.
     *
     * @param inCollectorComponentId a String stating the name of the collector to remove.
     * @return a CollectorListStatus stating the result. Values to expect are
     *         {@link CollectorListStatus#UNKNOWN} or
     *         {@link CollectorListStatus#REMOVED}.
     */
    public CollectorListStatus removeCollector(String inCollectorComponentId) {
        this.myLogger.fine("Trying to remove collector "
                + inCollectorComponentId);
        CollectorListStatus outStatus = this.myCollectorList
                .remove(inCollectorComponentId);
        if (outStatus == CollectorListStatus.REMOVED) {
            this.myLogger.fine("Collector removed.");
        } else {
            this.myLogger
                    .fine("Collector not found in collector list, hence not removed.");
        }
        return outStatus;
    }

    /**
     * Checks if the collector list contains the stated collector.
     *
     * @param inCollectorComponentId a String stating the name of the collector to look for.
     * @return a CollectorListStatus stating the result. Values to expect are
     *         {@link CollectorListStatus#KNOWN} or
     *         {@link CollectorListStatus#UNKNOWN}.
     */
    public CollectorListStatus containsCollector(String inCollectorComponentId) {
        return this.myCollectorList.contains(inCollectorComponentId);
    }

    private List<AnyDataContainer> extractData(Any inSequence) {
        this.myLogger.fine("Extracting data from sequence.");
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        // doubleBlobDataSeq
        // Data coming from simple property
        // For this case, the index, i.e., the position inside the sequence is 0
        if (inSequence.type().equal(doubleBlobDataSeqHelper.type())) {
            doubleBlobData[] blobDataArray = doubleBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            for (doubleBlobData blobData : blobDataArray) {
                populateContainerNumeric(container, blobData.time,
                        blobData.value, 0);
            }
            outList.add(container);
            // doubleSeqBlobDataSeq
            // Data coming from composite property
            // For this case, the index, i.e., the position inside the sequence
            // varies

        } else if (inSequence.type().equal(doubleSeqBlobDataSeqHelper.type())) {
            doubleSeqBlobData[] blobDataMatrix = doubleSeqBlobDataSeqHelper
                    .extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (doubleSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
                        populateContainerNumeric(container, blobDataArray.time,
                                blobData, index);
                        index++;
                    }// for
                }// for
            }// if
            // floatBlobDataSeq
            // Data coming from simple property
            // For this case, the index, i.e., the position inside the sequence
            // is 0
        } else if (inSequence.type().equal(floatBlobDataSeqHelper.type())) {
            floatBlobData[] blobDataArray = floatBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (floatBlobData blobData : blobDataArray) {
                populateContainerNumeric(container, blobData.time,
                        blobData.value, index);
            }
            outList.add(container);
            // floatSeqBlobDataSeq
            // Data coming from composite property
            // For this case, the index, i.e., the position inside the sequence
            // varies
        } else if (inSequence.type().equal(floatSeqBlobDataSeqHelper.type())) {
            floatSeqBlobData[] blobDataMatrix = floatSeqBlobDataSeqHelper
                    .extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (floatSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
                        populateContainerNumeric(container, blobDataArray.time,
                                blobData, index);
                        index++;
                    } // end for
                }// end for

            } // end if
            // longBlobDataSeq
        } else if (inSequence.type().equal(longBlobDataSeqHelper.type())) {
            longBlobData[] blobDataArray = longBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (longBlobData blobData : blobDataArray) {
                populateContainerNumeric(container, blobData.time,
                        blobData.value, index);
            }
            outList.add(container);
            // longSeqBlobDataSeq
        } else if (inSequence.type().equal(longSeqBlobDataSeqHelper.type())) {
            longSeqBlobData[] blobDataMatrix = longSeqBlobDataSeqHelper
                    .extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (longSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
                        populateContainerNumeric(container, blobDataArray.time,
                                blobData, index);
                        index++;
                    }// end for
                }// end for

            }// end internal if
            // longLongBlobDataSeq
        } else if (inSequence.type().equal(longLongBlobDataSeqHelper.type())) {
            longLongBlobData[] blobDataArray = longLongBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (longLongBlobData blobData : blobDataArray) {
                populateContainerNumeric(container, blobData.time,
                        blobData.value, index);
            }
            outList.add(container);
            // longLongSeqBlobDataSeq
        } else if (inSequence.type().equal(longLongSeqBlobDataSeqHelper.type())) {
            longLongSeqBlobData[] blobDataMatrix = longLongSeqBlobDataSeqHelper
                    .extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (longLongSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;

                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
                        populateContainerNumeric(container, blobDataArray.time,
                                blobData, index);
                        index++;
                    }// end for
                }// end for
            }// end internal if
            // uLongLongBlobDataSeq
        } else if (inSequence.type().equal(uLongLongBlobDataSeqHelper.type())) {
            uLongLongBlobData[] blobDataArray = uLongLongBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (uLongLongBlobData blobData : blobDataArray) {
                populateContainerNumeric(container, blobData.time,
                        blobData.value, index);
            }
            outList.add(container);
            // uLongLongSeqBlobDataSeq
        } else if (inSequence.type()
                .equal(uLongLongSeqBlobDataSeqHelper.type())) {
            uLongLongSeqBlobData[] blobDataMatrix = uLongLongSeqBlobDataSeqHelper
                    .extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (uLongLongSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
                        populateContainerNumeric(container, blobDataArray.time,
                                blobData, index);
                        index++;
                    }// end for
                }// end for
            }// end if
            // patternBlobDataSeq
        } else if (inSequence.type().equal(patternBlobDataSeqHelper.type())) {
            patternBlobData[] blobDataArray = patternBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (patternBlobData blobData : blobDataArray) {
                populateContainerObject(container, blobData.time,
                        blobData.value, index);
            }
            outList.add(container);
            // stringBlobDataSeq
        } else if (inSequence.type().equal(stringBlobDataSeqHelper.type())) {
            stringBlobData[] blobDataArray = stringBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (stringBlobData blobData : blobDataArray) {
                populateContainerObject(container, blobData.time,
                        blobData.value, index);
            }
            outList.add(container);
            // stringSeqBlobDataSeq
        } else if (inSequence.type().equal(stringSeqBlobDataSeqHelper.type())) {
            stringSeqBlobData[] blobDataMatrix = stringSeqBlobDataSeqHelper
                    .extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (stringSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Object blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
                        populateContainerObject(container, blobDataArray.time,
                                blobData, index);
                        index++;
                    }
                }
            }
            // enumBlobDataSeq
        } else if (inSequence.type().equal(enumBlobDataSeqHelper.type())) {
            enumBlobData[] blobDataArray = enumBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (enumBlobData blobData : blobDataArray) {
                populateContainerObject(container, blobData.time,
                        blobData.value, index);
            }
            outList.add(container);
            // anyBlobDataSeq
        } else if (inSequence.type().equal(anyBlobDataSeqHelper.type())) {
            anyBlobData[] blobDataArray = anyBlobDataSeqHelper
                    .extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (anyBlobData blobData : blobDataArray) {
                populateContainerObject(container, blobData.time,
                        blobData.value, index);
            }
            outList.add(container);
        } else {
            this.myLogger
                    .warning("Unknown CORBA data type received by blobber");
            throw new IllegalStateException(
                    "Unknown CORBA data type received, " + inSequence.type());
        }
        // As the final step we remove the last vertical bar and add end of line
        // to the clob.
        for (java.util.Iterator<AnyDataContainer> i = outList.iterator(); i
                .hasNext();) {
            AnyDataContainer container = i.next();
            if (container.clobBuilder.length() == 0) {
                /*
                 * If the container is of zero length we remove it from the
                 * returned list.
                 */
                i.remove();
            } else {
                container.clobBuilder
                        .setLength(container.clobBuilder.length() - 1);
                container.clobBuilder.append("\n");
            }
        }
        return outList;
    }

    private void populateContainerNumeric(AnyDataContainer inContainer,
                                          long inTime, Number inData, int inIndex) {
        // it would be possible to use populateContainerObject after creating
        // the BigDecimal but we want to keep
        // the format of the original data.
        String numberString = inData.toString();
        try {
            inContainer.dataList.add(new BigDecimal(numberString));
        } catch (NumberFormatException e) {
            this.myLogger.warning("Unexpected number format. Time: " + inTime
                    + " Data: " + inData);
            throw e;
        }

        inContainer.clobBuilder.append(inTime);
        inContainer.clobBuilder.append("|");
        inContainer.clobBuilder.append(numberString);
        inContainer.clobBuilder.append("|");
        inContainer.index = inIndex;

    }

    private void populateContainerObject(AnyDataContainer inContainer,
                                         long inTime, Object inData, int inIndex) {
        inContainer.dataList.add(inData);
        inContainer.clobBuilder.append(inTime);
        inContainer.clobBuilder.append("|");
        inContainer.clobBuilder.append(inData);
        inContainer.clobBuilder.append("|");
        inContainer.index = inIndex;

    }

    /**
     * Populate inList with the inCount number of containers.
     */
    private void populateList(List<AnyDataContainer> inList, int inCount) {
        for (int index = 0; index < inCount; index++) {
            inList.add(new AnyDataContainer());
        }
    }

    protected MonitorCollector getMonitorCollector(String inCollectorName)
            throws Exception {
        this.myLogger.fine("Trying to fetch container " + inCollectorName);
        return MonitorCollectorHelper.narrow(this.myContainerServices
                .getComponent(inCollectorName));
    }

    @Override
    public void run() {
        this.myLogger.info("Starting worker thread.");
        Thread.currentThread().setName("BlobberWorkerThread");
        CollectorData collectorData = null;
        int listSize = 0;
        boolean sleep = false;
        while (!this.myTerminate) {
            try {
                synchronized (this.myCollectorList) {
                    /*
                     * Even though the methods in the collector list are
                     * synchronized we still synchronize externally to ensure we
                     * get the list size that is relevant for the call to
                     * next().
                     */
                    collectorData = this.myCollectorList.next();
                    listSize = this.myCollectorList.size();
                    if (this.myCollectorList.getLastIndex() == 0) {
                        this.myStartTime = System.currentTimeMillis();
                        sleep = true;
                    }
                }
                MonitorCollector collector = getMonitorCollector(collectorData
                        .getCollectorId());
                this.myLogger.fine("Accessing getMonitorData()");
                MonitorDataBlock[] dataBlocks = collector.getMonitorData();
                collectorData.lastSuccessfulAccessTime = System
                        .currentTimeMillis();
                if (dataBlocks != null) {
                    this.myLogger.fine("Received " + dataBlocks.length
                            + " MonitorDataBlocks from collector "
                            + collector.name());

                    for (MonitorDataBlock block : dataBlocks) {
                        this.myMonitorDAO.openTransactionStore();
                        this.myLogger.fine("MonitorDataBlock contains "
                                + block.monitorBlobs.length + " MonitorBlobs");
                        for (MonitorBlob blob : block.monitorBlobs) {
                            BlobData blobData = null;

                            try {
                                List<AnyDataContainer> containerList = extractData(blob.blobDataSeq);
                                int index = 0;
                                for (AnyDataContainer container : containerList) {
                                    String key = blob.propertyName;
                                    String serialNumber = block.deviceSerialNumber;
                                    if (blob.propertySerialNumber != null
                                            && blob.propertySerialNumber.length != 0) {
                                        // data is coming from correlator,
                                        // handle serial numbers.
                                        serialNumber = blob.propertySerialNumber[index];
                                        // create unique key by adding the index
                                        // number.
                                        key += index;
                                        index++;
                                    }
                                    this.myLogger.fine("Handling data for "
                                            + key);
                                    blobData = collectorData.equipmentData
                                            .get(key);
                                    if (blobData == null) {
                                        blobData = new BlobData();
                                        blobData.componentName = block.componentName;
                                        blobData.serialNumber = serialNumber;
                                        blobData.propertyName = blob.propertyName
                                                .substring(blob.propertyName
                                                        .indexOf(':') + 1);
                                        blobData.startTime = block.startTime;
                                        collectorData.equipmentData.put(key,
                                                blobData);
                                    }

                                    // Update blob data.
                                    blobData.dataList
                                            .addAll(container.dataList);
                                    blobData.clob += container.clobBuilder;
                                    blobData.index = container.index;
                                    blobData.stopTime = block.stopTime;
                                    if (blobData.startTime == 0) {
                                        /*
                                         * This construction is left from the
                                         * time when several accesses to the
                                         * collector was made before storing.
                                         * The construction is left in case this
                                         * is activated again.
                                         */
                                        blobData.startTime = block.startTime;
                                    }

                                    if (blobData.dataList.size() > 0) {
                                        if (blobData.dataList.get(0) instanceof BigDecimal) {
                                            blobData.statistics = calculateStatistics(blobData.dataList);
                                        }
                                        storeData(blobData);
                                        blobData.reset();
                                    }
                                }
                            } catch (Exception e) {


                                if (!this.loggedFailedStoreMap.containsKey(blob.propertyName)) {

                                    this.myLogger.log(Level.WARNING,
                                            "Problem when handling property ["
                                                    + blob.propertyName + "]", e);
                                    this.myLogger
                                            .log(
                                                    Level.WARNING,
                                                    "Due to this exception the data cache for this property in collector ["
                                                            + collectorData
                                                            .getCollectorId()
                                                            + "] will be cleared, the data is NOT stored");
                                    this.myLogger.log(Level.WARNING,
                                            "The data contained in the data cache: "
                                                    + blobData);

                                 if (blobData != null) {
                                        blobData.reset();
                                    }
                                    this.loggedFailedStoreMap.put(blob.propertyName, null);
                                }
                            }
                        }
                        try {
							this.myMonitorDAO.closeTransactionStore();
						} catch (BatchUpdateException ex) {
							this.myLogger
									.log(Level.WARNING,
											"Exception caught. Some monitoring data couldn't be archived",
											ex);
						}
                    }
                }
                if (sleep && !this.myTerminate) {
                    sleep = false;
                    long totalTime = System.currentTimeMillis()
                            - this.myStartTime;
                    if (totalTime < getCollectInterval()) {
                        this.myLogger.fine("Total time for " + listSize
                                + " collector(s): " + totalTime + " msecs");
                        this.myLogger
                                .fine("Will now sleep for "
                                        + (getCollectInterval() - totalTime)
                                        + " msecs");
                        Thread.sleep(getCollectInterval() - totalTime);
                    } else {
                        this.myLogger
                                .warning("Total time of "
                                        + totalTime
                                        + " msecs for handling "
                                        + listSize
                                        + " registered collectors exceeded the set time of "
                                        + getCollectInterval() + " msec.");
                    }
                }
            } catch (InterruptedException e) {
                // If we end up here it means that we should
                // terminate.
            } catch (Exception e) {
                this.myLogger.log(Level.WARNING, "Exception caught", e);
                this.myLogger
                        .log(
                                Level.WARNING,
                                "Due to this exception the data cache for collector ["
                                        + collectorData.getCollectorId()
                                        + "] will be cleared, the data is LOST FOREVER");
                collectorData.equipmentData = new HashMap<String, BlobData>();
            }
        }
    }

    protected void storeData(BlobData inBlobData) throws Exception {
        this.myLogger.fine("Storing data for " + inBlobData.componentName);
        this.myLogger.fine(inBlobData.toString());
        inBlobData.sampleSize = inBlobData.dataList.size();
        this.myMonitorDAO.store(inBlobData);
    }

    private ComponentStatistics calculateStatistics(List<Object> inDataList) {
        ComponentStatistics outStatistics = new ComponentStatistics();
        outStatistics.min = (BigDecimal) inDataList.get(0);
        outStatistics.max = (BigDecimal) inDataList.get(0);
        outStatistics.mean = new BigDecimal(0);
        for (Object blobData : inDataList) {

            BigDecimal value = (BigDecimal) blobData;
            if (value.compareTo(outStatistics.min) == -1) {
                outStatistics.min = value;
            } else if (value.compareTo(outStatistics.max) == 1) {
                outStatistics.max = value;
            }
            outStatistics.mean = outStatistics.mean.add(value);
        }

        outStatistics.mean = outStatistics.mean.divide(new BigDecimal(
                inDataList.size()), MathContext.DECIMAL32);
        outStatistics.stdDev = new BigDecimal(-1);
        if (inDataList.size() > 1) {
            outStatistics.stdDev = new BigDecimal(0);
            for (Object blobData : inDataList) {
                BigDecimal value = (BigDecimal) blobData;
                outStatistics.stdDev = outStatistics.stdDev.add(value.subtract(
                        outStatistics.mean).pow(2));
            }
            outStatistics.stdDev = new BigDecimal(Math
                    .sqrt(outStatistics.stdDev.divide(
                    new BigDecimal(inDataList.size() - 1),
                    MathContext.DECIMAL32).doubleValue()));
        }

        return outStatistics;
    }

    protected long getCollectInterval() {
        synchronized (this.myCollectInterval) {
            return this.myCollectInterval;
        }
    }

    protected void setCollectInterval(long inSeconds) {
        if (inSeconds < 0) {
            throw new IllegalArgumentException(inSeconds + " must be above 0.");
        }
        synchronized (this.myCollectInterval) {
            this.myCollectInterval = inSeconds * 1000;
        }
    }

    public void terminate() {
        this.myTerminate = true;
    }

    protected class AnyDataContainer {
        public List<Object> dataList = new ArrayList<Object>();
        public StringBuilder clobBuilder = new StringBuilder();
        public int index;
    }

}
