/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2011 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */
package alma.acs.monitoring.blobber;

import java.math.BigDecimal;
import java.math.MathContext;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.Any;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.MonitorArchiver.CollectorListStatus;
import alma.TMCDB.MonitorBlob;
import alma.TMCDB.MonitorCollector;
import alma.TMCDB.MonitorCollectorHelper;
import alma.TMCDB.MonitorCollectorOperations;
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
import alma.acs.concurrent.ThreadLoopRunner.CancelableRunnable;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.monitoring.DAO.ComponentStatistics;
import alma.acs.monitoring.DAO.MonitorDAO;
import alma.acs.monitoring.blobber.CollectorList.BlobData;
import alma.acs.monitoring.blobber.CollectorList.CollectorData;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.StopWatch;



/**
 * The part of the blobber component that does the real work.
 * It implements Runnable to allow running it repeatedly in a separate thread.
 */
public class BlobberWorker extends CancelableRunnable {

    public static final String BLOBBER_CHECK_JVM_MEMORY_PROPERTYNAME = "alma.acs.monitoring.blobber.checkmemory";
	
    /**
     * DAOs store the data in the DB or on file, inside proper transaction(s) and performing auto-completion of hardware tables if necessary.
     */
    protected List<MonitorDAO> myMonitorDAOList;

//    /**
//     * WatchDog to get blobber queues statistics. Currently not used in the ACS layers of blobber comp.
//     */
//    protected BlobberWatchDog myWatchDog;

    /**
     * List of monitor collectors that this blobber serves.
     */
    private final CollectorList myCollectorList = new CollectorList();

    protected ContainerServices myContainerServices;
    protected Logger myLogger;

    /**
     * Maximum number of monitor collectors that a blobber accepts to handle.
     */
    private volatile int myMaxCollectorCount = 6;

    /**
     * The interval between blobber cycles, each harvesting all collectors.
     * This is used only for logging, since the cycle gets triggered from outside of this class.
     */
    protected long collectIntervalSec;

    /**
     * Remembers names of properties for which a failure has been logged already, 
     * to avoid repeating this log. 
     * <p>
     * We store the property name with full path, e.g. <code>CONTROL/AOSTiming/PSCR:STATUS</code>.
     */
    private final HashSet<String> loggedFailedStore = new HashSet<String>();

    /**
     * See "<code>archive.tmcdb.monitoring.profiling</code>" in archiveConfig.properties, 
     * and {@link #debugDataSender}.
     */
    private boolean isProfilingEnabled;

    /**
     * Used for special UDP debug messages, see {@link #isProfilingEnabled}.
     */
    private DebugDataSender debugDataSender;

    /**
     * Counts how often the blobber has run in total.
     */
    private volatile long cycleCount = 0L;

    /**
     * Was added on ACS-9_0_0-B but then not merged to the HEAD in the old location ARCHIVE/TMCDB/.
     * @TODO replace with generic component ref cache
     */
    private final HashMap<String, MonitorCollectorOperations> collectorName2ComponentReference = 
    												new HashMap<String, MonitorCollectorOperations>();

    /**
     * @param inContainerServices used for logging and to get references to the collector components.
     * @param blobberPlugin 
     * @throws AcsJCouldntCreateObjectEx If <code>blobberPlugin#createMonitorDAO</code> fails.
     */
    public BlobberWorker(ContainerServices inContainerServices, BlobberPlugin blobberPlugin) throws AcsJCouldntCreateObjectEx {
        this.myContainerServices = inContainerServices;
        myLogger = myContainerServices.getLogger();
        this.isProfilingEnabled = blobberPlugin.isProfilingEnabled();
        notifyCollectorIntervalChange(blobberPlugin.getCollectorIntervalSec());
        initWorker();
        this.myMonitorDAOList = blobberPlugin.getMonitorDAOs();
//        this.myWatchDog = blobberPlugin.getBlobberWatchDog();
    }

    protected void initWorker() {
        if (isProfilingEnabled) {
        	try {
        		debugDataSender = new DebugDataSender(myContainerServices.getName());
                myLogger.info("Blobber profiling enabled: " + debugDataSender.toString());
            } catch (java.net.UnknownHostException e) {
                isProfilingEnabled = false;
                myLogger.warning("Failed to enable blobber profiling: " + DebugDataSender.targetHost + " could not be resolved.");
            } catch (Exception e) {
        		isProfilingEnabled = false;
                myLogger.log(Level.WARNING, "Failed to enable blobber profiling.", e);
        	}
        } 
        else {
            myLogger.info("Blobber profiling NOT enabled.");
        }
    }

    /**
	 * @see #collectIntervalSec
	 * @see BlobberImpl#setCollectorIntervalSeconds(long)
	 */
	void notifyCollectorIntervalChange(long inCollectIntervalSec) {
		this.collectIntervalSec = inCollectIntervalSec;
	}
   

    /**
     * @return The maximum number of collectors that this blobber is allowed to have.
     */
    protected int getMaxCollectorCount() {
        return myMaxCollectorCount;
    }

    protected void setMaxCollectorCount(int inCount) {
        if (inCount <= 0) {
            throw new IllegalArgumentException(inCount + " must be above 0.");
        }
        myMaxCollectorCount = inCount;
    }

    /**
     * Adds the collector component of the given name to the blobber. If the collector is already
     * known to the blobber it will not be added again. 
     * <p>
     * HSO TODO: what is meant by the following description? I don't see any of this in the code. <br>
     * This method will however
     * NOT wait for this, it returns immediately with one exception. When the
     * second collector is added it is first ensured that one access has
     * completed to the firstly added collector in order to wait for the access
     * statistics needed to calculate the capacity of the blobber.
     * @param inCollectorComponentId a String stating the name of the collector to add.
     * @return a CollectorListStatus stating the result. Values to expect are
     *         {@link CollectorListStatus#ADDED},
     *         {@link CollectorListStatus#KNOWN} or
     *         {@link CollectorListStatus#FULL}.
     */
    public CollectorListStatus addCollector(String inCollectorComponentId) {
        myLogger.fine("Trying to add collector " + inCollectorComponentId);
        CollectorListStatus outStatus = CollectorListStatus.FULL;
        if (canHandle()) {
            outStatus = this.myCollectorList.add(inCollectorComponentId);
            if (outStatus == CollectorListStatus.ADDED) {
                myLogger.fine("Collector "  + inCollectorComponentId + " added.");
            } else {
                myLogger.fine("Collector "  + inCollectorComponentId + " already contained in collector list, not added again.");
            }
        } 
        else {
            myLogger.fine("The blobber has reached its capacity, collector "  + inCollectorComponentId + " not added.");
        }
        return outStatus;
    }

    protected boolean canHandle() {
    	return this.myCollectorList.size() < getMaxCollectorCount();
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
        myLogger.fine("Trying to remove collector " + inCollectorComponentId);
        CollectorListStatus outStatus = this.myCollectorList.remove(inCollectorComponentId);
        if (outStatus == CollectorListStatus.REMOVED) {
            myLogger.fine("Collector removed.");
        } else {
            myLogger.fine("Collector not found in collector list, hence not removed.");
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

    /**
     * HSO TODO: IDL struct MonitorBlob should be defined without using Corba Any (performance!).
     * Rather use separate fields for the possible data types.
     * 
     * @param inSequence
     * @return
     */
    private List<AnyDataContainer> extractData(Any inSequence) {
        myLogger.fine("Extracting data from sequence.");
        List<AnyDataContainer> outList = new ArrayList<AnyDataContainer>();
        // doubleBlobDataSeq
        // Data coming from simple property
        // For this case, the index, i.e., the position inside the sequence is 0
        if (inSequence.type().equal(doubleBlobDataSeqHelper.type())) {
            doubleBlobData[] blobDataArray = doubleBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            for (doubleBlobData blobData : blobDataArray) {
                populateContainerNumeric(container, blobData.time, blobData.value, 0);
            }
            outList.add(container);
            // doubleSeqBlobDataSeq
            // Data coming from composite property
            // For this case, the index, i.e., the position inside the sequence
            // varies

        } else if (inSequence.type().equal(doubleSeqBlobDataSeqHelper.type())) {
			doubleSeqBlobData[] blobDataMatrix = doubleSeqBlobDataSeqHelper.extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (doubleSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
						populateContainerNumeric(container, blobDataArray.time, blobData, index);
                        index++;
                    }// for
                }// for
            }// if
            // floatBlobDataSeq
            // Data coming from simple property
            // For this case, the index, i.e., the position inside the
            // sequence
            // is 0
        } else if (inSequence.type().equal(floatBlobDataSeqHelper.type())) {
			floatBlobData[] blobDataArray = floatBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (floatBlobData blobData : blobDataArray) {
				populateContainerNumeric(container, blobData.time, blobData.value, index);
            }
            outList.add(container);
            // floatSeqBlobDataSeq
            // Data coming from composite property
            // For this case, the index, i.e., the position inside the sequence
            // varies
        } else if (inSequence.type().equal(floatSeqBlobDataSeqHelper.type())) {
			floatSeqBlobData[] blobDataMatrix = floatSeqBlobDataSeqHelper.extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (floatSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
						populateContainerNumeric(container, blobDataArray.time, blobData, index);
                        index++;
                    } // end for
                }// end for

            } // end if
            // longBlobDataSeq
        } else if (inSequence.type().equal(longBlobDataSeqHelper.type())) {
			longBlobData[] blobDataArray = longBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (longBlobData blobData : blobDataArray) {
				populateContainerNumeric(container, blobData.time, blobData.value, index);
            }
            outList.add(container);
            // longSeqBlobDataSeq
        } else if (inSequence.type().equal(longSeqBlobDataSeqHelper.type())) {
			longSeqBlobData[] blobDataMatrix = longSeqBlobDataSeqHelper.extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (longSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
						populateContainerNumeric(container, blobDataArray.time, blobData, index);
                        index++;
                    }// end for
                }// end for

            }// end internal if
            // longLongBlobDataSeq
        } else if (inSequence.type().equal(longLongBlobDataSeqHelper.type())) {
			longLongBlobData[] blobDataArray = longLongBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (longLongBlobData blobData : blobDataArray) {
				populateContainerNumeric(container, blobData.time, blobData.value, index);
            }
            outList.add(container);
            // longLongSeqBlobDataSeq
        } else if (inSequence.type().equal(longLongSeqBlobDataSeqHelper.type())) {
			longLongSeqBlobData[] blobDataMatrix = longLongSeqBlobDataSeqHelper.extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (longLongSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;

                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
						populateContainerNumeric(container, blobDataArray.time, blobData, index);
                        index++;
                    }// end for
                }// end for
            }// end internal if
            // uLongLongBlobDataSeq
        } else if (inSequence.type().equal(uLongLongBlobDataSeqHelper.type())) {
			uLongLongBlobData[] blobDataArray = uLongLongBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (uLongLongBlobData blobData : blobDataArray) {
				populateContainerNumeric(container, blobData.time, blobData.value, index);
            }
            outList.add(container);
            // uLongLongSeqBlobDataSeq
        } else if (inSequence.type().equal(uLongLongSeqBlobDataSeqHelper.type())) {
			uLongLongSeqBlobData[] blobDataMatrix = uLongLongSeqBlobDataSeqHelper.extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (uLongLongSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Number blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
						populateContainerNumeric(container, blobDataArray.time, blobData, index);
                        index++;
                    }// end for
                }// end for
            }// end if
            // patternBlobDataSeq
        } else if (inSequence.type().equal(patternBlobDataSeqHelper.type())) {
			patternBlobData[] blobDataArray = patternBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (patternBlobData blobData : blobDataArray) {
				populateContainerObject(container, blobData.time, blobData.value, index);
            }
            outList.add(container);
            // stringBlobDataSeq
        } else if (inSequence.type().equal(stringBlobDataSeqHelper.type())) {
			stringBlobData[] blobDataArray = stringBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (stringBlobData blobData : blobDataArray) {
				populateContainerObject(container, blobData.time, blobData.value, index);
            }
            outList.add(container);
            // stringSeqBlobDataSeq
        } else if (inSequence.type().equal(stringSeqBlobDataSeqHelper.type())) {
			stringSeqBlobData[] blobDataMatrix = stringSeqBlobDataSeqHelper.extract(inSequence);
            if (blobDataMatrix != null && blobDataMatrix.length > 0) {
                populateList(outList, blobDataMatrix[0].value.length);
                for (stringSeqBlobData blobDataArray : blobDataMatrix) {
                    int index = 0;
                    for (Object blobData : blobDataArray.value) {
                        AnyDataContainer container = outList.get(index);
						populateContainerObject(container, blobDataArray.time, blobData, index);
                        index++;
                    }
                }
            }
            // enumBlobDataSeq
        } else if (inSequence.type().equal(enumBlobDataSeqHelper.type())) {
			enumBlobData[] blobDataArray = enumBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (enumBlobData blobData : blobDataArray) {
				populateContainerObject(container, blobData.time, blobData.value, index);
            }
            outList.add(container);
            // anyBlobDataSeq
        } else if (inSequence.type().equal(anyBlobDataSeqHelper.type())) {
			anyBlobData[] blobDataArray = anyBlobDataSeqHelper.extract(inSequence);
            AnyDataContainer container = new AnyDataContainer();
            int index = 0;
            for (anyBlobData blobData : blobDataArray) {
				populateContainerObject(container, blobData.time, blobData.value, index);
            }
            outList.add(container);
        } else {
			myLogger.warning("Unknown CORBA data type received by blobber");
            throw new IllegalStateException(
                    "Unknown CORBA data type received, " + inSequence.type());
        }
        // As the final step we remove the last vertical bar and add end of line
        // to the clob.
        for (java.util.Iterator<AnyDataContainer> i = outList.iterator(); i.hasNext();) {
            AnyDataContainer container = i.next();
            if (container.clobBuilder.length() == 0) {
                /*
                 * If the container is of zero length we remove it from the
                 * returned list.
                 */
                i.remove();
            } else {
                container.clobBuilder.setLength(container.clobBuilder.length() - 1);
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
        if (numberString.equalsIgnoreCase("nan"))
            return; //Skip NaN values (COMP-5564)

        try {
            inContainer.dataList.add(new BigDecimal(numberString));
        } catch (NumberFormatException e) {
            myLogger.warning("Unexpected number format. Time: " + inTime + " Data: " + inData);
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

    /**
     * Gets the reference of the {@link MonitorCollector} component 
     * that is identified by its name <code>inCollectorName</code>
     * which could for example be "CONTROL/CM01/MONITOR_COLLECTOR".
     * <p>
     * Collector references are cached in {@link #collectorName2ComponentReference}
     * across blobber cycles.
     */
    protected MonitorCollectorOperations getMonitorCollector(String inCollectorName) throws AcsJContainerServicesEx {
    	MonitorCollectorOperations ret = collectorName2ComponentReference.get(inCollectorName);
    	if (ret == null) {
            myLogger.fine("Trying to fetch collector for container " + inCollectorName);
    		ret = MonitorCollectorHelper.narrow(myContainerServices.getComponent(inCollectorName));
            collectorName2ComponentReference.put(inCollectorName, ret);
        }
    	return ret;
    }

	/**
	 * This method will be called at fixed intervals. It gathers the data from all registered collectors and stores it
	 * in the database, using the layer from module TMCBD/DAO.
	 * 
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {

		cycleCount++;
		myLogger.info("Running BlobberWorker cycle " + cycleCount);

		int collectorCount = 0;
		int insertCount = 0;

		StopWatch stopWatchAllCollectors = new StopWatch(myLogger);

		// Checking memory requires a GC run (and no other components running in the same container) 
		// to give reasonable results. Running GC from inside the program we don't want to do as default,
		// thus the use of the cheating property.
		long usedMemKBBeforeCycle = -1;
		if (Boolean.getBoolean(BLOBBER_CHECK_JVM_MEMORY_PROPERTYNAME)) {
			System.gc();
			Runtime rt = Runtime.getRuntime();
			usedMemKBBeforeCycle = (rt.totalMemory() - rt.freeMemory()) / 1024;
			myLogger.fine("Used JVM memory in kB after GC before blobber cycle: " + usedMemKBBeforeCycle);
		}
		

		// loop over all collectors. Note that collectors can be added and removed during this loop,
		// so that the initial list size does not necessarily show the number of executed collectors.
		CollectorData collectorData = null;
		myCollectorList.resetIterator();
		while (myCollectorList.hasNext()) {
			try {
				collectorData = myCollectorList.next();

				// has this thread been requested to terminate?
				if (shouldTerminate) {
					myLogger.info("Loop over collectors terminated prematurely, skipping '" + collectorData.getCollectorId() + "' and subsequent collectors.");
					break;
				}

				collectorCount++;
				StopWatch stopWatchCurrentCollector = new StopWatch(myLogger);

				// Get the corba ref for the current collector
				MonitorCollectorOperations collector = getMonitorCollector(collectorData.getCollectorId());

				insertCount += harvestCollector(collectorData, collector);

				stopWatchCurrentCollector.logLapTime("process monitoring data from collector " + collectorData.getCollectorId());

			} catch (Exception e) {
				myLogger.log(Level.WARNING, "Exception caught while processing monitor collector " + collectorData.getCollectorId() 
						+ "; the data cache for this collector will be cleared, the data is LOST FOREVER", e);
				// @TODO Shouldn't we raise an alarm also here, now that we do when blobber comp fails to initialize?
				// Then it would need to be cleared once (the same collector's??) data is processed ok in the next round.
			}
		}

		if (Boolean.getBoolean(BLOBBER_CHECK_JVM_MEMORY_PROPERTYNAME)) {
			System.gc();
			Runtime rt = Runtime.getRuntime();
			long usedMemKBAfterCycle = (rt.totalMemory() - rt.freeMemory()) / 1024;
			myLogger.fine("Used JVM memory in kB after GC after blobber cycle: " + usedMemKBAfterCycle);
		}

		long totalTimeMillis = stopWatchAllCollectors.getLapTimeMillis();

		if (totalTimeMillis < collectIntervalSec * 1000) {
			// the good case: all collectors were processed within the foreseen time window
			String msg = "Processed monitoring data from " + collectorCount + " collector(s) in " + totalTimeMillis + " ms (within the time limit).";
			myLogger.info(msg);
			if (isProfilingEnabled) {
				debugDataSender.sendUDPPacket(msg, cycleCount);
				debugDataSender.sendUDPPacket("Total inserts for cycle " + cycleCount + " were " + insertCount, cycleCount);
			}
		} 
		else {
			// the bad case: this run took too long.
			String msg = "Processed monitoring data from " + collectorCount + " collector(s) in " + totalTimeMillis
					+ " ms (exceeding the time limit of " + collectIntervalSec + " s).";
			myLogger.warning(msg);
			if (isProfilingEnabled) {
				debugDataSender.sendUDPPacket(msg, cycleCount);
				debugDataSender.sendUDPPacket("Total inserts for cycle " + cycleCount + " were " + insertCount, cycleCount);
			}
		}
	}


    /**
     * Retrieves and processes the data from one monitor collector component, that is, from one container.
     * <p>
     * Storage details:
     * <ul>
     *   <li> Uses {@link MonitorDAO} to control DB transactions and store the data.
     *   <li> Uses a total of one DB transaction (for the data from all properties of all devices, which comes in many {@link MonitorDataBlock}s).
     * </ul>
     * @param collectorData
     * @param collector
     * @return Number of attempted or successful DB inserts, 
     *         i.e., calls to {@link alma.archive.tmcdb.DAO.MonitorDAO#store(alma.archive.tmcdb.DAO.ComponentData)}.
     * @throws Exception
     */
	private int harvestCollector(CollectorData collectorData, MonitorCollectorOperations collector)
			throws Exception {

		int insertCount = 0;

		myLogger.fine("About to call " + collectorData.getCollectorId() + "#getMonitorData()");
		MonitorDataBlock[] dataBlocks = collector.getMonitorData();
		collectorData.setLastSuccessfulAccessTime(System.currentTimeMillis());
		
		if (dataBlocks != null) {
			
			// HSO TODO: do not call name() on the remote component, but use the local getCollectorId which happens to be the name.
		    myLogger.info("Received " + dataBlocks.length
		            + " MonitorDataBlocks from collector "
		            + collector.name());

			if (isProfilingEnabled) {
				debugDataSender.sendUDPPacket("Received " + dataBlocks.length + " MonitorDataBlocks from collector " + collector.name(), cycleCount);
				Runtime runtime = Runtime.getRuntime();
				debugDataSender.sendUDPPacket("Used memory: " + Long.toString((runtime.totalMemory() - runtime.freeMemory())), cycleCount);
				debugDataSender.sendUDPPacket("Free memory: " + Long.toString(runtime.freeMemory()), cycleCount);
				debugDataSender.sendUDPPacket("Total memory: " + Long.toString(runtime.totalMemory()), cycleCount);
			}
			
			for (MonitorDAO monitorDAO : myMonitorDAOList) {
				monitorDAO.openTransactionStore(Long.toString(cycleCount) + "-" + collectorData.getCollectorId()); // @TODO: Should we catch / log the possible Exception, or just let it fly?
			}

			// iterate over devices
			for (MonitorDataBlock block : dataBlocks) {
				myLogger.log(AcsLogLevel.DEBUG, "MonitorDataBlock for device " + block.componentName + " contains " + block.monitorBlobs.length + " MonitorBlobs");

				// iterate over properties
				for (MonitorBlob blob : block.monitorBlobs) {
					BlobData blobData = null;

					try {
						List<AnyDataContainer> containerList = extractData(blob.blobDataSeq);
						int index = 0;
						for (AnyDataContainer container : containerList) {
							String serialNumber = block.deviceSerialNumber;
							if (blob.propertySerialNumber != null
									&& blob.propertySerialNumber.length != 0) {
								// data is coming from correlator, handle serial numbers.
								// @TODO (hso): Can we delegate this special treatment to the DAO?
								serialNumber = blob.propertySerialNumber[index];
								// create unique key by adding the index number.
								index++;
							}
							myLogger.log(AcsLogLevel.DEBUG, "Handling data for property " + blob.propertyName);
							blobData = new BlobData();
							blobData.componentName = block.componentName;
							blobData.serialNumber = serialNumber;
							blobData.propertyName = blob.propertyName.substring(blob.propertyName.indexOf(':') + 1);
							blobData.startTime = block.startTime;

							// Update blob data.
							blobData.dataList.clear();
							blobData.dataList.addAll(container.dataList);
							blobData.clob = container.clobBuilder.toString();
							blobData.index = container.index;
							blobData.stopTime = block.stopTime;
							if (blobData.startTime == 0) {
								/*
								 * This construction is left from the time when several accesses to the collector was
								 * made before storing. The construction is left in case this is activated again.
								 */
								blobData.startTime = block.startTime;
							}

							if (blobData.dataList.size() > 0) {
								if (blobData.dataList.get(0) instanceof BigDecimal) {
									blobData.statistics = calculateStatistics(blobData.dataList);
								}

								insertCount++;
								storeData(blobData);
							}
						}
					} catch (Exception e) {

						if (!this.loggedFailedStore.contains(blob.propertyName)) {

							this.loggedFailedStore.add(blob.propertyName);

							// @TODO check if http://jira.alma.cl/browse/COMP-4512 can be closed
							String msg = "Problem when handling property [" + blob.propertyName + "]. "
									+ "The data cache for this property in collector ["
									+ collectorData.getCollectorId() + "] will be cleared, the data is NOT stored. ";
							myLogger.log(Level.WARNING, msg, e);

							// unclear if we want this log. If so, it should be included in the above log,
							// to avoid spreading the info over many log records that won't be adjacent in jlog.
							// see alma.archive.tmcdb.DAO.ComponentData.toString() where currently the clob data is excluded.
							myLogger.log(Level.WARNING, "The data contained in the data cache: " + blobData);
						} 
						else {

							// RK: Matthias suggested we log this too
							// HSO commented it out again, because now instead we log this AcsJStoreFailureEx
							// directly in alma.archive.tmcdb.DAO.MonitorDAOImpl.store(ComponentData) at level FINER
							// myLogger.log(Level.WARNING,
							// "Repeat of problem when handling property ["
							// + blob.propertyName + "]", e);
						}

						// RK: Matthias fixed a mem leak by moving this here:
						if (blobData != null) {
							blobData.reset();
						}
					}
				} // end loop over properties of a single device
			} // end loop over devices of a single container

			// close the transaction
			for (MonitorDAO monitorDAO : myMonitorDAOList) {
				try {
					// @TODO (HSO): We do not log the matching openTransactionStore at INFO level. Shouldn't this be symmetric?
					myLogger.info("myMonitorDAO.closeTransactionStore() for: " + dataBlocks.length
							+ " MonitorDataBlocks from collector " + collector.name());
					monitorDAO.closeTransactionStore();
				} catch (Exception ex) {
					myLogger.log(Level.WARNING, "Exception caught. Some monitoring data couldn't be archived", ex);
				}
			}
		}
		return insertCount;
	}

	
    /**
     * Stores the data using the MonitorDAO layer.
     */
    protected void storeData(BlobData inBlobData) throws Exception {
    	if (myLogger.isLoggable(Level.FINEST)) {
    		// we only log at level finest because the same log will also come from inside the DAO#store implementation
    		// @HSO TODO: Include the "clob" field again in toString, but use a different method here 
    		// which will not log clob data.
    		myLogger.finest("Storing property data for " + inBlobData.componentName + ": " + inBlobData.toString());
    	}
    	inBlobData.sampleSize = inBlobData.dataList.size();

    	Exception firstExInDAOStore = null;
		for (MonitorDAO monitorDAO : myMonitorDAOList) {
			try {
				monitorDAO.store(inBlobData);
			} catch (Exception ex) {
				if (firstExInDAOStore == null) {
					// throw this later, but first store in the other DAOs
					firstExInDAOStore = ex;
				}
			}
		}
		if (firstExInDAOStore != null) {
			throw firstExInDAOStore;
		}
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
            outStatistics.stdDev = new BigDecimal(
                    Math.sqrt(outStatistics.stdDev.divide(
                            new BigDecimal(inDataList.size() - 1),
                            MathContext.DECIMAL32).doubleValue()));
        }

        return outStatistics;
    }

    
    
    protected static class AnyDataContainer {
        public List<Object> dataList = new ArrayList<Object>();
        public StringBuilder clobBuilder = new StringBuilder();
        public int index;
    }


	/**
	 * An interesting, non-standard way of supplying a listening GUI application with debug logs, 
	 * without having to tap the log stream.
	 */
	private static class DebugDataSender
	{
		public static final String targetHost = "tmc-services.aiv.alma.cl";

		private InetAddress address;

		private String location;

		private String blobberName;

		public DebugDataSender(String blobberName) throws UnknownHostException {
            this.blobberName = blobberName;
            location = System.getenv("LOCATION");
            if (location == null) {
                location = "GENERIC";
            }
            address = InetAddress.getByName(targetHost);
		}
	
		protected void sendUDPPacket(String msg, long cycleCount) {
			try {
				int port = 9090;

				String date = IsoDateFormat.formatCurrentDate();
				String compMsg = location + " | " + blobberName + " | " + cycleCount + " | " + date + "|" + msg;

				byte[] message = compMsg.getBytes();

				// Initialize a datagram packet with data and address
				DatagramPacket packet = new DatagramPacket(message, message.length, address, port);

				// Create a datagram socket, send the packet through it, close it.
				// @TODO (HSO): can/should we reuse this DatagramSocket across calls? 
				DatagramSocket dsocket = new DatagramSocket();
				dsocket.send(packet);
				dsocket.close();

			} catch (Exception e) {
				// do not use the logger here, since the whole purpose of using DebugDataSender is to produce debug data 
				// without jamming the log channels.
				System.err.println("Exception caught while sending UDP packet:" + e.getMessage());
			}
		}
		
		public String toString() {
			return "location=" + location + " address=" + address + " blobberName=" + blobberName;
		}
	}

}
