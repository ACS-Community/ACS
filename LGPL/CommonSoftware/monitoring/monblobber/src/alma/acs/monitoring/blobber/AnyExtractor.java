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

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.Any;
import org.omg.CORBA.TypeCodePackage.BadKind;

import alma.ACSErrTypeCommon.wrappers.AcsJNoResourcesEx;
import alma.TMCDB.booleanBlobData;
import alma.TMCDB.booleanBlobDataSeqHelper;
import alma.TMCDB.booleanSeqBlobData;
import alma.TMCDB.booleanSeqBlobDataSeqHelper;
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
import alma.TMCDB.uLongBlobData;
import alma.TMCDB.uLongBlobDataSeqHelper;
import alma.TMCDB.uLongLongBlobData;
import alma.TMCDB.uLongLongBlobDataSeqHelper;
import alma.TMCDB.uLongLongSeqBlobData;
import alma.TMCDB.uLongLongSeqBlobDataSeqHelper;
import alma.TMCDB.uLongSeqBlobData;
import alma.TMCDB.uLongSeqBlobDataSeqHelper;
import alma.acs.monitoring.MonitorPointTimeSeries;
import alma.acs.monitoring.MonitorPointValue;

/**
 * Processes the monitor point data that arrives to the blobber in the form of Corba Anys.
 * <p>
 * TODOs:
 * <ul>
 *   <li>Refactor this class to separate the two steps of 
 *       (a) unpacking corba any data, 
 *       (b) demultiplexing sequence data using MonitorPointExpert.
 *       Note that with ACS 12.2 we already removed the generation of CLOB data which was also mixed in to this code.
 *   <li>Long-term: IDL struct MonitorBlob should perhaps be defined without using Corba Any (performance!).
 *       Rather use separate fields for the possible data types.
 * <ul>
 */
class AnyExtractor
{
	private final Logger logger;
	private final MonitorPointExpert monitorPointExpert;

	/**
	 * @param logger
	 * @param monitorPointExpert TODO remove this once the extractor only extracts corba anys w/o other processing
	 */
	AnyExtractor(Logger logger, MonitorPointExpert monitorPointExpert) {
		this.logger = logger;
		this.monitorPointExpert = monitorPointExpert;
	}
	
	
	/**
	 * The main method of this class.
	 * It checks <code>inSequence</code> for the recognized data types
	 * and extracts them using the respective Corba Helper classes.
	 * We keep NaN floating point types, so that they will have to be supressed later in the
	 * data processing if needed. 
	 * <p>
	 * Currently this method also rearranges the sequence data based on information 
	 * obtained from monitorPointExpert. This should be done separately in the future.
	 * 
	 * @param inSequence
	 *            The sequence of data from one property (monitor point) from one collector interval.
	 * @param propertyName Used along with {@link #monitorPointExpert} for MP-specific data restructuring.
	 * @return List of one or many MonitorPointTimeSeries objects extracted from inSequence.
	 *         The list position is in line with the "derived index" in case a monitor point was expanded into many. 
	 * @throws AcsJNoResourcesEx 
	 */
	List<MonitorPointTimeSeries> extractData(Any inSequence, String propertyName) throws AcsJNoResourcesEx {
		
		List<MonitorPointTimeSeries> outList = new ArrayList<MonitorPointTimeSeries>();

		try {
			// doubleBlobDataSeq
			// This is time series data coming from a simple property.
			// For this case, the index, i.e., the position inside the sequence is 0
			if (inSequence.type().equal(doubleBlobDataSeqHelper.type())) {
				doubleBlobData[] blobDataArray = doubleBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, doubleBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (doubleBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					mpv.addValue(blobData.value); // Double
					mpTs.addMonitorPointValue(mpv);
				}
			}
	
			// doubleSeqBlobDataSeq
			// This is time series data coming from a multi-valued property.
			else if (inSequence.type().equal(doubleSeqBlobDataSeqHelper.type())) {
				doubleSeqBlobData[] blobDataMatrix = doubleSeqBlobDataSeqHelper.extract(inSequence);
				if (blobDataMatrix != null && blobDataMatrix.length > 0) {
					if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
						// We interpret this as a time series of a single multi-valued monitor point
						MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, doubleSeqBlobDataSeqHelper.type().id());
						outList.add(mpTs);
						for (doubleSeqBlobData blobDataArray : blobDataMatrix) {
							MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
							for (double value : blobDataArray.value) {
								mpv.addValue(value); // Double
							}
							mpTs.addMonitorPointValue(mpv);
						}
					}
					else {
						// We interpret this as a time series of multiple single-valued monitor points
						populateList(outList, blobDataMatrix[0].value.length, doubleBlobDataSeqHelper.type().id());
						for (doubleSeqBlobData blobDataArray : blobDataMatrix) {
							int index = 0;
							for (double value : blobDataArray.value) {
								MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
								mpv.addValue(value); // Double
								MonitorPointTimeSeries mpTs = outList.get(index);
								mpTs.addMonitorPointValue(mpv);
								index++;
							}
						}
					}
				}
			}
	
			// floatBlobDataSeq
			// This is time series data coming from a simple property.
			else if (inSequence.type().equal(floatBlobDataSeqHelper.type())) {
				floatBlobData[] blobDataArray = floatBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, floatBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (floatBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					mpv.addValue(blobData.value); // Float
					mpTs.addMonitorPointValue(mpv);
				}
			}
			
			// floatSeqBlobDataSeq
			// This is time series data coming from a multi-valued property.
			else if (inSequence.type().equal(floatSeqBlobDataSeqHelper.type())) {
				floatSeqBlobData[] blobDataMatrix = floatSeqBlobDataSeqHelper.extract(inSequence);
				if (blobDataMatrix != null && blobDataMatrix.length > 0) {
					if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
						// We interpret this as a time series of a single multi-valued monitor point
						MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, floatSeqBlobDataSeqHelper.type().id());
						outList.add(mpTs);
						for (floatSeqBlobData blobDataArray : blobDataMatrix) {
							MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
							for (float value : blobDataArray.value) {
								mpv.addValue(value); // Float
							}
							mpTs.addMonitorPointValue(mpv);
						}
					}
					else {
						// We interpret this as a time series of multiple single-valued monitor points
						populateList(outList, blobDataMatrix[0].value.length, floatBlobDataSeqHelper.type().id());
						for (floatSeqBlobData blobDataArray : blobDataMatrix) {
							int index = 0;
							for (float value : blobDataArray.value) {
								MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
								mpv.addValue(value); // Float
								MonitorPointTimeSeries mpTs = outList.get(index);
								mpTs.addMonitorPointValue(mpv);
								index++;
							}
						}
					}
				}
			}
	
			// longBlobDataSeq
			// This is time series data coming from a simple property.
			else if (inSequence.type().equal(longBlobDataSeqHelper.type())) {
				longBlobData[] blobDataArray = longBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, longBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (longBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					mpv.addValue(blobData.value); // Long
					mpTs.addMonitorPointValue(mpv);
				}
			}
			// longSeqBlobDataSeq
			// This is time series data coming from a multi-valued property.
			else if (inSequence.type().equal(longSeqBlobDataSeqHelper.type())) {
				longSeqBlobData[] blobDataMatrix = longSeqBlobDataSeqHelper.extract(inSequence);
				if (blobDataMatrix != null && blobDataMatrix.length > 0) {
					if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
						// We interpret this as a time series of a single multi-valued monitor point
						MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, longSeqBlobDataSeqHelper.type().id());
						outList.add(mpTs);
						for (longSeqBlobData blobDataArray : blobDataMatrix) {
							MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
							for (long value : blobDataArray.value) {
								mpv.addValue(value); // Long
							}
							mpTs.addMonitorPointValue(mpv);
						}
					}
					else {
						// We interpret this as a time series of multiple single-valued monitor points
						populateList(outList, blobDataMatrix[0].value.length, longBlobDataSeqHelper.type().id());
						for (longSeqBlobData blobDataArray : blobDataMatrix) {
							int index = 0;
							for (long value : blobDataArray.value) {
								MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
								mpv.addValue(value); // Long
								MonitorPointTimeSeries mpTs = outList.get(index);
								mpTs.addMonitorPointValue(mpv);
								index++;
							}
						}
					}
				}
			}
			
			// uLongBlobDataSeq
			// This is time series data coming from a simple property.
			else if (inSequence.type().equal(uLongBlobDataSeqHelper.type())) {
				uLongBlobData[] blobDataArray = uLongBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, uLongBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (uLongBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					// TODO: use Long and fix mismatch if blobData.value < 0
					mpv.addValue(blobData.value); // Integer
					mpTs.addMonitorPointValue(mpv);
				}
			}
			// uLongSeqBlobDataSeq
			// This is time series data coming from a multi-valued property.
			else if (inSequence.type().equal(uLongSeqBlobDataSeqHelper.type())) {
				uLongSeqBlobData[] blobDataMatrix = uLongSeqBlobDataSeqHelper.extract(inSequence);
				if (blobDataMatrix != null && blobDataMatrix.length > 0) {
					if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
						// We interpret this as a time series of a single multi-valued monitor point
						MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, uLongSeqBlobDataSeqHelper.type().id());
						outList.add(mpTs);
						for (uLongSeqBlobData blobDataArray : blobDataMatrix) {
							MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
							for (int value : blobDataArray.value) {
								// TODO: use Long and fix mismatch if value < 0
								mpv.addValue(value); // Integer
							}
							mpTs.addMonitorPointValue(mpv);
						}
					}
					else {
						// We interpret this as a time series of multiple single-valued monitor points
						populateList(outList, blobDataMatrix[0].value.length, uLongBlobDataSeqHelper.type().id());
						for (uLongSeqBlobData blobDataArray : blobDataMatrix) {
							int index = 0;
							for (int value : blobDataArray.value) {
								MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
								// TODO: use Long and fix mismatch if value < 0
								mpv.addValue(value); // Integer
								MonitorPointTimeSeries mpTs = outList.get(index);
								mpTs.addMonitorPointValue(mpv);
								index++;
							}
						}
					}
				}
			}
	
			// longLongBlobDataSeq
			// This is time series data coming from a simple property.
			else if (inSequence.type().equal(longLongBlobDataSeqHelper.type())) {
				longLongBlobData[] blobDataArray = longLongBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, longLongBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (longLongBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					mpv.addValue(blobData.value); // Long
					mpTs.addMonitorPointValue(mpv);
				}
			}
			// longLongSeqBlobDataSeq
			// This is time series data coming from a multi-valued property.
			else if (inSequence.type().equal(longLongSeqBlobDataSeqHelper.type())) {
				longLongSeqBlobData[] blobDataMatrix = longLongSeqBlobDataSeqHelper.extract(inSequence);
				if (blobDataMatrix != null && blobDataMatrix.length > 0) {
					if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
						// We interpret this as a time series of a single multi-valued monitor point
						MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, longLongSeqBlobDataSeqHelper.type().id());
						outList.add(mpTs);
						for (longLongSeqBlobData blobDataArray : blobDataMatrix) {
							MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
							for (long value : blobDataArray.value) {
								mpv.addValue(value); // Long
							}
							mpTs.addMonitorPointValue(mpv);
						}
					}
					else {
						// We interpret this as a time series of multiple single-valued monitor points
						populateList(outList, blobDataMatrix[0].value.length, longLongBlobDataSeqHelper.type().id());
						for (longLongSeqBlobData blobDataArray : blobDataMatrix) {
							int index = 0;
							for (long value : blobDataArray.value) {
								MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
								mpv.addValue(value); // Long
								MonitorPointTimeSeries mpTs = outList.get(index);
								mpTs.addMonitorPointValue(mpv);
								index++;
							}
						}
					}
				}
			}
	
			// uLongLongBlobDataSeq
			// This is time series data coming from a simple property.
			else if (inSequence.type().equal(uLongLongBlobDataSeqHelper.type())) {
				uLongLongBlobData[] blobDataArray = uLongLongBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, uLongLongBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (uLongLongBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					// TODO: Use BigInteger and fix mismatch for negative values, something like 
					//          if (blobData.value < 0) {BigInteger.valueOf(blobData.value).add(new BigInteger("10000000000000000", 16)));
					//          This will not resolve overflow problems with computing the statistics though.
					//       Or better get rid of "unsigned long long" in our IDL, or replace it with something like
					//          "typedef fixed<31,0> BigInt"
					mpv.addValue(blobData.value); // Long
					mpTs.addMonitorPointValue(mpv);
				}
			}
			// uLongLongSeqBlobDataSeq
			// This is time series data coming from a multi-valued property.
			else if (inSequence.type().equal(uLongLongSeqBlobDataSeqHelper.type())) {
				uLongLongSeqBlobData[] blobDataMatrix = uLongLongSeqBlobDataSeqHelper.extract(inSequence);
				if (blobDataMatrix != null && blobDataMatrix.length > 0) {
					if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
						// We interpret this as a time series of a single multi-valued monitor point
						MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, uLongLongSeqBlobDataSeqHelper.type().id());
						outList.add(mpTs);
						for (uLongLongSeqBlobData blobDataArray : blobDataMatrix) {
							MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
							// TODO: See type mismatch comment above for uLongLongBlobDataSeq
							for (long value : blobDataArray.value) {
								mpv.addValue(value); // Long
							}
							mpTs.addMonitorPointValue(mpv);
						}
					}
					else {
						// We interpret this as a time series of multiple single-valued monitor points
						populateList(outList, blobDataMatrix[0].value.length, uLongLongBlobDataSeqHelper.type().id());
						for (uLongLongSeqBlobData blobDataArray : blobDataMatrix) {
							int index = 0;
							// TODO: See type mismatch comment above for uLongLongBlobDataSeq
							for (long value : blobDataArray.value) {
								MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
								mpv.addValue(value); // Long
								MonitorPointTimeSeries mpTs = outList.get(index);
								mpTs.addMonitorPointValue(mpv);
								index++;
							}
						}
					}
				}
			}
	
			// booleanBlobDataSeq
			// This is time series data coming from a simple property.
			else if (inSequence.type().equal(booleanBlobDataSeqHelper.type())) {
				booleanBlobData[] blobDataArray = booleanBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, booleanBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (booleanBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					mpv.addValue(blobData.value); // Boolean
					mpTs.addMonitorPointValue(mpv);
				}
			}
			// booleanSeqBlobDataSeq
			// This is time series data coming from a multi-valued property.
			else if (inSequence.type().equal(booleanSeqBlobDataSeqHelper.type())) {
				booleanSeqBlobData[] blobDataMatrix = booleanSeqBlobDataSeqHelper.extract(inSequence);
				if (blobDataMatrix != null && blobDataMatrix.length > 0) {
					if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
						// We interpret this as a time series of a single multi-valued monitor point
						MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, booleanSeqBlobDataSeqHelper.type().id());
						outList.add(mpTs);
						for (booleanSeqBlobData blobDataArray : blobDataMatrix) {
							MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
							for (boolean value : blobDataArray.value) {
								mpv.addValue(value); // Boolean
							}
							mpTs.addMonitorPointValue(mpv);
						}
					}
					else {
						// We interpret this as a time series of multiple single-valued monitor points
						populateList(outList, blobDataMatrix[0].value.length, booleanBlobDataSeqHelper.type().id());
						for (booleanSeqBlobData blobDataArray : blobDataMatrix) {
							int index = 0;
							for (boolean value : blobDataArray.value) {
								MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
								mpv.addValue(value); // Boolean
								MonitorPointTimeSeries mpTs = outList.get(index);
								mpTs.addMonitorPointValue(mpv);
								index++;
							}
						}
					}
				}
			}
	
			// patternBlobDataSeq
			else if (inSequence.type().equal(patternBlobDataSeqHelper.type())) {
				patternBlobData[] blobDataArray = patternBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, patternBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (patternBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					mpv.addValue(blobData.value); // Long
					mpTs.addMonitorPointValue(mpv);
				}
			}
			
			// stringBlobDataSeq
			else if (inSequence.type().equal(stringBlobDataSeqHelper.type())) {
				stringBlobData[] blobDataArray = stringBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, stringBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (stringBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					mpv.addValue(blobData.value); // String
					mpTs.addMonitorPointValue(mpv);
				}
			}
			// stringSeqBlobDataSeq
			else if (inSequence.type().equal(stringSeqBlobDataSeqHelper.type())) {
				stringSeqBlobData[] blobDataMatrix = stringSeqBlobDataSeqHelper.extract(inSequence);
				if (blobDataMatrix != null && blobDataMatrix.length > 0) {
					if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
						// We interpret this as a time series of a single multi-valued monitor point
						MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, stringSeqBlobDataSeqHelper.type().id());
						outList.add(mpTs);
						for (stringSeqBlobData blobDataArray : blobDataMatrix) {
							MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
							for (String value : blobDataArray.value) {
								mpv.addValue(value); // String
							}
							mpTs.addMonitorPointValue(mpv);
						}
					}
					else {
						// We interpret this as a time series of multiple single-valued monitor points
						populateList(outList, blobDataMatrix[0].value.length, stringBlobDataSeqHelper.type().id());
						for (stringSeqBlobData blobDataArray : blobDataMatrix) {
							int index = 0;
							for (String value : blobDataArray.value) {
								MonitorPointValue mpv = new MonitorPointValue(blobDataArray.time);
								mpv.addValue(value); // String
								MonitorPointTimeSeries mpTs = outList.get(index);
								mpTs.addMonitorPointValue(mpv);
								index++;
							}
						}
					}
				}
			}
	
			// enumBlobDataSeq
			else if (inSequence.type().equal(enumBlobDataSeqHelper.type())) {
				enumBlobData[] blobDataArray = enumBlobDataSeqHelper.extract(inSequence);
				MonitorPointTimeSeries mpTs = new MonitorPointTimeSeries(0, enumBlobDataSeqHelper.type().id());
				outList.add(mpTs);
				for (enumBlobData blobData : blobDataArray) {
					MonitorPointValue mpv = new MonitorPointValue(blobData.time);
					mpv.addValue(blobData.value); // Integer
					mpTs.addMonitorPointValue(mpv);
				}
			}
	
			else {
				logger.warning("Unknown CORBA data type received by blobber");
				throw new IllegalStateException("Unknown CORBA data type received, " + inSequence.type());
			}
		}
		catch (BadKind ex) {
			// This should never happen because we call TypeCode.id() only on TypeCodes
			// that do have an ID.
			logger.log(Level.WARNING, "Unexpected exception related to reading Any TypeCode IDs.", ex);
		}
		
		return outList;
	}


	/**
	 * Populate inList with the inCount number of containers.
	 */
	private void populateList(List<MonitorPointTimeSeries> inList, int inCount, String anyTypeCode) {
		for (int index = 0; index < inCount; index++) {
			inList.add(new MonitorPointTimeSeries(index, anyTypeCode));
		}
	}

}
