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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.omg.CORBA.Any;

import alma.ACSErrTypeCommon.wrappers.AcsJNoResourcesEx;
import alma.TMCDB.anyBlobData;
import alma.TMCDB.anyBlobDataSeqHelper;
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
import alma.acs.monitoring.blobber.BlobberWorker.AnyDataContainer;

/**
 * TODO: Refactor this class to separate the steps of 
 * - unpacking corba any,
 * - fixing device dependent issues (isUnique etc), and
 * - creating the CLOB  
 * 
 * @author hsommer
 *
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
	 * HSO TODO: IDL struct MonitorBlob should be defined without using Corba Any (performance!). 
	 * Rather use separate fields for the possible data types.
	 * 
	 * @param inSequence
	 *            The sequence of data from one property (monitor point) from one collector interval.
	 * @param propertyName
	 * @return
	 * @throws AcsJNoResourcesEx 
	 */
	List<AnyDataContainer> extractData(Any inSequence, String propertyName) throws AcsJNoResourcesEx {
		
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
		}

		// doubleSeqBlobDataSeq
		// Data coming from composite property
		// For this case, the index, i.e., the position inside the sequence varies
		else if (inSequence.type().equal(doubleSeqBlobDataSeqHelper.type())) {
			doubleSeqBlobData[] blobDataMatrix = doubleSeqBlobDataSeqHelper.extract(inSequence);
			if (blobDataMatrix != null && blobDataMatrix.length > 0) {
				// If the monitor point is unique
				if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
					AnyDataContainer container = new AnyDataContainer();
					int index = 0;
					for (doubleSeqBlobData blobDataArray : blobDataMatrix) {
						// We interpret blobDataArray as the multiple values of a single monitor point at a given time
						populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
					}
					outList.add(container);
				}
				// If the monitor point is not unique
				else {
					populateList(outList, blobDataMatrix[0].value.length);
					for (doubleSeqBlobData blobDataArray : blobDataMatrix) {
						int index = 0;
						for (Number blobData : blobDataArray.value) {
							// We interpret blobDataArray as values of multiple (single-valued) monitor points at a given time
							AnyDataContainer container = outList.get(index);
							populateContainerNumeric(container, blobDataArray.time, blobData, index);
							index++;
						}
					}
				}
			}
		}

		// floatBlobDataSeq
		// Data coming from simple property
		// For this case, the index, i.e., the position inside the sequence is 0
		else if (inSequence.type().equal(floatBlobDataSeqHelper.type())) {
			floatBlobData[] blobDataArray = floatBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (floatBlobData blobData : blobDataArray)
				populateContainerNumeric(container, blobData.time, blobData.value, index);
			outList.add(container);
		}

		// floatSeqBlobDataSeq
		// Data coming from composite property
		// For this case, the index, i.e., the position inside the sequence varies
		else if (inSequence.type().equal(floatSeqBlobDataSeqHelper.type())) {
			floatSeqBlobData[] blobDataMatrix = floatSeqBlobDataSeqHelper.extract(inSequence);
			if (blobDataMatrix != null && blobDataMatrix.length > 0) {
				// If the monitor point is unique
				if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
					AnyDataContainer container = new AnyDataContainer();
					int index = 0;
					for (floatSeqBlobData blobDataArray : blobDataMatrix)
						populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
					outList.add(container);
				}
				// If the monitor point is not unique
				else {
					populateList(outList, blobDataMatrix[0].value.length);
					for (floatSeqBlobData blobDataArray : blobDataMatrix) {
						int index = 0;
						for (Number blobData : blobDataArray.value) {
							AnyDataContainer container = outList.get(index);
							populateContainerNumeric(container, blobDataArray.time, blobData, index);
							index++;
						}
					}
				}
			}
		}

		// longBlobDataSeq
		else if (inSequence.type().equal(longBlobDataSeqHelper.type())) {
			longBlobData[] blobDataArray = longBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (longBlobData blobData : blobDataArray)
				populateContainerNumeric(container, blobData.time, blobData.value, index);
			outList.add(container);
		}

		// longSeqBlobDataSeq
		else if (inSequence.type().equal(longSeqBlobDataSeqHelper.type())) {
			longSeqBlobData[] blobDataMatrix = longSeqBlobDataSeqHelper.extract(inSequence);
			if (blobDataMatrix != null && blobDataMatrix.length > 0) {
				// If the monitor point is unique
				if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
					AnyDataContainer container = new AnyDataContainer();
					int index = 0;
					for (longSeqBlobData blobDataArray : blobDataMatrix)
						populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
					outList.add(container);
				}
				// If the monitor point is not unique
				else {
					populateList(outList, blobDataMatrix[0].value.length);
					for (longSeqBlobData blobDataArray : blobDataMatrix) {
						int index = 0;
						for (Number blobData : blobDataArray.value) {
							AnyDataContainer container = outList.get(index);
							populateContainerNumeric(container, blobDataArray.time, blobData, index);
							index++;
						}
					}
				}
			}
		}

		// uLongBlobDataSeq
		else if (inSequence.type().equal(uLongBlobDataSeqHelper.type())) {
			uLongBlobData[] blobDataArray = uLongBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (uLongBlobData blobData : blobDataArray) {
				// TODO: use long and fix mismatch if blobData.value < 0
				populateContainerNumeric(container, blobData.time, blobData.value, index);
			}
			outList.add(container);
		}

		// uLongSeqBlobDataSeq
		else if (inSequence.type().equal(uLongSeqBlobDataSeqHelper.type())) {
			uLongSeqBlobData[] blobDataMatrix = uLongSeqBlobDataSeqHelper.extract(inSequence);
			if (blobDataMatrix != null && blobDataMatrix.length > 0) {
				// If the monitor point is unique
				if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
					AnyDataContainer container = new AnyDataContainer();
					int index = 0;
					for (uLongSeqBlobData blobDataArray : blobDataMatrix) {
						// TODO: use long[] and fix mismatch for blobDataArray.value values < 0
						populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
					}
					outList.add(container);
				}
				// If the monitor point is not unique
				else {
					populateList(outList, blobDataMatrix[0].value.length);
					for (uLongSeqBlobData blobDataArray : blobDataMatrix) {
						int index = 0;
						for (Integer blobData : blobDataArray.value) {
							// TODO: use Long and fix mismatch for blobData < 0
							AnyDataContainer container = outList.get(index);
							populateContainerNumeric(container, blobDataArray.time, blobData, index);
							index++;
						}
					}
				}
			}
		}

		// longLongBlobDataSeq
		else if (inSequence.type().equal(longLongBlobDataSeqHelper.type())) {
			longLongBlobData[] blobDataArray = longLongBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (longLongBlobData blobData : blobDataArray)
				populateContainerNumeric(container, blobData.time, blobData.value, index);
			outList.add(container);
		}

		// longLongSeqBlobDataSeq
		else if (inSequence.type().equal(longLongSeqBlobDataSeqHelper.type())) {
			longLongSeqBlobData[] blobDataMatrix = longLongSeqBlobDataSeqHelper.extract(inSequence);
			if (blobDataMatrix != null && blobDataMatrix.length > 0) {
				// If the monitor point is unique
				if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
					AnyDataContainer container = new AnyDataContainer();
					int index = 0;
					for (longLongSeqBlobData blobDataArray : blobDataMatrix)
						populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
					outList.add(container);
				}
				// If the monitor point is not unique
				else {
					populateList(outList, blobDataMatrix[0].value.length);
					for (longLongSeqBlobData blobDataArray : blobDataMatrix) {
						int index = 0;
						for (Number blobData : blobDataArray.value) {
							AnyDataContainer container = outList.get(index);
							populateContainerNumeric(container, blobDataArray.time, blobData, index);
							index++;
						}
					}
				}
			}
		}

		// uLongLongBlobDataSeq
		else if (inSequence.type().equal(uLongLongBlobDataSeqHelper.type())) {
			uLongLongBlobData[] blobDataArray = uLongLongBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (uLongLongBlobData blobData : blobDataArray) {
				// TODO: Use BigInteger and fix mismatch for negative values,
				//          e.g. if (blobData.value < 0) {BigInteger.valueOf(blobData.value).add(new BigInteger("10000000000000000", 16)));
				//          This will not resolve problems with computing the statistics though.
				//       Or better get rid of "unsigned long long" in our IDL, or replace it with something like
				//          "typedef fixed<31,0> BigInt"
				populateContainerNumeric(container, blobData.time, blobData.value, index);
			}
			outList.add(container);
		}

		// uLongLongSeqBlobDataSeq
		else if (inSequence.type().equal(uLongLongSeqBlobDataSeqHelper.type())) {
			uLongLongSeqBlobData[] blobDataMatrix = uLongLongSeqBlobDataSeqHelper.extract(inSequence);
			if (blobDataMatrix != null && blobDataMatrix.length > 0) {
				// If the monitor point is unique
				if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
					AnyDataContainer container = new AnyDataContainer();
					int index = 0;
					for (uLongLongSeqBlobData blobDataArray : blobDataMatrix) {
						// TODO: See type mismatch comment above for uLongLongBlobDataSeq
						populateContainerNumericArray(container, blobDataArray.time, blobDataArray.value, index);
					}
				}
				// If the monitor point is not unique
				else {
					populateList(outList, blobDataMatrix[0].value.length);
					for (uLongLongSeqBlobData blobDataArray : blobDataMatrix) {
						int index = 0;
						for (Long blobData : blobDataArray.value) {
							AnyDataContainer container = outList.get(index);
							// TODO: See type mismatch comment above for uLongLongBlobDataSeq
							populateContainerNumeric(container, blobDataArray.time, blobData, index);
							index++;
						}
					}
				}
			}
		}

		// booleanBlobDataSeq
		else if (inSequence.type().equal(booleanBlobDataSeqHelper.type())) {
			booleanBlobData[] blobDataArray = booleanBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (booleanBlobData blobData : blobDataArray)
				populateContainerBoolean(container, blobData.time, blobData.value, index);
			outList.add(container);
		}

		// booleanSeqBlobDataSeq
		else if (inSequence.type().equal(booleanSeqBlobDataSeqHelper.type())) {
			booleanSeqBlobData[] blobDataMatrix = booleanSeqBlobDataSeqHelper.extract(inSequence);
			if (blobDataMatrix != null && blobDataMatrix.length > 0) {
				// If the monitor point is unique
				if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
					AnyDataContainer container = new AnyDataContainer();
					int index = 0;
					for (booleanSeqBlobData blobDataArray : blobDataMatrix)
						populateContainerBooleanArray(container, blobDataArray.time, blobDataArray.value, index);
				}
				// If the monitor point is not unique
				else {
					populateList(outList, blobDataMatrix[0].value.length);
					for (booleanSeqBlobData blobDataArray : blobDataMatrix) {
						int index = 0;
						for (Boolean blobData : blobDataArray.value) {
							AnyDataContainer container = outList.get(index);
							populateContainerBoolean(container, blobDataArray.time, blobData, index);
							index++;
						}
					}
				}
			}
		}

		// patternBlobDataSeq
		else if (inSequence.type().equal(patternBlobDataSeqHelper.type())) {
			patternBlobData[] blobDataArray = patternBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (patternBlobData blobData : blobDataArray)
				populateContainerObject(container, blobData.time, blobData.value, index);
			outList.add(container);
		}

		// stringBlobDataSeq
		else if (inSequence.type().equal(stringBlobDataSeqHelper.type())) {
			stringBlobData[] blobDataArray = stringBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (stringBlobData blobData : blobDataArray)
				populateContainerObject(container, blobData.time, blobData.value, index);
			outList.add(container);
		}

		// stringSeqBlobDataSeq
		else if (inSequence.type().equal(stringSeqBlobDataSeqHelper.type())) {
			stringSeqBlobData[] blobDataMatrix = stringSeqBlobDataSeqHelper.extract(inSequence);
			if (blobDataMatrix != null && blobDataMatrix.length > 0) {
				// If the monitor point is unique
				if (monitorPointExpert.isMultivaluedMonitorPoint(propertyName)) {
					AnyDataContainer container = new AnyDataContainer();
					int index = 0;
					for (stringSeqBlobData blobDataArray : blobDataMatrix)
						populateContainerObjectArray(container, blobDataArray.time, blobDataArray.value, index);
				}
				// If the monitor point is not unique
				else {
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
			}
		}

		// enumBlobDataSeq
		else if (inSequence.type().equal(enumBlobDataSeqHelper.type())) {
			enumBlobData[] blobDataArray = enumBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (enumBlobData blobData : blobDataArray)
				populateContainerObject(container, blobData.time, blobData.value, index);
			outList.add(container);
		}

		// anyBlobDataSeq
		else if (inSequence.type().equal(anyBlobDataSeqHelper.type())) {
			anyBlobData[] blobDataArray = anyBlobDataSeqHelper.extract(inSequence);
			AnyDataContainer container = new AnyDataContainer();
			int index = 0;
			for (anyBlobData blobData : blobDataArray)
				populateContainerObject(container, blobData.time, blobData.value, index);
			outList.add(container);
		}

		else {
			logger.warning("Unknown CORBA data type received by blobber");
			throw new IllegalStateException("Unknown CORBA data type received, " + inSequence.type());
		}

		// As the final step we remove the last vertical bar and add end of line
		// to the clob.
		for (java.util.Iterator<AnyDataContainer> i = outList.iterator(); i.hasNext();) {
			AnyDataContainer container = i.next();
			if (container.clobBuilder.length() == 0) {
				/*
				 * If the container is of zero length we remove it from the returned list.
				 */
				i.remove();
			} else {
				container.clobBuilder.setLength(container.clobBuilder.length() - 1);
				container.clobBuilder.append("\n");
			}
		}
		return outList;
	}

	
	// For float[]
	void populateContainerNumericArray(AnyDataContainer inContainer, long inTime, float[] inData, int inIndex) {
		String numbersString = "";
		String numberString = "";
		DecimalFormat df = new DecimalFormat("###.#######");
		df.setMinimumFractionDigits(1);
		if (inData != null && inData.length > 0) {
			for (int i = 0; i < inData.length; i++) {
				numberString = df.format(inData[i]);
				if (i < inData.length - 1)
					numbersString += numberString + " ";
				else
					numbersString += numberString;
			}
		}

		inContainer.dataList.add(numbersString);
		inContainer.clobBuilder.append(inTime);
		inContainer.clobBuilder.append("|");
		inContainer.clobBuilder.append(numbersString);
		inContainer.clobBuilder.append("|");
		inContainer.index = inIndex;
	}

	// For double[]
	void populateContainerNumericArray(AnyDataContainer inContainer, long inTime, double[] inData, int inIndex) {
		String numbersString = "";
		String numberString = "";
		DecimalFormat df = new DecimalFormat("###.#########");
		df.setMinimumFractionDigits(1);
		if (inData != null && inData.length > 0) {
			for (int i = 0; i < inData.length; i++) {
				numberString = df.format(inData[i]);
				if (i < inData.length - 1)
					numbersString += numberString + " ";
				else
					numbersString += numberString;
			}
		}

		inContainer.dataList.add(numbersString);
		inContainer.clobBuilder.append(inTime);
		inContainer.clobBuilder.append("|");
		inContainer.clobBuilder.append(numbersString);
		inContainer.clobBuilder.append("|");
		inContainer.index = inIndex;
	}

	// For int[]
	void populateContainerNumericArray(AnyDataContainer inContainer, long inTime, int[] inData, int inIndex) {
		String numbersString = "";
		String numberString = "";
		DecimalFormat df = new DecimalFormat("###.#########");
		if (inData != null && inData.length > 0) {
			for (int i = 0; i < inData.length; i++) {
				numberString = df.format(inData[i]);
				if (i < inData.length - 1)
					numbersString += numberString + " ";
				else
					numbersString += numberString;
			}
		}

		inContainer.dataList.add(numbersString);
		inContainer.clobBuilder.append(inTime);
		inContainer.clobBuilder.append("|");
		inContainer.clobBuilder.append(numbersString);
		inContainer.clobBuilder.append("|");
		inContainer.index = inIndex;
	}

	// For long[]
	void populateContainerNumericArray(AnyDataContainer inContainer, long inTime, long[] inData, int inIndex) {
		String numbersString = "";
		String numberString = "";
		DecimalFormat df = new DecimalFormat("###.#########");
		if (inData != null && inData.length > 0) {
			for (int i = 0; i < inData.length; i++) {
				numberString = df.format(inData[i]);
				if (i < inData.length - 1)
					numbersString += numberString + " ";
				else
					numbersString += numberString;
			}
		}

		inContainer.dataList.add(numbersString);
		inContainer.clobBuilder.append(inTime);
		inContainer.clobBuilder.append("|");
		inContainer.clobBuilder.append(numbersString);
		inContainer.clobBuilder.append("|");
		inContainer.index = inIndex;
	}

	// For boolean[]
	void populateContainerBooleanArray(AnyDataContainer inContainer, long inTime, boolean[] inData, int inIndex) {
		String numbersString = "";
		if (inData != null && inData.length > 0) {
			for (int i = 0; i < inData.length; i++) {
				if (i < inData.length - 1)
					numbersString += (new Boolean(inData[i])) + " ";
				else
					numbersString += (new Boolean(inData[i]));
			}
		}

		inContainer.dataList.add(numbersString);
		inContainer.clobBuilder.append(inTime);
		inContainer.clobBuilder.append("|");
		inContainer.clobBuilder.append(numbersString);
		inContainer.clobBuilder.append("|");
		inContainer.index = inIndex;
	}

	// For Object[]
	void populateContainerObjectArray(AnyDataContainer inContainer, long inTime, Object[] inData, int inIndex) {
		String numbersString = "";
		if (inData != null && inData.length > 0) {
			for (int i = 0; i < inData.length; i++) {
				if (i < inData.length - 1)
					numbersString += inData[i] + " ";
				else
					numbersString += inData[i];
			}
		}

		inContainer.dataList.add(numbersString);
		inContainer.clobBuilder.append(inTime);
		inContainer.clobBuilder.append("|");
		inContainer.clobBuilder.append(numbersString);
		inContainer.clobBuilder.append("|");
		inContainer.index = inIndex;
	}

	/**
	 * 
	 * @param inData Auto-converted int, long, float, or double
	 */
	void populateContainerNumeric(AnyDataContainer inContainer, long inTime, Number inData, int inIndex) {

		// Skip NaN values (COMP-5564)
		if (inData instanceof Float && ((Float)inData).isNaN() ||
			inData instanceof Double && ((Double)inData).isNaN() ) { 
			return; 
		}

		inContainer.dataList.add(inData);

		String numberString = inData.toString();
		inContainer.clobBuilder.append(inTime);
		inContainer.clobBuilder.append("|");
		inContainer.clobBuilder.append(numberString);
		inContainer.clobBuilder.append("|");
		inContainer.index = inIndex;
	}

	void populateContainerBoolean(AnyDataContainer inContainer, long inTime, Boolean inData, int inIndex) {
		String booleanString = (inData ? "1" : "0");
		inContainer.dataList.add(inData);
		inContainer.clobBuilder.append(inTime);
		inContainer.clobBuilder.append("|");
		inContainer.clobBuilder.append(booleanString);
		inContainer.clobBuilder.append("|");
		inContainer.index = inIndex;
	}

	void populateContainerObject(AnyDataContainer inContainer, long inTime, Object inData, int inIndex) {
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

}
