/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

import java.lang.reflect.Array;

import com.cosylab.util.ArrayHelper;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.datatypes.HistoryIteratorProxy;
import abeans.engine.Request;
import abeans.engine.RequestCallback;
import abeans.models.acs.baci.TypelessProperty;
import abeans.models.acs.baci.util.SequenceConverterHelper;
import abeans.models.acs.baci.util.TimeConverter;
import abeans.pluggable.RemoteException;
import abeans.pluggable.acs.maci.NarrowCORBAProxy;

/**
 * This is a proxy interface, used for direct access from modeling layer into pluggable layer,
 * bypassing Abeans Engine, to the transient resource created in response to a "history" query.
 * 
 * For ACS, iterator queries for all (less than <code>MAXIMUM_ELEMENTS</code> and
 * <code>maxElements</code>) history elements, converts the timestamps
 * and filters them using the <code>startTime</code> and <code>stopTime</code> bounds.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class HistoryIteratorProxyImpl implements HistoryIteratorProxy, Identifiable {
	
	/**
	 * Identifier.
	 */
	protected transient Identifier id;

	/**
	 * History request to be handled by this iterator.
	 */
	protected Request request;

	/**
	 * Request callback.
	 */
	protected RequestCallback rcb;
	
	/**
	 * Array of history element values.
	 */
	protected Object lvalues;
	
	/**
	 * Array of history element timestamps.
	 */
	protected long[] ltimestamps;
	
	/**
	 * Maximum history element to be prefetched.
	 */
	public static final int MAXIMUM_ELEMENTS = 100;
	
	/**
	 * Iterator index cursor.
	 */
	protected int current = 0;
	
	/**
	 * Number of element in the iterator,
	 * i.e. elements returned in the last call of getHistory() method.
	 */
	protected int iteratorExactSize = 0;

	/**
	 * Creates a new instance of proxy implementation.
	 * 
	 * @param	property		property to be queries for history, non-<code>null</code>
	 * @param	proxy			property proxy, non-<code>null</code>
	 * @param	maxElements 	the maximum number of last history entries to be iterated over
	 * @param	startTime		all returned history entries will have later time than this
	 * @param	stopTime		all returned history entries will have earlier time than this
	 * @param	request			the request in response to which this object is being instantiated,
	 * 							non-<code>null</code>
	 * @see com.cosylab.datatypes.HistoryConstraints
	 */
	public HistoryIteratorProxyImpl(TypelessProperty property, NarrowCORBAProxy proxy,
									int maxElements, long startTime, long stopTime,
									Request request, RequestCallback rcb)
		throws RemoteException
	{
		assert (request != null);
		assert (rcb != null);
		assert (maxElements >= 0);
		assert (startTime >= 0);
		assert (stopTime >= 0);
		
		this.request = request;
		this.rcb = rcb;
		
		// prefetch history elements
		initialize(property, proxy, maxElements, startTime, stopTime);
	}
	
	/**
	 * Queries property for history elements, converts the timestamps
 	 * and filters them using the <code>startTime</code> and <code>stopTime</code> bounds.
 	 * 
	 * @param	property		property to be queries for history, non-<code>null</code>
	 * @param	proxy			property proxy, non-<code>null</code>
	 * @param	maxElements 	the maximum number of last history entries to be iterated over
	 * @param	startTime		all returned history entries will have later time than this
	 * @param	stopTime		all returned history entries will have earlier time than this
	 * @throws RemoteException
	 */
	private void initialize(TypelessProperty property, NarrowCORBAProxy proxy,
							int maxElements, long startTime, long stopTime)
		throws RemoteException
	{
		if (startTime <= 0) startTime = 0;
		if (stopTime <= 0) stopTime = Long.MAX_VALUE;
		if (maxElements <= 0) maxElements = MAXIMUM_ELEMENTS;
		
		int elements = Math.min(maxElements, MAXIMUM_ELEMENTS);
		
		Object valueHolder = property.getHistoryValueHolder();
		
		alma.ACS.abeans.TimeSeqHolder times = new alma.ACS.abeans.TimeSeqHolder();
		
		proxy.invoke("get_history", new Object[] { new Integer(elements), valueHolder, times });
		
		Object values = property.extractHistoryValueHolder(valueHolder);
		
		// convert times
		long[] timestamps = times.value;
		for (int i = 0; i < timestamps.length; i++)
			timestamps[i] = TimeConverter.toJava(timestamps[i]);

		int ix1 = 0;
		for (; ix1 < timestamps.length && timestamps[ix1] < startTime; ix1++);
		
		int ix2 = ix1;
		for (; ix2 < timestamps.length && timestamps[ix2] < stopTime; ix2++);
		ix2--;

		int	size = ix2 - ix1 + 1;
		ltimestamps = new long[size];
		lvalues = Array.newInstance(property.getDataType(), size);
		System.arraycopy(values, ix1, lvalues, 0, size);
		System.arraycopy(timestamps, ix1, ltimestamps, 0, size);

		iteratorExactSize = ltimestamps.length;

		// flip
		ArrayHelper.flip(lvalues);
		ArrayHelper.flip(ltimestamps);
		
		// convert -> double[]
		if (lvalues instanceof float[])
			lvalues = SequenceConverterHelper.toAbeansSequence((float[])lvalues);

		// convert -> long[]
		else if (lvalues instanceof byte[])
			lvalues = SequenceConverterHelper.toAbeansSequence((byte[])lvalues);
		else if (lvalues instanceof short[])
			lvalues = SequenceConverterHelper.toAbeansSequence((short[])lvalues);
		else if (lvalues instanceof int[])
			lvalues = SequenceConverterHelper.toAbeansSequence((int[])lvalues);

	}

	/**
	 * Returns a chunk of history data. The history data is filtered using the
	 * <code>HistoryConstraints</code> sent in the request that created this transient proxy
	 * object. The implementation of this proxy must fill the data and the timestamps
	 * array based on the supplied constraints, up to the capacity of the created arrays.
	 * <b>This is an iterator method and will be called repeatedly: the first call returns the
	 * most recent data (so that timestamps[0] contains the very last history data element), the
	 * next call older data etc).
	 * </b>
	 * 
	 * @param		data		data array of the correct (data access) type, non-<code>null</code>
	 * @param		timestamps	time stamp array in Java time format, the same length as the data
	 * 							array, non-<code>null</code>
	 * @return					the <b>actual</b> number of history elements in the array; useful 
	 * 							because the number can be smaller than the preallocated array size
	 * @throws		RemoteException
	 * 							if the data acquisition fails in the remote layer; or if the 
	 * 							array parameters do not match in size
	 * 
	 * @see abeans.datatypes.HistoryIteratorProxy#getHistory(java.lang.Object, long[])
	 */
	public int getHistory(Object data, long[] timestamps)
		throws RemoteException
	{
			
		if (data == null || timestamps == null)
		{
			RemoteException re = new RemoteException(this, "Arrays supplied to 'getHistory' are null.");
			re.caughtIn(this, "getHistory");
			re.putValue("data", data);
			re.putValue("timestamps", timestamps);
			throw re;
		}
		
		int retVal = 0;
		int n = timestamps.length;
		if (n <= ltimestamps.length - current) retVal = n;
		else retVal = ltimestamps.length - current;
		
		if (Array.getLength(data) != n)
		{
			RemoteException re = new RemoteException(this, "Arrays supplied to 'getHistory' are not of the same length.");
			re.putValue("data", data);
			re.putValue("data.length", new Integer(Array.getLength(data)));
			re.putValue("timestamps", timestamps);
			re.putValue("timestamps.length", new Integer(timestamps.length));
			throw re;
		}
		
		System.arraycopy(lvalues, current, data, 0, retVal);
		System.arraycopy(ltimestamps, current, timestamps, 0, retVal);
		current = current + retVal;
		
		// fire requestEnds, if iterator has reached the end
		if (current >= ltimestamps.length)
			rcb.requestEnds(request);

		return retVal;
	}

	/**
	 * Returns the total size of data items in the iterator. The design contract for this method
	 * is stronger than for the method of the <code>HistoryIterator</code> from Datatypes: this 
	 * method returns the exact value. 
	 * 
	 * @return					the amount of data in the iterator
	 * @throws	RemoteException	if the call is remote and it fails
	 * 
	 * @see abeans.datatypes.HistoryIteratorProxy#exactSize()
	 */
	public int exactSize() throws RemoteException
	{
		return iteratorExactSize;
	}

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier() {
		if (id == null)
			id = new IdentifierSupport("History Proxy Implementation", getClass().getName(), Identifier.PLUG);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}
}
