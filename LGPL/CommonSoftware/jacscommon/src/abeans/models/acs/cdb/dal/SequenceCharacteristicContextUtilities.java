/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.cdb.dal;

import java.util.logging.Level;

import com.cosylab.util.NameValueList;

import abeans.core.defaults.MessageLogEntry;
import abeans.datatypes.AbeansDataExchangeException;
import abeans.datatypes.TypelessProperty;
import abeans.datatypes.UpdateableUtilities;
import abeans.engine.RequestException;
import abeans.engine.RequestInterceptor;
import abeans.engine.Response;
import abeans.engine.ResponseType;
import abeans.engine.SimpleRequest;
import abeans.models.RequestUtilities;
import abeans.pluggable.Proxy;
import abeans.pluggable.RemoteInfo;

/**
 * This is a class containing convenience methods for accessing sequence characteristics, as
 * defined by the Datatypes and its mapping into <code>abeans.datatypes</code>.
 * Access to characteristics is typed (i.e. this class contains different methods
 * for accessing characteristics of different types). The access to a characteristic element
 * consists of creating a suitable Abeans Engine <code>abeans.engine.Request</code>, populating 
 * it with the neccessary data, submitting the request to the Abeans Engine database, unpacking
 * the reply and returning the result.
 * <p>
 * The implementations of this class consist of the procedures described in the Datatypes to 
 * Abeans document mapping: 
 * <ol>
 * 	<li>
 * 		A new request is created by obtaining the database from the <code>invoker</code> parameter
 * 		of the methods, and invoking one of the request factory methods on the database.
 * 	</li>
 * 	<li>
 * 		The request is populated with indexing data. All characteristics accesses are indexed by
 * 		<code>UpdateableUtilities.LATEST_REQUEST_INDEX_KEY</code> index and their key is the
 * 		<code>invoker</code> parameter.
 * 	</li>
 * 	<li>
 * 		The type of the sequence characteristics (and thus the type of the <code>abeans.engine.Response</code>
 * 		is inserted into the request properties). The type is based on the characteristics access
 * 		method chosen.
 * 	</li>
 * 	<li>
 * 		The target of the request is set to the remote info provided as the method parameter plus
 * 		the characteristic name and the "get" query invoked on it.
 * 	</li>
 * 	<li>
 * 		The timeout is set to the specified timeout parameter.
 * 	</li>
 * 	<li>
 * 		The <code>interceptor</code>, if non-<code>null</code>, is called and passed the
 * 		request, before the request is submitted to the database. The interceptor may 
 * 		do some additional processing on the request.
 * 	</li>
 * 	<li>
 * 		The request is submitted to the database. It is a singe response, blocking call
 * 		request, as required by the Datatypes to Abeans mapping.
 * 	</li>
 * 	<li>
 * 		The <code>interceptor</code>, if non-<code>null</code>, is first called and passed
 * 		the request to perform post-submittal processing, and then called again with the
 * 		same request to perform error check on the request.
 * 	</li>
 * 	<li>
 * 		The value is extracted from the signle response of the request and returned from 
 * 		the method call. This completes the characteristic access processing.
 * 	</li>
 * </ol>
 * <p>
 * Note that this class is final, non-instantiable (it has a private constructor), and consists
 * of static methods only. Modeling elements that are Datatypes compliant and wish to access
 * the characteristics may delegate their calls to the implementations of this class.
 * </p> 
 * 
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public final class SequenceCharacteristicContextUtilities
{
	/**
	 * Private constructor. This is a non-instantiable utility class.
	 */
	private SequenceCharacteristicContextUtilities()
	{
	}

	/**
	 * Accesses a sequence characteristic of a double type. See class javadoc to see a description of the
	 * standard characteristic get processing implemented by this method.
	 * 
	 * @param		parentInfo		the remote info to which the characteristic name and the get 	
	 * 								query will be appended; this remote info will be used as the 
	 * 								request target; non-<code>null</code>
	 * @param		name			the name of the characteristics, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @param		timeout			the timeout to use with the request, in milliseconds, or 
	 * 								0 if the request is not to be timed
	 * @param		interceptor		the interceptor that will be passed the request for pre-submittal
	 * 								and post-submittal processing, may be <code>null</code> in which
	 * 								case no such processing will occur
	 * @param		invoker			the property that owns the characteristic; the database of this
	 * 								property will be used to submit the request; this property will
	 * 								be used as the latest request index key, non-<code>null</code>
	 * @return						the characteristic value
	 * @throws		AbeansDataExchangeException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	public static double[] getDoubleSeqCharacteristic(RemoteInfo parentInfo, String name, Proxy proxy, long timeout, RequestInterceptor interceptor, TypelessProperty invoker) throws AbeansDataExchangeException
	{
		return (double[])internalGetCharacteristic(parentInfo, name, proxy, timeout, interceptor, invoker, ResponseType.DOUBLE_SEQ);
	} 
	
	/**
	 * Accesses a sequence characteristic of a string type. See class javadoc to see a description of the
	 * standard characteristic get processing implemented by this method.
	 * 
	 * @param		parentInfo		the remote info to which the characteristic name and the get 	
	 * 								query will be appended; this remote info will be used as the 
	 * 								request target; non-<code>null</code>
	 * @param		name			the name of the characteristics, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @param		timeout			the timeout to use with the request, in milliseconds, or 
	 * 								0 if the request is not to be timed
	 * @param		interceptor		the interceptor that will be passed the request for pre-submittal
	 * 								and post-submittal processing, may be <code>null</code> in which
	 * 								case no such processing will occur
	 * @param		invoker			the property that owns the characteristic; the database of this
	 * 								property will be used to submit the request; this property will
	 * 								be used as the latest request index key, non-<code>null</code>
 	 * @return						the characteristic value
	 * @throws		AbeansDataExchangeException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	public static String[] getStringSeqCharacteristic(RemoteInfo parentInfo, String name, Proxy proxy, long timeout, RequestInterceptor interceptor, TypelessProperty invoker) throws AbeansDataExchangeException
	{
		return (String[])internalGetCharacteristic(parentInfo, name, proxy, timeout, interceptor, invoker, ResponseType.STRING_SEQ);
	}
	
	/**
	 * Accesses a sequence characteristic of a long type. See class javadoc to see a description of the
	 * standard characteristic get processing implemented by this method.
	 * 
	 * @param		parentInfo		the remote info to which the characteristic name and the get 	
	 * 								query will be appended; this remote info will be used as the 
	 * 								request target; non-<code>null</code>
	 * @param		name			the name of the characteristics, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @param		timeout			the timeout to use with the request, in milliseconds, or 
	 * 								0 if the request is not to be timed
	 * @param		interceptor		the interceptor that will be passed the request for pre-submittal
	 * 								and post-submittal processing, may be <code>null</code> in which
	 * 								case no such processing will occur
	 * @param		invoker			the property that owns the characteristic; the database of this
	 * 								property will be used to submit the request; this property will
	 * 								be used as the latest request index key, non-<code>null</code>
	 * @return						the characteristic value
	 * @throws		AbeansDataExchangeException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	public static int[] getIntegerSeqCharacteristic(RemoteInfo parentInfo, String name, Proxy proxy, long timeout, RequestInterceptor interceptor, TypelessProperty invoker) throws AbeansDataExchangeException
	{
		return (int[])internalGetCharacteristic(parentInfo, name, proxy, timeout, interceptor, invoker, ResponseType.LONG_SEQ);
	} 
	
	/**
	 * Accesses a sequence characteristic of a long type. See class javadoc to see a description of the
	 * standard characteristic get processing implemented by this method.
	 * 
	 * @param		parentInfo		the remote info to which the characteristic name and the get 	
	 * 								query will be appended; this remote info will be used as the 
	 * 								request target; non-<code>null</code>
	 * @param		name			the name of the characteristics, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @param		timeout			the timeout to use with the request, in milliseconds, or 
	 * 								0 if the request is not to be timed
	 * @param		interceptor		the interceptor that will be passed the request for pre-submittal
	 * 								and post-submittal processing, may be <code>null</code> in which
	 * 								case no such processing will occur
	 * @param		invoker			the property that owns the characteristic; the database of this
	 * 								property will be used to submit the request; this property will
	 * 								be used as the latest request index key, non-<code>null</code>
	 * @return						the characteristic value
	 * @throws		AbeansDataExchangeException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	/*
	public static long[] getLongSeqCharacteristic(RemoteInfo parentInfo, String name, Proxy proxy, long timeout, RequestInterceptor interceptor, TypelessProperty invoker) throws AbeansDataExchangeException
	{
		return (long[])internalGetCharacteristic(parentInfo, name, proxy, timeout, interceptor, invoker, ResponseType.LONG_SEQ);
	} 
	*/
	
	/**
	 * Accesses a sequence characteristic the type of which will be determined at runtime.
	 * This is known as a dynamic characteristic in datatypes. 
	 * See class javadoc to see a description of the
	 * standard characteristic get processing implemented by this method.
	 * 
	 * @param		parentInfo		the remote info to which the characteristic name and the get 	
	 * 								query will be appended; this remote info will be used as the 
	 * 								request target; non-<code>null</code>
	 * @param		name			the name of the characteristics, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @param		timeout			the timeout to use with the request, in milliseconds, or 
	 * 								0 if the request is not to be timed
	 * @param		interceptor		the interceptor that will be passed the request for pre-submittal
	 * 								and post-submittal processing, may be <code>null</code> in which
	 * 								case no such processing will occur
	 * @param		invoker			the property that owns the characteristic; the database of this
	 * 								property will be used to submit the request; this property will
	 * 								be used as the latest request index key, non-<code>null</code>
	 * @return						the characteristic value
	 * @throws		AbeansDataExchangeException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	public static Object getSeqCharacteristic(RemoteInfo parentInfo, String name, Proxy proxy, long timeout, RequestInterceptor interceptor, TypelessProperty invoker) throws AbeansDataExchangeException
	{
		return (Object[])internalGetCharacteristic(parentInfo, name, proxy, timeout, interceptor, invoker, ResponseType.OBJECT);
	} 

	/**
	 * Accesses a characteristic the type of which will be determined at runtime.
	 * This is known as a dynamic characteristic in datatypes. 
	 * See class javadoc to see a description of the
	 * standard characteristic get processing implemented by this method.
	 * 
	 * @param		parentInfo		the remote info to which the characteristic name and the get 	
	 * 								query will be appended; this remote info will be used as the 
	 * 								request target; non-<code>null</code>
	 * @param		name			the name of the characteristics, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @param		timeout			the timeout to use with the request, in milliseconds, or 
	 * 								0 if the request is not to be timed
	 * @param		interceptor		the interceptor that will be passed the request for pre-submittal
	 * 								and post-submittal processing, may be <code>null</code> in which
	 * 								case no such processing will occur
	 * @param		invoker			the property that owns the characteristic; the database of this
	 * 								property will be used to submit the request; this property will
	 * 								be used as the latest request index key, non-<code>null</code>
	 * @param		seqElemType		type of sequence element, additional "sequenceElementType"
	 * 								is added to indicate sequence type request, non-<code>null</code>
	 * @return						the characteristic value
	 * @throws		AbeansDataExchangeException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	private static Object internalGetCharacteristic(RemoteInfo parentInfo, String name, Proxy proxy, long timeout, RequestInterceptor interceptor, TypelessProperty invoker, ResponseType seqElemType) throws AbeansDataExchangeException
	{
		assert (parentInfo != null);
		assert (name != null);
		assert (name.length() == 0);
		assert (proxy != null);
		assert (timeout >= 0);
		assert (invoker != null);
		
		// TODO logging reference - prevents GC to terminate connection (JacORB)
		if (invoker.isDebug()) new MessageLogEntry(invoker, "internalGetCharacteristic::getCharacteristic", new Object[] { parentInfo, name, proxy, new Long(timeout), interceptor, invoker }).dispatch();
		
		try
		{
			SimpleRequest sr = invoker.getDatabase().createSimpleRequest(true);
			sr.setProxy(proxy);
			NameValueList nvl = new NameValueList(3);
			nvl.put(UpdateableUtilities.LATEST_REQUEST_INDEX_KEY, invoker);
			nvl.put("type", seqElemType);
			sr.setProperties(nvl);
			sr.setTarget(parentInfo.createHierarchyAndQuery(name, "get"));
			sr.setTimeoutDelta(timeout);
			if (interceptor != null) interceptor.checkRequestBeforeSubmittal(sr);
			RequestUtilities.doSubmit(invoker.getDatabase(), sr);
			if (interceptor != null) interceptor.checkRequestAfterSubmittal(sr);
			if (interceptor != null) interceptor.checkRequestForErrors(sr, true);
			Response rp = sr.getLastResponse();
			Object retVal = rp.getValueAsObject();
			if (invoker.isDebug()) new MessageLogEntry(invoker, "internalGetCharacteristic::getCharacteristic", "Read characteristc '" + name + "', value is " + retVal + ".", Level.FINE).dispatch();
			if (invoker.isDebug()) new MessageLogEntry(invoker, "internalGetCharacteristic::getCharacteristic", "Exiting.", Level.FINEST).dispatch();
			return retVal;
		} catch (RequestException re)
		{
			throw new AbeansDataExchangeException(invoker, "Exception while executing Abeans Engine sequence characteristic get reqeust.", re);
		}	
	} 
}
