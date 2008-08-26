/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.jlog.test;

import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Random;
import java.util.Vector;

import com.cosylab.logging.client.cache.ILogMap;
import com.cosylab.logging.client.cache.LogBufferedFileCache;
import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogFileCache;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.util.IsoDateFormat;

/**
 * A collection of methods to be used in the tests
 * 
 * @author acaproni
 *
 */
public class CacheUtils {
	
	// The header and footer to create log to fill the caches
	public static final String logHeaderStr = " TimeStamp=\"";
	public static final String logBodyStr = "\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[";
	public static final String logFooterStr = "]]></";
	
	// The parser
	private static ACSLogParser parser;
	
	// The type of cache to create
	public static final int LOGFILECACHE_TYPE=0;
	public static final int LOGBUFFEREDFILECACHE_TYPE=1;
	public static final int LOGCACHE_TYPE=2;
	
	// The number of possible cache types
	public static final int NUMOFCACHETYPES=3;
	
	/**
	 * Generate a random collection of keys.
	 * 
	 * The size of the collection can be random or fixed.
	 * If it is random then the exact parameter must be false; in that case
	 * the number of elements returned is limited by size (inclusive)
	 * The number of elements in the collection is fixed if the exact parameter
	 * is true; in that case the number of elements returned is specified in size.
	 * 
	 * If the size is random, the returned collection can be empty.
	 * 
	 * If common is not null, each generated key must also be present in common.
	 * For this to work, the size of common is an upper limit for the size of the 
	 * returned collection.
	 * 
	 * @param size The upper limit of the number of elements in the collection 
	 * 			   or the number of elements in the returned collection depending
	 * 			   on the value of the exact parameter 
	 * @param exact If true the size of the returned collection is equal to
	 * 				size otherwise size is the maximum limit
	 * @param minValue The minimum allowed value of the keys in the collection (inclusive)
	 * @param maxValue The maximum allowed value of the keys in the collection (inclusive)
	 * @param common A collection of keys 
	 *               They key in the returned collection must be present
	 *               in common too
	 *               If common is null the keys are generated without constraints 
	 * @return A posible empty collection of keys
	 */
	public static Collection<Integer> generateKeys(
			int size, 
			boolean exact, 
			int minValue,
			int maxValue, 
			Collection<Integer> common) {
		if (size<=0) {
			throw new IllegalArgumentException("Invalid size");
		}
		if (minValue<0 && maxValue<0) {
			throw new IllegalArgumentException("Illegal min/max value below 0");
		}
		if (minValue>=maxValue) {
			throw new IllegalArgumentException("minValue greater or equal to maxValue");
		}
		if (exact && size>common.size()) {
			throw new IllegalArgumentException("Impossible to generate the keys against the passed collection");
		}
		int desiredLength=0;
		Random rnd = new Random(System.currentTimeMillis());
		if (!exact) {
			// size is an upper limit (inclusive)
			while (desiredLength==0 || (maxValue-minValue+1<desiredLength)) {
				desiredLength=rnd.nextInt(size+1);
			}
		} else {
			desiredLength=size;
		}
		Vector<Integer> v = new Vector<Integer>(desiredLength);
		if (desiredLength==common.size()) {
			// we have to return a collection with the same elements
			// contained in common
			for (Integer key: common) {
				v.add(key);
			}
		} else {
			Integer key=-1;
			while (v.size()<desiredLength) {
				// Generate the new key
				while (key<minValue || key>maxValue) {
					key = rnd.nextInt(maxValue+1);
					if ((common!=null && !common.contains(key)) || v.contains(key)) {
						key=-1;
					}
				}
				v.add(key);
				key=-1;
			}
		}
		return v;
	}
	
	/**
	 * Generate a set of logs to be used for testing
	 * Each log has 
	 *    - a different time stamp.
	 *    - the message contains the key of the log
	 *    - the log type is random (all types but Trace because trace
	 *      has no body)
	 *
	 * @param numOfLogs The number of logs to put in the collection
	 * @return The collection with the logs
	 */
	public static Collection<ILogEntry> generateLogs(int numOfLogs) throws Exception {
		Random rnd = new Random(Calendar.getInstance().getTimeInMillis());
		long now = Calendar.getInstance().getTimeInMillis()-1000*60*60*24; // Yesterday
		SimpleDateFormat df = new IsoDateFormat();
		Vector<ILogEntry> v = new Vector<ILogEntry>(numOfLogs);
		for (int t=0; t<numOfLogs; t++) {
			Date dt = new Date(now+t*1000);
			StringBuffer dateSB = new StringBuffer();
			FieldPosition pos = new FieldPosition(0);
			df.format(dt,dateSB,pos);

			StringBuilder logStr = new StringBuilder("<");
			int typePos = rnd.nextInt(LogTypeHelper.values().length);
			LogTypeHelper type = LogTypeHelper.values()[typePos];
			
			if (type==LogTypeHelper.TRACE) {
				type=LogTypeHelper.INFO;
			}
			logStr.append(type.logEntryType);
			logStr.append(logHeaderStr);
			logStr.append(dateSB.toString());
			logStr.append(logBodyStr);
			logStr.append(t);
			logStr.append(logFooterStr);
			logStr.append(type.logEntryType);
			logStr.append('>');
			if (parser==null) {
				parser = ACSLogParserFactory.getParser();
			}
			ILogEntry log = parser.parse(logStr.toString());
			v.add(log);
		}
		return v;
	}
	
	
	
	/**
	 * Generate a set of logs of a given type
	 * Each log has 
	 *    - a different time stamp.
	 *    - the message contains the key of the log
	 *
	 * @param numOfLogs The number of logs to put in the collection
	 * @return The collection with the logs
	 */
	
	public static Collection<ILogEntry> generateLogsType(int numOfLogs, LogTypeHelper logType) throws Exception {
		Random rnd = new Random(Calendar.getInstance().getTimeInMillis());
		long now = Calendar.getInstance().getTimeInMillis()-1000*60*60*24; // Yesterday
		SimpleDateFormat df = new IsoDateFormat();
		Vector<ILogEntry> v = new Vector<ILogEntry>(numOfLogs);
		
		for (int t=0; t<numOfLogs; t++) {
			Date dt = new Date(now+t*1000);
			StringBuffer dateSB = new StringBuffer();
			FieldPosition pos = new FieldPosition(0);
			df.format(dt,dateSB,pos);

			StringBuilder logStr = new StringBuilder("<");
			
			if (logType==LogTypeHelper.TRACE) {
				logType=LogTypeHelper.INFO;
			}
			logStr.append(logType.logEntryType);
			logStr.append(logHeaderStr);
			logStr.append(dateSB.toString());
			logStr.append(logBodyStr);
			logStr.append(t);
			logStr.append(logFooterStr);
			logStr.append(logType.logEntryType);
			logStr.append('>');
			if (parser==null) {
				parser = ACSLogParserFactory.getParser();
			}
			ILogEntry log = parser.parse(logStr.toString());
			v.add(log);
		}
		return v;
	}
		
	
	
	/**
	 * Add numOfLogs log to the cache.
	 * 
	 * @param cache The cache to fill with logs
	 * @param numOfLogs The number of logs to insert in cache
	 */
	public static void populateCache(ILogMap cache, int numOfLogs) throws Exception {
		if (cache ==null) {
			throw new IllegalArgumentException("The cache is null");
		}
		if (numOfLogs<=0) {
			throw new IllegalArgumentException("Impossible to add "+numOfLogs+" logs");
		}
		Collection<ILogEntry> logs = generateLogs(numOfLogs);
		for (ILogEntry log: logs) {
			cache.add(log);
		}
	}
	
	/**
	 * Create a cache of the given type.
	 * The cache is created with the empty constructor if the opt
	 * param is null.
	 * If opt is not null the cache is created passing opt in the
	 * constructor.
	 * LogFileCache has only the empty constructor: for this class
	 * opt is ignored.
	 * 
	 * @param type The type of the cache
	 * @param opt The optional integer argument (if it is null,
	 *            the cache is created with the empty constructor)
	 * @return The cache implementing ILogMap
	 */
	public static ILogMap getCache(int type, Integer opt) throws Exception {
		switch (type) {
		case LOGFILECACHE_TYPE:
			return new LogFileCache();
		case LOGBUFFEREDFILECACHE_TYPE:
			if (opt==null) {
				return new LogBufferedFileCache();
			} else {
				return new LogBufferedFileCache(opt);
			}
		case LOGCACHE_TYPE:
			if (opt==null) {
				return new LogCache();
			} else {
				return new LogCache(opt);
			}
		default:
			throw new IllegalArgumentException("Invalid cache type "+type);
		}
	}
}
