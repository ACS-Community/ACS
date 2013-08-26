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
package alma.acs.logging;

import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;

import alma.acs.logging.RepeatGuard.Logic;

/**
 * This class can be used as a convenient map that keeps {@link RepeatGuard} objects associated with <code>String IDs</code>.
 * For example, if your application repeatedly sends an event related to the unavailability of some component,
 * and you want to limit these events on a per-component basis, then you either need to keep and use a separate RepeatGuard object for every component instance,
 * or alternatively you can use one instance of <code>MultipleRepeatGuard</code> and take the component names as <code>ID</code>s.
 * <p>
 * The additional benefit is that this class optionally offers a bounded cache for the various RepeatGuards, which limits memory consumption for cases
 * in which the number of possible IDs is large, e.g. if the ID is the log message coming from unknown / 3rd party source. 
 * The drawback is that the least accessed RepeatGuards will be removed internally when the cache is full and will be re-created
 * upon access, which may distort the original guarding behavior (e.g. because the first check() of the re-created RepeatGuard always returns true,
 * even if the old RepeatGuard object would have returned false for another 100 invocations, or because {@linkplain RepeatGuard#counterAtLastExecution()}
 * will not be accurate.
 * 
 * @author hsommer
 * @since ACS 8.0.0 
 */
public class MultipleRepeatGuard
{
	private final long defaultInterval;
	private final TimeUnit defaultTimeUnit;
	private final int defaultMaxRepetitions;
	private final Logic defaultLogic;
	
	private final BoundedHashMap<String, RepeatGuard> guards;
	
	/////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////// Repeat Guard functionality /////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Constructor for a <code>MultipleRepeatGuard</code> with a bounded RepeatGuard cache.
	 * <p>
	 * The first 4 parameters are the same as in {@link RepeatGuard#RepeatGuard(long, TimeUnit, int, Logic)}
	 * and are used as defaults for all RepeatGuards created by this class.
	 * To change these values for particular RepeatGuards, use {@link #setRepeatGuard(String, RepeatGuard)} 
	 * using a new RepeatGuard object or the one obtained beforehand from {@link #getRepeatGuard(String)}.
	 * 
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of repetitions.
	 * @param logic Evaluation logic for <code>interval</code> and <code>maxRepetitions</code>. 
	 *        The logic will be "reduced" automatically if <code>interval</code> or <code>maxRepetitions</code> 
	 *        have a value <= 0, so as to be based only on the other positive value.
	 * @param maxCacheSize
	 *        Number of <code>ID</code>s (as used in {@link #check(String)}) whose repetition we'll keep track of.
	 *        Setting this value higher will use more memory;
	 *        setting it lower than the actual number of IDs will result in unnecessary execution of the guarded actions,
	 *        as the matching <code>RepeatGuard</code>s will be lost and re-created.
	 * @throws IllegalArgumentException if maxRepetitions <= 0 && interval <= 0
	 */
	public MultipleRepeatGuard(long interval, TimeUnit timeUnit, int maxRepetitions, Logic logic, int maxCacheSize) {
		this.defaultInterval = interval;
		this.defaultTimeUnit = timeUnit;
		this.defaultMaxRepetitions = maxRepetitions;
		this.defaultLogic = logic;
		
		guards = new BoundedHashMap<String, RepeatGuard>(maxCacheSize, true);
	}
	
	/**
	 * Constructor for a <code>MultipleRepeatGuard</code> without a bounded RepeatGuard cache 
	 * (which means that memory limits indirectly set the bound, with possible side effects).
	 * @see #MultipleRepeatGuard(long, TimeUnit, int, Logic, int) 
	 */
	public MultipleRepeatGuard(long interval, TimeUnit timeUnit, int maxRepetitions, Logic logic) {
		this(interval, timeUnit, maxRepetitions, logic, Integer.MAX_VALUE);
	}
	
	/**
	 * @see RepeatGuard#check()
	 */
	public synchronized boolean check(String ID) {
		RepeatGuard guard = getOrCreateRepeatGuard(ID);
		return guard.check();
	}

	/**
	 * @see RepeatGuard#checkAndIncrement()
	 */
	public synchronized boolean checkAndIncrement(String ID) {
		RepeatGuard guard = getOrCreateRepeatGuard(ID);
		return guard.checkAndIncrement();
	}
	
	/**
	 * @see RepeatGuard#increment()
	 */
	public synchronized void increment(String ID) {
		RepeatGuard guard = getOrCreateRepeatGuard(ID);
		guard.increment();
	}
	
	/**
	 * @see RepeatGuard#counter()
	 */
	public synchronized int counter(String ID) {
		RepeatGuard guard = getOrCreateRepeatGuard(ID);
		return guard.counter();
	}

	/**
	 * Note that depending on the RepeatGuard cache limit, the RepeatGuard for the given ID 
	 * may get re-created during this call, and thus the counter may be 0
	 * even though the old "true" counter was > 0.
	 * 
	 * @see RepeatGuard#counterAtLastExecution()
	 */
	public synchronized int counterAtLastExecution(String ID) {
		RepeatGuard guard = getOrCreateRepeatGuard(ID);
		return guard.counterAtLastExecution();
	}
	
	/////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////// Map administration ///////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////

	public synchronized boolean hasRepeatGuard(String ID) {
		return guards.containsKey(ID);
	}
	
	public synchronized RepeatGuard getRepeatGuard(String ID) {
		return getOrCreateRepeatGuard(ID);
	}
	
	/**
	 * Sets a possibly modified <code>RepeatGuard</code> for the given <code>ID</code>, 
	 * which allows for some <code>RepeatGuard</code>s having different settings than those passed in 
	 * {@link #MultipleRepeatGuard(long, TimeUnit, int, Logic, int)}. 
	 * @param ID
	 * @param guard
	 */
	public synchronized void setRepeatGuard(String ID, RepeatGuard guard) {
		guards.put(ID, guard);
	}
	
	/**
	 * Gets the actual number of RepeatGuards in the internal cache,
	 * regardless of whether there is an upper limit and whether it has been reached etc.
	 */
	public int getCacheSize() {
		return guards.size();
	}
	
	public synchronized void clearCache() {
		guards.clear();
	}
	
	public void setCacheLimit(int maxEntries) {
		guards.setMaxEntries(maxEntries);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////
	/////////////////////// Auxiliary methods and inner classes /////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////

	private synchronized RepeatGuard getOrCreateRepeatGuard(String ID) {
		RepeatGuard guard = guards.get(ID);
		if (guard == null) {
			guard = new RepeatGuard(defaultInterval, defaultTimeUnit, defaultMaxRepetitions, defaultLogic);
			guards.put(ID, guard);
		}
		return guard;
	}

	/**
	 * @TODO This class could be moved up to module jacsutil later.
	 */
	private static class BoundedHashMap<K,V> extends LinkedHashMap<K,V> {
		private volatile int maxEntries;

		/**
		 * Constructor.
		 * The initial capacity of this map is determined as <code>Math.min(16, maxEntries)</code>.
		 * The load factor is taken as 0.75 which is the default for other map types.
		 * <p>
		 * @param maxEntries the maximum capacity of this map. 
		 * @param accessOrder
		 *            if true, then this map is an LRU (least-recently used) cache that will remove the least recently used entries 
		 *            when the number of stored entries has reached <code>maxEntries</code> and a new entry is stored; 
		 *            otherwise insertion order is used (removing the entry that was inserted first).
		 *            See {@link LinkedHashMap#LinkedHashMap(int, float, boolean)}.
		 */
		BoundedHashMap(int maxEntries, boolean accessOrder) {
			super(Math.min(16, maxEntries), 0.75f, accessOrder);
			setMaxEntries(maxEntries);
		}
		
		@Override
		protected boolean removeEldestEntry(Entry eldest) {
			return size() > maxEntries;
		}
		
		/**
		 * Sets a new value for the maximum number of entries in this map,
		 * overriding the <code>maxEntries</code> value given in the constructor
		 * or in previous invocations of this method.
		 * @param maxEntries must be a positive number
		 */
		void setMaxEntries(int maxEntries) {
			if (maxEntries <= 0) {
				throw new IllegalArgumentException("maxEntries must be >= 1");
			}
			this.maxEntries = maxEntries;
		}
	}
}
