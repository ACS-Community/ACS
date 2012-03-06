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

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import org.apache.commons.collections.list.CursorableLinkedList;

import alma.MonitorArchiver.CollectorListStatus;
import alma.acs.monitoring.DAO.ComponentData;

/**
 * This class encapsulates a <code>{@link CursorableLinkedList}&lt;{@link CollectorData}&gt;</code>,
 * which holds all monitor collector references together with their data that got harvested by a blobber.
 * It allows calls to {@link #add(CollectorData)} and {@link #remove(CollectorData)} while iterating
 * over the list of CollectorData objects using methods {@link #next()} or {@link #hasNext()}.
 * <p>
 * The only added value compared to using the <code>CursorableLinkedList</code> directly is 
 * the synchronization of concurrent calls and the type casts (CursorableLinkedList does not support generics).
 */
public class CollectorList {

    /**
     * CursorableLinkedList allows concurrent modifications and iteration, 
     * so that we can use a simple iterator instead of taking care of list indices ourselves.
     */
    private final CursorableLinkedList myList = new CursorableLinkedList();
    
	/**
	 * Smart list iterator, that takes account of list insertions and removals
	 * in between the calls to next(), as long as the list methods
	 * are not called concurrently.
	 * @TOOD Use generics once the apache commons lists support them.
	 */
	private ListIterator myListIterator;
    
    
	public CollectorList() {
		resetIterator();
    }

    /**
     * Creates and adds a CollectorData object for the given collector ID.
     * @see #add(CollectorData)
     */
    public CollectorListStatus add(String inCollectorName) {
        return add(new CollectorData(inCollectorName));
    }

    /**
     * @param inData  The object that identifies a collector and holds its data.
     * @return ADDED if the CollectorData object was added to this list, or KNOWN if it was already in the list.
     */
    public CollectorListStatus add(CollectorData inData) {
        CollectorListStatus outValue = CollectorListStatus.KNOWN;
        synchronized (myList) {
	        if (!this.myList.contains(inData)) {
	            this.myList.add(inData);
	            outValue = CollectorListStatus.ADDED;
	        }
        }
        return outValue;
    }

    
    /**
     * @param inCollectorName The ID of the collector for which we check the list.
     * @return KNOWN if the given collector is already in this list, or UNKNOWN otherwise.
     */
    public CollectorListStatus contains(String inCollectorName) {
        return contains(new CollectorData(inCollectorName));
    }

    /**
     * This method is currently used only from {@link #contains(String)}.
     * Make it public if it should be used from outside.
     */
    protected CollectorListStatus contains(CollectorData inData) {
        CollectorListStatus outValue = CollectorListStatus.UNKNOWN;
        synchronized (myList) {
	        if (this.myList.contains(inData)) {
	            outValue = CollectorListStatus.KNOWN;
	        }
        }
        return outValue;
    }

    /**
     * 
     * @param inCollectorName
     * @return REMOVED if the given collector was in this list and got removed, or UNKNOWN otherwise.
     */
    public CollectorListStatus remove(String inCollectorName) {
        return remove(new CollectorData(inCollectorName));
    }

    /**
     * This method is currently used only from {@link #remove(String)}.
     * Make it public if it should be used from outside.
     */
    protected synchronized CollectorListStatus remove(CollectorData inData) {
    	boolean removedIt;
        synchronized (myList) {
        	removedIt = myList.remove(inData);
        }
        return ( removedIt ? CollectorListStatus.REMOVED : CollectorListStatus.UNKNOWN );
    }

    
    /**
     * @return The number of monitor collectors in this list.
     */
    public int size() {
        synchronized (myList) {
        	return this.myList.size();
        }
    }

    
    /**
     * @return The next CollectorData from the list.
     * @throws NoSuchElementException if we are at the end of the list. 
     *         Should have checked with {@link #hasNext()}, and called {@link #resetIterator()}.
     */
    public CollectorData next() {
        synchronized (myList) {
	    	return (CollectorData) myListIterator.next();
        }
    }


    /**
     * @return <code>true</code> if a subsequent call to {@link #next()}
     *         will return another CollectorData object from the current iteration;
     *         <code>false</code> otherwise.
     */
    public boolean hasNext() {
        synchronized (myList) {
        	return myListIterator.hasNext();
        }
    }        

    public void resetIterator() {
        synchronized (myList) {
        	myListIterator = myList.listIterator();
        }
    }
    
    /**
     * This class associates a Collector-ID with data:
     * <ul>
     *   <li>The {@link BlobData} for all properties watched by the collector.
     *   <li>The last successful update time.
     * </ul>.
     * The collector-ID is the name of the collector component deployed in the container
     * from whose components we want to collect monitoring data.
     */
    protected static class CollectorData {

        private final String collectorId;

        public CollectorData(String inCollectorId) {
            if (inCollectorId == null) {
                throw new IllegalArgumentException("inCollectorId cannot be null.");
            }
            this.collectorId = inCollectorId;
        }

        /**
         * @return The collector ID for a given container, which is the name of the collector component deployed in that container.
         */
        public String getCollectorId() {
        	return this.collectorId;
        }
        

		@Override
		public boolean equals(Object inObject) {
			if (this == inObject)
				return true;
			if (inObject == null)
				return false;
			boolean outResult = false;
			try {
				CollectorData data = (CollectorData) inObject;
				if (collectorId.equals(data.collectorId)) {
					outResult = true;
				}
			} catch (Exception e) {
			}
			return outResult;
		}

		/**
		 * Must be consistent with equals(), based only on collectorId.
		 */
		@Override
		public int hashCode() {
			return collectorId.hashCode();
		}

		/**
		 * Timestamp of the last successful data retrieval from this monitor collector.
		 * <p>
		 * Currently no-op, but could be useful to store this data in the future.
		 */
		void setLastSuccessfulAccessTime(long currentTimeMillis) {
		}

	}

    protected static class BlobData extends ComponentData {
        public List<Object> dataList = new ArrayList<Object>();

        public void reset() {
            super.reset();
            dataList.clear();
        }
    }

}
