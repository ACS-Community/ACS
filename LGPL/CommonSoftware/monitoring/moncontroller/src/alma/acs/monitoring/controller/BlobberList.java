package alma.acs.monitoring.controller;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.MonitorArchiver.BlobberOperations;

/**
 * Ordered list of {@link BlobberInfo} objects. 
 * Duplicate blobber names are not allowed.
 * Access is thread-safe.
 */
class BlobberList {
	
	/**
	 * Blobber name, ref and some meta data.
	 */
	class BlobberInfo {
		
		/**
		 * Name of the blobber component.
		 */
		final String blobberName;
		
		/**
		 * Reference to the blobber component. 
		 * May be null if the blobber component has not been requested yet, 
		 * or if it is not even available in the system (see http://jira.alma.cl/browse/COMP-6572)
		 */
		BlobberOperations blobberRef;
		
		/**
		 * Avoids requesting a blobber reference too soon after a failure,
		 * see http://jira.alma.cl/browse/COMP-6572
		 */
		long lastRefRequestFailedTimeMillis;
		
		/**
		 * Number of calls to the blobber that failed, for example by throwing runtime exceptions.
		 * <p>
		 * @TODO: This information could be used to stop using that blobber, after all collectors are deregistered and re-assigned
		 * to other blobbers.
		 */
		int numCallsFailed;
		
		/**
		 * Private c'tor, to enforce construction via {@link BlobberList#createBlobberInfo(String)}.
		 * @param blobberName
		 */
		private BlobberInfo(String blobberName) {
			this.blobberName = blobberName;
		}
		
		/**
		 * @return True if the last failed request for the blobber reference was less than 30 seconds ago. 
		 */
		boolean hadRefRequestFailureWithinLastSec(int seconds) {
			return ( System.currentTimeMillis() - lastRefRequestFailedTimeMillis < seconds * 1000 );
		}
	}
	
	/**
	 * We back this list with an insertion-ordered map, to find BlobberInfos fast by name.
	 * @TODO: Use a PriorityQueue with custom sorting, so that the first blobber found has the
	 *        fewest collectors assigned. Then we don't need to keep an index, but just take the first blobber,
	 *        possibly skipping problematic blobbers. This will nicely implement load balancing based on number
	 *        of assigned collectors.
	 */
	protected final LinkedHashMap<String, BlobberInfo> blobberMap = new LinkedHashMap<String, BlobberInfo>();

	/**
	 * Zero-based index, used by {@link #getNextBlobberInfo()} to iterate over the list.
	 */
	protected int lastBlobberPosition;
	
	/**
	 * Factory method for BlobberInfo objects. They automatically get stored in the list. 
	 * @param blobberName
	 * @throws AcsJIllegalArgumentEx If the blobber name already exists in the list.
	 */
	void createBlobberInfo(String blobberName) throws AcsJIllegalArgumentEx {
		synchronized (blobberMap) {
			if (blobberName == null || blobberMap.containsKey(blobberName)) {
				throw new AcsJIllegalArgumentEx();
			}
			blobberMap.put(blobberName, new BlobberInfo(blobberName));
		}
	}
	
	BlobberInfo getBlobberInfo(String blobberName) {
		synchronized (blobberMap) {
			return blobberMap.get(blobberName);
		}
	}
	
	/**
	 * @return All "live" BlobberInfo objects from the internal list. The returned list itself is a copy and safe to iterate over.
	 */
	List<BlobberInfo> getBlobberInfos() {
		synchronized (blobberMap) {
			return new ArrayList<BlobberInfo>(blobberMap.values());
		}
	}
	
	/**
	 * Returns all blobber names, or only those whose reference we hold.
	 * @param referencedOnly If true, then consider only blobbers that have been activated 
	 *                       and that are referenced by this controller.
	 * @return List of blobber names.
	 */
	List<String> getBlobberNames(boolean referencedOnly) {
		List<String> ret = new ArrayList<String>();
		
		synchronized (blobberMap) {
			for (BlobberInfo blobberInfo : blobberMap.values()) {
				if (!referencedOnly || blobberInfo.blobberRef != null) {
					ret.add(blobberInfo.blobberName);
				}
			}
		}
		
		return ret;
	}
	
	/**
	 * Number of BlobberInfos contained in this list.
	 */
	int size() {
		synchronized (blobberMap) {
			return blobberMap.size();
		}
	}
	
	/**
	 * Circularly iterates over the BlobberInfos. 
	 * Returns <code>null</code> if no BlobberInfos are available.
	 * <p>
	 * If blobbers get added or removed in between calls to this method, there is a simple and possibly "unfair"
	 * algorithm that will find a valid blobber, but it might even be the same blobber as last time,
	 * or a blobber may be skipped.
	 */
	BlobberInfo getNextBlobberInfo() {
		BlobberInfo ret = null;
		synchronized (blobberMap) {
			int maxPos = blobberMap.size() - 1;
			if (maxPos >= 0) {
				lastBlobberPosition++; // this becomes the next blobber position
				if (lastBlobberPosition > maxPos) {
					lastBlobberPosition = 0;
				}
				// iterate over the ordered map values to lastBlobberPosition
				Iterator<BlobberInfo> iterator = blobberMap.values().iterator();
				for (int i = 0; i <= lastBlobberPosition; i++) {
					ret = iterator.next();
				}
			}
			else {
				// no BlobberInfo available, just return null
			}
		}
		return ret;
	}
}
