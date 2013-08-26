/*
 * 	  Created on 13-Feb-2006
 * 
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.container.archive;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.concurrent.atomic.AtomicLong;

import alma.ArchiveIdentifierError.wrappers.AcsJIdentifierUnexpectedEx;
import alma.ArchiveIdentifierError.wrappers.AcsJRangeExhaustedEx;
import alma.ArchiveIdentifierError.wrappers.AcsJRangeLockedEx;
import alma.ArchiveIdentifierError.wrappers.AcsJRangeUnlockedEx;
import alma.ArchiveIdentifierError.wrappers.AcsJUidAlreadyExistsEx;
import alma.archive.range.IdentifierRange;
import alma.archive.range.RangeT;
import alma.entities.commonentity.EntityRefT;
import alma.entities.commonentity.EntityT;

/**
 * This class represents a range of UIDs as created by the Alma archive, 
 * and allows using them as XML document IDs or links to XML documents. 
 * A range is always limited to a finite number of UIDs, given either by an explicitly requested limit, 
 * or otherwise by <code>Long.MAX_VALUE</code> many UIDs.
 * <p>
 * This class is implemented as an intelligent wrapper for the castor class {@link IdentifierRange}
 * which itself gets generated from the schema "IdentifierRange.xsd".
 * <p>
 * For a better description, see the <a href="http://almasw.hq.eso.org/almasw/bin/view/Archive/UidLibrary">Archive/UidLibrary wiki page</a>.
 * <p>
 * General remark on the length of UIDs: there is no limit on the length of the three "/x012af" parts of a UID,
 * as was confirmed by awicenec to hsommer on 2007-04-20. It is therefore the Archive's responsibility to
 * ensure that all code dealing with UIDs (which should all be under archive responsibility anyway) 
 * be updated accordingly whenever the actually generated UIDs exceed the "Long" limit.
 *    
 * @author simon, hsommer
 */
public class Range
{
	private final String archiveid;
	private final boolean isLocked;
	
	private final String rangeid;
	private final AtomicLong documentid;
	
	private final long minDocumentid;
	private final long maxDocumentid;
	
	/**
	 * 
	 */
	public Range(IdentifierRange identifierRange)
	{
		archiveid = identifierRange.getArchiveID();
		isLocked = identifierRange.getIsLocked();
		
		RangeT r = identifierRange.getRange();
		
		rangeid = r.getRangeID();
		
		String baseid = r.getBaseDocumentID();
		minDocumentid = Long.parseLong(baseid,16);
		documentid = new AtomicLong(minDocumentid);
		
		String maxid = r.getMaxDocumentID();
		maxDocumentid = ( maxid != null ? Long.parseLong(maxid,16) : Long.MAX_VALUE ); 
	}

	/**
	 * 
	 * @return number of UIDs in the range (no matter how many UIDs have been used so far)
	 */
	public long getLength() {
		return maxDocumentid-minDocumentid+1;
	}
	
	/**
	 * A range can be either locked or unlocked. 
	 * This immutable property was originally called "serialised" but was considered too confusing 
	 * since this logical concept has nothing to do with technically serializing the castor class to XML.
	 * <p>
	 * TODO: explain better the real meaning of this locking, and the forseen scenarios. 
	 * @return true if this object represents a locked range.
	 */
	public boolean isLocked() {
		return isLocked;
	}
	
	/**
	 * Assigns a UID to the <code>EntityT</code> castor object that should be a direct child of an Alma XML entity.
	 * As a means of protection, this call will fail if the entity already has a UID. If you actually need to
	 * replace a UID in some rare cases, please use {@link #replaceUniqueEntityId(EntityT)}. 
	 * <p>
	 * The same functionality is offered in {@link ContainerServices#assignUniqueEntityId(EntityT)} which actually delegates to here.
	 */
	public void assignUniqueEntityId(EntityT entity) throws AcsJUidAlreadyExistsEx, AcsJRangeLockedEx, AcsJRangeExhaustedEx {
		setUniqueEntityId(entity, false);
	}

	/**
	 * Assigns a UID to the <code>EntityT</code> castor object that should be a direct child of an Alma XML entity.
	 * Unlike {@link #assignUniqueEntityID}, this method will silently replace any existing UID,
	 * which is possibly dangerous. Therefore it should only be used in rare cases where replacing an existing ID is 
	 * needed, for example when the ObsPrep tool might translate locally created documents into an archivable format. 
	 */
	public void replaceUniqueEntityId(EntityT entity) throws AcsJUidAlreadyExistsEx, AcsJRangeLockedEx, AcsJRangeExhaustedEx {
		setUniqueEntityId(entity, true);
	}

	
	private void setUniqueEntityId(EntityT entity, boolean allowReplacing) throws AcsJUidAlreadyExistsEx, AcsJRangeLockedEx, AcsJRangeExhaustedEx {
		if (entity == null) {
			throw new NullPointerException("argument 'entity' must not be null.");
		}
		if (!isLocked)
		{
			if (entity.getEntityId() != null && entity.getEntityId().length() > 0 && !allowReplacing) {
				AcsJUidAlreadyExistsEx ex = new AcsJUidAlreadyExistsEx();
				ex.setObjectDesc("Entity " + entity.getEntityTypeName());
				ex.setUid(entity.getEntityId());
				throw ex;
			}
			String uid = getNextID();
			entity.setEntityId(uid);
			entity.setEntityIdEncrypted("-- id encryption not yet implemented --");
		}
		else {
			AcsJRangeLockedEx ex = new AcsJRangeLockedEx();
			ex.setRange(rangeIdString());
			throw ex;
		}
	}

	
	
	/**
	 * Assigns a UID from the range to a given entity <b>reference</b>.
	 * Note that this is different from assigning a UID to an entity.
	 * This method only works if this range is locked, see {@link #isLocked()}.
	 * @param ref
	 */
	public void assignUniqueEntityRef(EntityRefT ref) throws AcsJRangeUnlockedEx, AcsJRangeExhaustedEx {
		if (isLocked)
		{
			String uid = getNextID();
			ref.setEntityId(uid);
		}
		else {
			AcsJRangeUnlockedEx ex = new AcsJRangeUnlockedEx();
			ex.setRange(rangeIdString());
			throw ex;
		}			
	}
	
	/**
	 * Returns a UID that is constructed from the archiveID, the rangeID, and the running local document ID.
	 */
	private String getNextID() throws AcsJRangeExhaustedEx {
		long nextID = documentid.getAndIncrement();
		if (nextID <= maxDocumentid) {
			return generateUID(archiveid, rangeid, nextID);
		} 
		else {
			documentid.decrementAndGet(); // to avoid a LONG overflow in the zillions of next calls
			AcsJRangeExhaustedEx ex = new AcsJRangeExhaustedEx();
			ex.setRange(rangeIdString());
			ex.setRangeMaxDocumentId(""+getMaxDocumentId());
			throw ex;
		}
	}

	/**
	 * This method is only exposed for testing purposes, when a UID has to be generated from its constituent parts.
	 * You should normally not use this method.  
	 */
	public final static String generateUID(String _archiveID, String _rangeID, long _localId) {
		String uid = "uid://" + _archiveID + 
		  "/X" + _rangeID + 
		  "/X" + Long.toHexString(_localId);
		return uid;
	}

	/**
	 * Checks whether another UID can be pulled from this range without causing an exception due to an overflow.
	 * <p>
	 * Warning about thread safety: This method does not reserve a UID or anything like that, 
	 * thus a returned <code>true</code> value does not necessarily mean that a later call 
	 * to one of the assignXYZ etc methods will succeed. 
	 * @return true if at least one more UID can be retrieved from this range.
	 */
	public boolean hasNextID() {
		return ( documentid.get() < maxDocumentid );
	}
	
	long getMaxDocumentId() {
		return maxDocumentid;
	}
	
	/**
	 * cryptic comment from Simon: TODO: this needs to be moved to protected soon, will require the ArchiveID web service to be moved to this namespace
	 * @return
	 * @throws AcsJIdentifierUnexpectedEx
	 */
	public URI next() throws AcsJIdentifierUnexpectedEx, AcsJRangeExhaustedEx
	{
		try{
			URI uri = new URI(getNextID());
			return uri;
		}
		catch (URISyntaxException e){
			throw new AcsJIdentifierUnexpectedEx(e);
		}
	}
	
	/**
	 * Gets the UID of this range document itself.
	 * @throws AcsJIdentifierUnexpectedEx  if the string from {@link #rangeIdString()} cannot be turned into a URI 
	 */
	public URI rangeId() throws AcsJIdentifierUnexpectedEx 	{
		String uid = rangeIdString();
		try {
			URI uri = new URI(uid);
			return uri;
		}
		catch (URISyntaxException e) {
			throw new AcsJIdentifierUnexpectedEx(e);
		}
	}
	
	/**
	 * Gets a String representation of the UID of this range document itself.
	 */
	public final String rangeIdString() {
		String uid = "uid://" + archiveid + 
	 	  "/X" + rangeid + 
		  "/X0";
		return uid;
	}
	
}
