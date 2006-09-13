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

import alma.acs.container.ContainerException;
import alma.archive.range.IdentifierRange;
import alma.archive.range.RangeT;
import alma.entities.commonentity.EntityRefT;
import alma.entities.commonentity.EntityT;

/**
 * This class is an intelligent wrapper for the castor class {@link IdentifierRange}
 * which itself gets generated from the schema "IdentifierRange.xsd".
 * 
 * See <a href="http://almasw.hq.eso.org/almasw/bin/view/Archive/UidLibrary">wiki</a>.
 * 
 * @author simon
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
		isLocked = identifierRange.getSerialised();
		
		RangeT r = identifierRange.getRange();
		
		rangeid = r.getRangeID();
		
		String baseid = r.getBaseDocumentID();
		minDocumentid = Long.parseLong(baseid,16);
		documentid = new AtomicLong(minDocumentid);
		
		String maxid = r.getMaxDocumentID();
		maxDocumentid = ( maxid != null ? Long.parseLong(maxid,16) : Long.MAX_VALUE ); 
	}

	public boolean isLocked() {
		return isLocked;
	}
	
	/**
	 * @see {@link ContainerServices#assignUniqueEntityId(EntityT)}
	 */
	public void assignUniqueEntityId(EntityT entity) throws ContainerException {
		if (!isLocked)
		{
			String uid = getNextID(); 
			entity.setEntityId(uid);
			entity.setEntityIdEncrypted("-- id encryption not yet implemented --");
		}
		else {
			throw new ContainerException("Cannot assign Entity IDs from a locked range");
		}
	}

	/**
	 * Assigns a UID from the range to a given entity <b>reference</b>.
	 * Note that this is different from assigning a UID to an entity.
	 * This method only works if this range is locked, see {@link #isLocked()}.
	 * @param ref
	 * @throws ContainerException
	 */
	public void assignUniqueEntityRef(EntityRefT ref) throws ContainerException {
		if (isLocked)
		{
			String uid = getNextID();
			ref.setEntityId(uid);
		}
		else {
			throw new ContainerException("Cannot assign Reference IDs with an unlocked (\"non-serialized\") range.");
		}			
	}
	
	/**
	 * Returns a UID that is constructed from the archiveID, the rangeID, and the running local document ID.
	 * @throws ContainerException
	 */
	private String getNextID() throws ContainerException {
		long nextID = documentid.getAndIncrement();
		if (nextID <= maxDocumentid) {
			String uid = "uid://X" + archiveid + 
		 	  "/X" + rangeid + 
			  "/X" + Long.toHexString(nextID);
			return uid;
		} 
		else {
			documentid.decrementAndGet(); // to avoid a LONG overflow in the zillions of next calls
			throw new ContainerException("UID range maximum is reached, no more UIDs available."); // TODO ACS exception with rangeID, UID, ...
		}
	}

	
	
	/**
	 * cryptic comment from Simon: TODO: this needs to be moved to protected soon, will require the ArchiveID web service to be moved to this namespace
	 * @return
	 * @throws ContainerException
	 */
	public URI next() throws ContainerException
	{
		try{
			URI uri = new URI(getNextID());
			return uri;
		}
		catch (URISyntaxException e){
			throw new ContainerException(e);
		}
	}
	
	/**
	 * Gets the UID of this range document itself.
	 * @throws ContainerException
	 */
	public URI rangeId() throws ContainerException 	{
		String uid = "uid://X" + archiveid + 
	 	  "/X" + rangeid + 
		  "/X0";
		try
		{
			URI uri = new URI(uid);
			return uri;
		}
		catch (URISyntaxException e)
		{
			throw new ContainerException();
		}
	}
}
