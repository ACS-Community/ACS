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
package alma.archive.identifier;

import java.net.URI;
import java.net.URISyntaxException;

import alma.archive.exceptions.UIDLibraryException;
import alma.archive.range.IdentifierRange;
import alma.archive.range.RangeT;
import alma.entities.commonentity.EntityRefT;
import alma.entities.commonentity.EntityT;

/**
 * @author simon
 */
public class Range
{
	private String archiveid;
	private boolean serialized;
	
	private String rangeid;
	private long documentid = 0;
	
	private long minDocumentid = 0;
	private long maxDocumentid = Long.MAX_VALUE;
	
	/**
	 * 
	 */
	public Range(IdentifierRange identifierRange)
		throws
			UIDLibraryException
	{
		archiveid = identifierRange.getArchiveID();
		serialized = identifierRange.getSerialised();
		
		RangeT r = identifierRange.getRange();
		
		rangeid = r.getRangeID();
		
		String baseid = r.getBaseDocumentID();
		minDocumentid = Long.parseLong(baseid,16);
		documentid = minDocumentid;
		
		String maxid = r.getMaxDocumentID();
		if (maxid != null) maxDocumentid = Long.parseLong(maxid,16);
	}
	
	public void assignUniqueEntityId(EntityT entity)
		throws
			UIDLibraryException
	{
		if (!serialized)
		{
			if (documentid > maxDocumentid) throw new UIDLibraryException(
				"Maximum document exceeded"); 
			String uid = getNextID(); 
			entity.setEntityId(uid);
			entity.setEntityIdEncrypted("-- id encryption not yet implemented --");
		}
		else throw new UIDLibraryException(
			"Cannot assign Entity ID's with a serialized range");
	}
	
	public void assignUniqueEntityRef(EntityRefT ref)
		throws
			UIDLibraryException
	{
		if (serialized)
		{
			if (documentid > maxDocumentid) throw new UIDLibraryException(
				"Maximum document exceeded");
			String uid = getNextID();
			ref.setEntityId(uid);
		}
		else throw new UIDLibraryException(
			"Cannot assign Refernce ID's with a non-serialized range");
	}
	
	private String getNextID()
		throws
			UIDLibraryException
	{
		if (documentid > maxDocumentid) 
			throw new UIDLibraryException("Range Exceeded");
		
		String uid = "uid://X" + archiveid + 
					 	  "/X" + rangeid + 
						  "/X" + Long.toHexString(documentid);
		documentid++;
		return uid;
	}
	
	//TODO: this needs to be moved to protected soon, will require the
	//ArchiveID web service to be moved to this namespace
	public URI next() throws UIDLibraryException
	{
		try{
			URI uri = new URI(getNextID());
			return uri;
		}
		catch (URISyntaxException e){
			throw new UIDLibraryException(e.getMessage());
		}
	}
	
	public URI rangeId() 
		throws 
			UIDLibraryException
	{
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
			throw new UIDLibraryException();
		}
	}
}
